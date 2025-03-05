{-# LANGUAGE OverloadedStrings #-}
module CodeSketch.Json
  ( definitionToJSON
  , pathInfoToJSON
  , rootToJSON
  , rootToJSONString
  , rootToTextOutline
  ) where

import CodeSketch.Types
import Data.Aeson
import Data.Aeson.Encoding (encodingToLazyByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import Text.Printf (printf)
import Data.List (sortBy, nubBy)

-- ANSI color codes
type Color = String

reset :: Color
reset = "\ESC[0m"

bold :: Color
bold = "\ESC[1m"

dim :: Color
dim = "\ESC[2m"

red :: Color
red = "\ESC[31m"

green :: Color
green = "\ESC[32m"

yellow :: Color
yellow = "\ESC[33m"

blue :: Color
blue = "\ESC[34m"

magenta :: Color
magenta = "\ESC[35m"

cyan :: Color
cyan = "\ESC[36m"

colorize :: Color -> String -> String
colorize color text = color ++ text ++ reset

-- | Convert a definition to JSON
instance ToJSON Definition where
  toJSON def = object
    [ "iden" .= iden def
    , "type" .= defTypeToText (defType def)
    , "vis"  .= visibilityToText (visibility def)
    ]

-- | Convert a path_info to JSON
instance ToJSON PathInfo where
  toJSON pi = object
    [ "path" .= path pi
    , "defs" .= defs pi
    ]

-- | Helper to convert definition type to string
defTypeToText :: DefType -> String
defTypeToText dt = case dt of
  Module   -> "module"
  Struct   -> "struct"
  Enum     -> "enum"
  Function -> "fn"
  Trait    -> "trait"
  Impl     -> "impl"
  Other s  -> s

-- | Helper to convert visibility to string
visibilityToText :: Visibility -> String
visibilityToText v = case v of
  Public    -> "*"
  Protected -> "+"
  Private   -> "-"

-- | Convert a definition to JSON value
definitionToJSON :: Definition -> Value
definitionToJSON = toJSON

-- | Convert a path_info to JSON value
pathInfoToJSON :: PathInfo -> Value
pathInfoToJSON = toJSON

-- | Convert the root structure to JSON value
rootToJSON :: Root -> Value
rootToJSON = toJSON

-- | Convert the root structure to a JSON string
rootToJSONString :: Root -> String
rootToJSONString = BL.unpack . encode

-- | Format a definition without line numbers
formatDefinition :: Definition -> String
formatDefinition def = 
  let -- Special case for impl blocks
      formatImpl = case defType def of
        Impl -> case signature (defInfo def) of
                 -- Show "impl TraitName for TypeName" format
                 Just sig -> colorize (bold ++ blue) "impl " ++ colorize cyan sig
                 Nothing -> colorize (bold ++ blue) "impl " ++ colorize cyan (baseImplName (iden def))
        _ -> ""

      -- Regular formatting for non-impl items
      regularFormatting = 
        let -- Type description and color
            (typeDesc, typeColor) = case defType def of
              Module   -> ("module", cyan)
              Struct   -> ("struct", green)
              Enum     -> ("enum", green)
              Function -> ("fn", yellow)
              Trait    -> ("trait", magenta)
              -- Skip impl blocks, they're handled separately
              Impl     -> ("", reset)
              Other s  -> (s, reset)
            
            -- Visibility description and color (Rust-specific)
            (visDesc, visColor) = case visibility def of
              Public    -> ("pub", green)
              Protected -> ("protected", yellow)
              Private   -> ("", reset)  -- Hide "private" for Rust
            
            -- Name with appropriate styling
            coloredName = case defType def of
              Module -> colorize (bold ++ cyan) (iden def)
              Struct -> colorize (bold ++ green) (iden def)
              Enum   -> colorize (bold ++ green) (iden def)
              Trait  -> colorize (bold ++ magenta) (iden def)
              -- Skip impl blocks, they're handled separately
              Impl   -> ""
              _      -> colorize bold (iden def)
            
            -- Function signature
            sigText = case defType def of
              Function -> case signature (defInfo def) of
                           Just sig -> " " ++ colorize dim sig
                           Nothing -> ""
              _ -> ""

        in (if null visDesc then "" else colorize visColor visDesc ++ " ") ++ 
           (if null typeDesc then "" else colorize typeColor typeDesc ++ " ") ++ 
           coloredName ++ sigText
      
      -- Extract base name from impl identifier
      baseImplName name = case break (==':') name of
                           (base, _) -> base
                           
  -- Return impl special formatting or regular formatting
  in if defType def == Impl
       then formatImpl
       else regularFormatting

-- | Convert a path_info to a text outline with indentation
pathInfoToTextOutline :: PathInfo -> String
pathInfoToTextOutline pi =
  colorize (bold ++ blue) (path pi) ++ ":\n" ++ 
  definitionsToTextTree (defs pi) []

-- | Add line number to formatted definition
addLineNumber :: Definition -> String -> String
addLineNumber def formattedDef =
  let lineNumPrefix = case lineNum (defInfo def) of
                        Just num -> colorize dim (printf "%4d " num)
                        Nothing -> "     "
  in lineNumPrefix ++ formattedDef

-- | Build a hierarchical text tree of definitions
definitionsToTextTree :: [Definition] -> [String] -> String
definitionsToTextTree definitions parentPath =
  let 
    -- Top-level definitions (no parent or parent not in scope)
    isTopLevel def = case parent (defInfo def) of
                     Nothing -> True
                     Just p -> not (p `elem` (map iden definitions))
    
    -- Get top-level definitions and sort them by line number
    topDefs = sortByLineNumber (filter isTopLevel definitions)
    
    -- Process each top-level definition
    processTopDef def = 
      let indent = replicate (length parentPath * 2 + 2) ' '
          -- Get child definitions
          childrenNames = children (defInfo def)
          -- Also include definitions that have this def as parent
          childrenByParent = filter (\d -> case parent (defInfo d) of
                                          Just p -> p == iden def
                                          Nothing -> False
                                    ) definitions
          -- Combine both sources of children
          allChildrenNames = childrenNames ++ map iden childrenByParent
          childDefs = filter (\d -> iden d `elem` allChildrenNames) definitions
          -- Sort children by line number
          sortedChildDefs = sortByLineNumber childDefs
          
          -- Generate output with line number before indentation
          formattedDef = indent ++ formatDefinition def 
          topLine = addLineNumber def formattedDef ++ "\n"
          
          childLines = if null childDefs 
                        then "" 
                        else definitionsToTextTree sortedChildDefs (parentPath ++ [iden def])
      in topLine ++ childLines
  in
    concatMap processTopDef topDefs
    
-- | Sort definitions by line number
sortByLineNumber :: [Definition] -> [Definition]
sortByLineNumber defs = 
  -- Remove duplicate methods first
  let uniqueDefs = nubBy (\d1 d2 -> iden d1 == iden d2 && 
                                    defType d1 == defType d2 && 
                                    signature (defInfo d1) == signature (defInfo d2)) defs
  -- Then sort by line number
  in sortBy (\d1 d2 -> 
               let l1 = lineNum (defInfo d1)
                   l2 = lineNum (defInfo d2)
               in case (l1, l2) of
                    (Just n1, Just n2) -> compare n1 n2
                    (Just _, Nothing) -> LT
                    (Nothing, Just _) -> GT
                    (Nothing, Nothing) -> EQ
             ) uniqueDefs

-- | Convert the root structure to a text outline
rootToTextOutline :: Root -> String
rootToTextOutline = concatMap pathInfoToTextOutline