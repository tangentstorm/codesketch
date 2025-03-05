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

-- | Convert a definition to a readable text line
definitionToTextLine :: Definition -> String
definitionToTextLine def = 
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

-- | Build a hierarchical text tree of definitions
definitionsToTextTree :: [Definition] -> [String] -> String
definitionsToTextTree definitions parentPath =
  let 
    -- Top-level definitions (no parent or parent not in scope)
    isTopLevel def = case parent (defInfo def) of
                     Nothing -> True
                     Just p -> not (p `elem` (map iden definitions))
    
    -- Get top-level definitions
    topDefs = filter isTopLevel definitions
    
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
          
          -- Generate output
          topLine = indent ++ definitionToTextLine def ++ "\n"
          childLines = if null childDefs 
                        then "" 
                        else definitionsToTextTree childDefs (parentPath ++ [iden def])
      in topLine ++ childLines
  in
    concatMap processTopDef topDefs

-- | Convert the root structure to a text outline
rootToTextOutline :: Root -> String
rootToTextOutline = concatMap pathInfoToTextOutline