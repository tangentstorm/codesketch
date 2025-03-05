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
  let -- Type description
      typeDesc = case defType def of
        Module   -> "module"
        Struct   -> "struct"
        Enum     -> "enum"
        Function -> "fn"
        Trait    -> "trait"
        Impl     -> "impl"
        Other s  -> s
      
      -- Visibility description
      visDesc = case visibility def of
        Public    -> "public"
        Protected -> "protected"
        Private   -> "private"
      
      -- Name
      name = iden def
      
      -- Signature text for functions and impl blocks
      sigText = case defType def of
        Function -> case signature (defInfo def) of
                     Just sig -> " " ++ sig
                     Nothing -> ""
        Impl -> case signature (defInfo def) of
                 Just sig -> " " ++ sig
                 Nothing -> ""
        _ -> ""
  in visDesc ++ " " ++ typeDesc ++ " " ++ name ++ sigText

-- | Convert a path_info to a text outline with indentation
pathInfoToTextOutline :: PathInfo -> String
pathInfoToTextOutline pi =
  path pi ++ ":\n" ++ 
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