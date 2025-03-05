module CodeSketch.TreeSitter.Rust
  ( extractRustDefinitions
  ) where

import CodeSketch.Types
import CodeSketch.Errors as Errors
import CodeSketch.TreeSitter.Common

import TreeSitter
import TreeSitter.Rust (rustLanguage)
import TreeSitter.Node
import System.FilePath (takeExtension)
import Control.Monad (forM)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import Data.Maybe (mapMaybe, catMaybes, listToMaybe, isJust)
import Data.List (find)

-- | Extract Rust definitions from a file using tree-sitter
extractRustDefinitions :: FilePath -> IO (Maybe [Definition])
extractRustDefinitions filename = do
  if takeExtension filename /= ".rs"
    then return Nothing
    else do
      parseResult <- parseFile rustLanguage filename
      case parseResult of
        Nothing -> return Nothing
        Just (tree, source) -> do
          rootNode <- rootNode tree
          
          -- Extract all types of definitions
          functions <- findFunctions rootNode source
          structs <- findStructs rootNode source
          enums <- findEnums rootNode source
          traits <- findTraits rootNode source
          impls <- findImplBlocks rootNode source
          modules <- findModules rootNode source
          
          -- Combine all definitions
          let allDefs = functions ++ structs ++ enums ++ traits ++ impls ++ modules
          
          -- Build parent-child relationships
          let treeDefs = buildDefinitionTree allDefs
          
          return $ Just treeDefs

-- | Find all function definitions
findFunctions :: Node -> BS.ByteString -> IO [Definition]
findFunctions root source = do
  -- Find all function_item nodes (Rust functions)
  functions <- queryNodes rustLanguage root "((function_item) @function)" source
  
  -- Process each function node
  mapM (functionToDefinition source) functions

-- | Convert a function_item node to a Definition
functionToDefinition :: BS.ByteString -> Node -> IO Definition
functionToDefinition source funcNode = do
  -- Find the function name node
  nameNodes <- queryNodes rustLanguage funcNode "(function_item name: (identifier) @name)" source
  name <- case nameNodes of
            (nameNode:_) -> nodeText nameNode source
            [] -> return $ T.pack "unknown"
  
  -- Check if it's public
  visibilityNodes <- queryNodes rustLanguage funcNode "(visibility_modifier) @visibility" source
  visText <- case visibilityNodes of
               (visNode:_) -> nodeText visNode source
               [] -> return $ T.pack ""
  let vis = if visText == T.pack "pub" then Public else Private
  
  -- Get signature (params + return type)
  parameterNodes <- queryNodes rustLanguage funcNode "(parameters) @params" source
  signatureText <- case parameterNodes of
                   (paramNode:_) -> nodeText paramNode source
                   [] -> return $ T.pack ""
  
  -- Get line number
  lineNum <- nodeLineNumber funcNode
  
  -- Create the definition
  return $ Definition 
    { iden = T.unpack name
    , defType = Function
    , visibility = vis
    , defInfo = DefInfo (Just $ T.unpack signatureText) Nothing [] (Just lineNum)
    }

-- | Find all struct definitions
findStructs :: Node -> BS.ByteString -> IO [Definition]
findStructs root source = do
  -- Find all struct_item nodes
  structs <- queryNodes rustLanguage root "((struct_item) @struct)" source
  mapM (structToDefinition source) structs

-- | Convert a struct_item node to a Definition
structToDefinition :: BS.ByteString -> Node -> IO Definition
structToDefinition source structNode = do
  -- Find the struct name
  nameNodes <- queryNodes rustLanguage structNode "(struct_item name: (identifier) @name)" source
  name <- case nameNodes of
            (nameNode:_) -> nodeText nameNode source
            [] -> return $ T.pack "unknown"
  
  -- Check visibility
  visibilityNodes <- queryNodes rustLanguage structNode "(visibility_modifier) @visibility" source
  visText <- case visibilityNodes of
               (visNode:_) -> nodeText visNode source
               [] -> return $ T.pack ""
  let vis = if visText == T.pack "pub" then Public else Private
  
  -- Get line number
  lineNum <- nodeLineNumber structNode
  
  return $ Definition 
    { iden = T.unpack name
    , defType = Struct
    , visibility = vis
    , defInfo = DefInfo Nothing Nothing [] (Just lineNum)
    }

-- | Find all enum definitions
findEnums :: Node -> BS.ByteString -> IO [Definition]
findEnums root source = do
  -- Find all enum_item nodes
  enums <- queryNodes rustLanguage root "((enum_item) @enum)" source
  mapM (enumToDefinition source) enums

-- | Convert an enum_item node to a Definition
enumToDefinition :: BS.ByteString -> Node -> IO Definition
enumToDefinition source enumNode = do
  -- Find the enum name
  nameNodes <- queryNodes rustLanguage enumNode "(enum_item name: (identifier) @name)" source
  name <- case nameNodes of
            (nameNode:_) -> nodeText nameNode source
            [] -> return $ T.pack "unknown"
  
  -- Check visibility
  visibilityNodes <- queryNodes rustLanguage enumNode "(visibility_modifier) @visibility" source
  visText <- case visibilityNodes of
               (visNode:_) -> nodeText visNode source
               [] -> return $ T.pack ""
  let vis = if visText == T.pack "pub" then Public else Private
  
  -- Get line number
  lineNum <- nodeLineNumber enumNode
  
  return $ Definition 
    { iden = T.unpack name
    , defType = Enum
    , visibility = vis
    , defInfo = DefInfo Nothing Nothing [] (Just lineNum)
    }

-- | Find all trait definitions
findTraits :: Node -> BS.ByteString -> IO [Definition]
findTraits root source = do
  -- Find all trait_item nodes
  traits <- queryNodes rustLanguage root "((trait_item) @trait)" source
  mapM (traitToDefinition source) traits

-- | Convert a trait_item node to a Definition
traitToDefinition :: BS.ByteString -> Node -> IO Definition
traitToDefinition source traitNode = do
  -- Find the trait name
  nameNodes <- queryNodes rustLanguage traitNode "(trait_item name: (identifier) @name)" source
  name <- case nameNodes of
            (nameNode:_) -> nodeText nameNode source
            [] -> return $ T.pack "unknown"
  
  -- Check visibility
  visibilityNodes <- queryNodes rustLanguage traitNode "(visibility_modifier) @visibility" source
  visText <- case visibilityNodes of
               (visNode:_) -> nodeText visNode source
               [] -> return $ T.pack ""
  let vis = if visText == T.pack "pub" then Public else Private
  
  -- Get line number
  lineNum <- nodeLineNumber traitNode
  
  -- Find methods within the trait
  methodNodes <- queryNodes rustLanguage traitNode 
    "(trait_item body: (declaration_list (function_signature) @trait_method))" source
  
  methodNames <- forM methodNodes $ \methodNode -> do
    nameNodes <- queryNodes rustLanguage methodNode "(function_signature name: (identifier) @name)" source
    case nameNodes of
      (nameNode:_) -> nodeText nameNode source
      [] -> return $ T.pack ""
  
  let childrenNames = map T.unpack $ filter (not . T.null) methodNames
  
  return $ Definition 
    { iden = T.unpack name
    , defType = Trait
    , visibility = vis
    , defInfo = DefInfo Nothing Nothing childrenNames (Just lineNum)
    }

-- | Find all impl blocks
findImplBlocks :: Node -> BS.ByteString -> IO [Definition]
findImplBlocks root source = do
  -- Find all impl_item nodes
  impls <- queryNodes rustLanguage root "((impl_item) @impl)" source
  mapM (implToDefinition source) impls

-- | Convert an impl_item node to a Definition
implToDefinition :: BS.ByteString -> Node -> IO Definition
implToDefinition source implNode = do
  -- Find the type name being implemented
  typeNodes <- queryNodes rustLanguage implNode "(impl_item type: (type_identifier) @type)" source
  typeName <- case typeNodes of
                (typeNode:_) -> nodeText typeNode source
                [] -> return $ T.pack "unknown"
  
  -- Check if it's a trait implementation
  traitNodes <- queryNodes rustLanguage implNode "(impl_item trait: (type_identifier) @trait)" source
  traitName <- case traitNodes of
                 (traitNode:_) -> do
                   tn <- nodeText traitNode source
                   return $ Just tn
                 [] -> return Nothing
  
  -- Generate a unique identifier and signature for the impl block
  let uniqueId = case traitName of
                   Just trait -> T.unpack typeName ++ ":" ++ T.unpack trait
                   Nothing -> T.unpack typeName
                   
  let implSignature = case traitName of
                        Just trait -> Just $ T.unpack trait ++ " for " ++ T.unpack typeName
                        Nothing -> Nothing
  
  -- Get line number
  lineNum <- nodeLineNumber implNode
  
  -- Find methods within the impl block
  methodNodes <- queryNodes rustLanguage implNode 
    "(impl_item body: (declaration_list (function_item) @impl_method))" source
  
  methodNames <- forM methodNodes $ \methodNode -> do
    nameNodes <- queryNodes rustLanguage methodNode "(function_item name: (identifier) @name)" source
    case nameNodes of
      (nameNode:_) -> nodeText nameNode source
      [] -> return $ T.pack ""
  
  let childrenNames = map T.unpack $ filter (not . T.null) methodNames
  
  return $ Definition 
    { iden = uniqueId
    , defType = Impl
    , visibility = Private
    , defInfo = DefInfo implSignature Nothing childrenNames (Just lineNum)
    }

-- | Find all module definitions
findModules :: Node -> BS.ByteString -> IO [Definition]
findModules root source = do
  -- Find all mod_item nodes
  modules <- queryNodes rustLanguage root "((mod_item) @module)" source
  mapM (moduleToDefinition source) modules

-- | Convert a mod_item node to a Definition
moduleToDefinition :: BS.ByteString -> Node -> IO Definition
moduleToDefinition source moduleNode = do
  -- Find the module name
  nameNodes <- queryNodes rustLanguage moduleNode "(mod_item name: (identifier) @name)" source
  name <- case nameNodes of
            (nameNode:_) -> nodeText nameNode source
            [] -> return $ T.pack "unknown"
  
  -- Check visibility
  visibilityNodes <- queryNodes rustLanguage moduleNode "(visibility_modifier) @visibility" source
  visText <- case visibilityNodes of
               (visNode:_) -> nodeText visNode source
               [] -> return $ T.pack ""
  let vis = if visText == T.pack "pub" then Public else Private
  
  -- Get line number
  lineNum <- nodeLineNumber moduleNode
  
  return $ Definition 
    { iden = T.unpack name
    , defType = Module
    , visibility = vis
    , defInfo = DefInfo Nothing Nothing [] (Just lineNum)
    }

-- | Build parent-child relationships between definitions
buildDefinitionTree :: [Definition] -> [Definition]
buildDefinitionTree defs = defs 
  -- Parent-child relationships should be correctly established during extraction
  -- For simplicity, we're returning the definitions as-is for now
  -- In a real implementation, we would need to ensure all relationships are properly built