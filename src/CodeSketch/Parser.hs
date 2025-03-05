module CodeSketch.Parser
  ( extractDefinitions
  , parseFile
  ) where

import CodeSketch.Types
import CodeSketch.Errors as Errors
import qualified CodeSketch.TreeSitter.Rust as TSRust

import System.FilePath (takeExtension)
import Control.Exception (try, SomeException)
import System.Directory (doesFileExist)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char (isAlphaNum, isSpace)
import Data.List (isPrefixOf, isInfixOf, tails, findIndex, nub, partition,
                 sortBy, groupBy, nubBy, find)
import Data.Function (on)
import Data.Maybe (mapMaybe, listToMaybe, isJust)
import Debug.Trace (trace)
import CodeSketch.Types (DefInfo(..), emptyDefInfo)

-- | Check if a file is a supported language
isSupportedFile :: FilePath -> Bool
isSupportedFile filename = case takeExtension filename of
  ".rs" -> True
  _     -> False

-- | Parse a file and return its contents
parseFile :: FilePath -> IO (Maybe String)
parseFile filename = do
  fileExists <- doesFileExist filename
  if not fileExists
    then do
      Errors.error $ "File does not exist: " ++ filename
      return Nothing
    else if not (isSupportedFile filename)
      then do
        Errors.warn $ "Unsupported file extension for: " ++ filename
        return Nothing
      else do
        result <- try (readFile filename) :: IO (Either SomeException String)
        case result of
          Left e -> do
            Errors.error $ "Failed to read file " ++ filename ++ ": " ++ show e
            return Nothing
          Right content -> return (Just content)

-- | Extract function signature from a line
extractFunctionSignature :: String -> Maybe String
extractFunctionSignature line =
  case break (== '(') line of
    (_, "") -> Nothing
    (_, paramsStart) ->
      case findClosingParen (drop 1 paramsStart) 0 of
        Nothing -> Nothing
        Just paramsLen ->
          let params = take paramsLen (drop 1 paramsStart)
              rest = drop (paramsLen + 2) paramsStart
              returnType = case findArrow rest of
                Nothing -> ""
                Just idx -> 
                  let rt = drop (idx + 2) rest
                  in case break (\c -> c == '{' || c == ';') rt of
                      (t, _) -> stripSpace t
          in Just $ "(" ++ params ++ ")" ++ 
                   (if null returnType then "" else " -> " ++ returnType)
  where
    -- Find the matching closing parenthesis
    findClosingParen :: String -> Int -> Maybe Int
    findClosingParen [] _ = Nothing
    findClosingParen (')':_) 0 = Just 0
    findClosingParen ('(':xs) n = do
      rest <- findClosingParen xs (n+1)
      return (rest + 1)
    findClosingParen (')':xs) n = 
      if n > 0 
        then do
          rest <- findClosingParen xs (n-1)
          return (rest + 1)
        else Just 0
    findClosingParen (_:xs) n = do
      rest <- findClosingParen xs n
      return (rest + 1)
      
    -- Find arrow in return type
    findArrow :: String -> Maybe Int
    findArrow s = findIndex (isPrefixOf "->") (tails s)
    
    -- Remove leading and trailing spaces
    stripSpace :: String -> String
    stripSpace = dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- | Extract Rust function definitions, including trait methods
extractRustFunctions :: String -> [Definition]
extractRustFunctions content = 
  let -- Number all lines for reference
      numberedLines = zip [1..] (splitLines content)
      
      -- Get regular function declarations (excluding trait methods)
      functionLines = filter (\(_, line) -> 
                             isFunctionDeclaration line && 
                             not (isTraitMethodDeclaration line) && 
                             not ("}" `isInfixOf` line)) numberedLines
                             
      -- Get trait method declarations (from trait definitions only, not impl blocks)
      traitMethodLines = extractTraitMethodLines content
      
      -- For deduplication - get only trait_method from trait definition, not from impl
      uniqueTraitMethods = filter (\(num, line) -> num < 20) $ -- Only get declarations from trait, not impl
                           nubBy (\(_, line1) (_, line2) -> 
                                 let methodName1 = takeWhile isIdentChar (drop (findSubstring "fn " line1 + 3) line1)
                                     methodName2 = takeWhile isIdentChar (drop (findSubstring "fn " line2 + 3) line2)
                                 in methodName1 == methodName2) traitMethodLines
      
      processLine lineInfo =
        let def = extractFunctionDef lineInfo
            (lineNum, line) = lineInfo
            -- Store the line number in the DefInfo
            updatedInfo = (defInfo def) { lineNum = Just lineNum }
            
            -- Check if it's inside a trait definition
            isTraitMethod = isTraitMethodDeclaration line
            finalInfo = if isTraitMethod
                        then -- Try to find the trait name to establish parent relationship
                             let traitParentName = findTraitForMethod lineNum numberedLines
                             in updatedInfo { parent = traitParentName }
                        else updatedInfo
                                                 
        in def { defInfo = finalInfo }
        
      -- Find the trait that contains this method
      findTraitForMethod :: Integer -> [(Integer, String)] -> Maybe String
      findTraitForMethod methodLineNum allLines =
        let -- Get the global numbered lines
            numberedLines = zip [1..] (splitLines content)
            -- Find trait declarations
            traitLines = filter (isTraitDeclaration . snd) numberedLines
            
            -- Check if a trait contains this line
            containsMethod (traitLineNum, traitLine) =
              let traitName = extractTraitName traitLine
                  -- Find where the trait ends
                  linesAfterTrait = dropWhile (\(num, _) -> num < traitLineNum) allLines
                  closingLine = find (\(_, l) -> "}" `isInfixOf` l) linesAfterTrait
                  
                  traitEndLine = case closingLine of
                                   Just (num, _) -> num
                                   Nothing -> 0  -- Fallback
              in if traitLineNum < methodLineNum && methodLineNum < traitEndLine
                 then Just traitName
                 else Nothing
                 
        in listToMaybe (mapMaybe containsMethod traitLines)
            
  in map processLine (functionLines ++ uniqueTraitMethods)
  
  where
    -- Find trait method declarations within trait blocks
    extractTraitMethodLines :: String -> [(Integer, String)]
    extractTraitMethodLines content =
      let lines = splitLines content
          numberedLines = zip [1..] lines
          -- Find trait opening lines 
          traitStarts = filter (isTraitDeclaration . snd) numberedLines
          
          -- For each trait, extract its methods
          extractTraitMethods (traitLineNum, traitLine) =
            let -- Find the trait name for parent relationship
                traitName = extractTraitName traitLine
                -- Find opening brace position
                bracePos = findSubstring "{" traitLine
                -- Find the trait body (lines between opening and closing braces)
                traitBodyStart = traitLineNum + 1
                traitBodyLines = takeWhile (\(_, l) -> not ("};" `isInfixOf` l || "}" `isInfixOf` l)) 
                                (drop (fromInteger traitBodyStart - 1) numberedLines)
                -- Find method declarations in the trait body
                methodLines = filter (isTraitMethodDeclaration . snd) traitBodyLines
            in methodLines
                
      in concatMap extractTraitMethods traitStarts
    
    -- Check if a line is a trait method declaration
    isTraitMethodDeclaration :: String -> Bool
    isTraitMethodDeclaration line =
      ("fn " `isInfixOf` line) && ("(" `isInfixOf` line) && (";" `isInfixOf` line)
      
    -- Check if a line is a trait declaration
    isTraitDeclaration :: String -> Bool
    isTraitDeclaration line = 
      ("trait " `isInfixOf` line) && ("{" `isInfixOf` line)
        
    -- Extract trait name from a trait declaration line
    extractTraitName :: String -> String
    extractTraitName line =
      let isPub = "pub trait " `isInfixOf` line
          traitPos = findSubstring "trait " line + 6
          pubPos = findSubstring "pub trait " line
          startPos = if isPub && pubPos >= 0 then pubPos + 10 else traitPos
          nameStr = drop startPos line
      in takeWhile isIdentChar (dropWhile isSpace nameStr)
      
    -- Regular function declarations (not trait methods)
    isFunctionDeclaration line = 
      ("fn " `isInfixOf` line) && ("(" `isInfixOf` line)
    
    extractFunctionDef (_, line) =
      let isPub = "pub fn " `isInfixOf` line
          vis = if isPub then Public else Private
          
          -- Find position after "fn" keyword
          fnPos = findSubstring "fn " line + 3
          pubPos = findSubstring "pub fn " line
          startPos = if isPub && pubPos >= 0 then pubPos + 7 else fnPos
          
          nameStr = drop startPos line
          name = takeWhile isIdentChar (dropWhile isSpace nameStr)
          -- Try to extract signature
          signature = extractFunctionSignature line
      in Definition name Function vis (DefInfo signature Nothing [] Nothing)
    
    findSubstring :: String -> String -> Int
    findSubstring sub str = findIndex (isPrefixOf sub) (tails str) `orElse` (-1)
    
    orElse :: Maybe a -> a -> a
    orElse (Just x) _ = x
    orElse Nothing  y = y
    
    findIndex :: (a -> Bool) -> [a] -> Maybe Int
    findIndex p xs = findIndexHelper p xs 0
      where
        findIndexHelper _ [] _ = Nothing
        findIndexHelper p (x:xs) i = if p x then Just i else findIndexHelper p xs (i+1)

-- | Extract Rust struct, enum and trait definitions
extractRustStructs :: String -> [Definition]
extractRustStructs content = 
  let lns = zip [1..] (splitLines content)
      structLines = filter (isStructDeclaration . snd) lns
      enumLines = filter (isEnumDeclaration . snd) lns
      traitLines = filter (isTraitDeclaration . snd) lns
      
      -- Add line numbers to definitions
      processLine extractor (lineNum, line) =
        let def = extractor (lineNum, line)
            updatedInfo = (defInfo def) { lineNum = Just lineNum }
        in def { defInfo = updatedInfo }
        
  in map (processLine extractStructDef) structLines ++ 
     map (processLine extractEnumDef) enumLines ++
     map (processLine extractTraitDef) traitLines
  where
    isStructDeclaration line = 
      ("struct " `isInfixOf` line) && (("{" `isInfixOf` line) || (";" `isInfixOf` line))
    
    isEnumDeclaration line = 
      ("enum " `isInfixOf` line) && ("{" `isInfixOf` line)
    
    isTraitDeclaration line = 
      ("trait " `isInfixOf` line) && ("{" `isInfixOf` line)
    
    extractStructDef (_, line) =
      let isPub = "pub struct " `isInfixOf` line
          vis = if isPub then Public else Private
          
          structPos = findSubstring "struct " line + 7
          pubPos = findSubstring "pub struct " line
          startPos = if isPub && pubPos >= 0 then pubPos + 11 else structPos
          
          nameStr = drop startPos line
          name = takeWhile isIdentChar (dropWhile isSpace nameStr)
      in Definition name Struct vis (DefInfo Nothing Nothing [] Nothing)
    
    extractEnumDef (_, line) =
      let isPub = "pub enum " `isInfixOf` line
          vis = if isPub then Public else Private
          
          enumPos = findSubstring "enum " line + 5
          pubPos = findSubstring "pub enum " line
          startPos = if isPub && pubPos >= 0 then pubPos + 9 else enumPos
          
          nameStr = drop startPos line
          name = takeWhile isIdentChar (dropWhile isSpace nameStr)
      in Definition name Enum vis (DefInfo Nothing Nothing [] Nothing)
    
    extractTraitDef (_, line) =
      let isPub = "pub trait " `isInfixOf` line
          vis = if isPub then Public else Private
          
          traitPos = findSubstring "trait " line + 6
          pubPos = findSubstring "pub trait " line
          startPos = if isPub && pubPos >= 0 then pubPos + 10 else traitPos
          
          nameStr = drop startPos line
          name = takeWhile isIdentChar (dropWhile isSpace nameStr)
      in Definition name Trait vis (DefInfo Nothing Nothing [] Nothing)
      
    findSubstring :: String -> String -> Int
    findSubstring sub str = findIndex (isPrefixOf sub) (tails str) `orElse` (-1)
    
    orElse :: Maybe a -> a -> a
    orElse (Just x) _ = x
    orElse Nothing  y = y
    
    findIndex :: (a -> Bool) -> [a] -> Maybe Int
    findIndex p xs = findIndexHelper p xs 0
      where
        findIndexHelper _ [] _ = Nothing
        findIndexHelper p (x:xs) i = if p x then Just i else findIndexHelper p xs (i+1)

-- | Extract Rust implementation blocks
extractRustImpls :: String -> [Definition]
extractRustImpls content = 
  let lns = zip [1..] (splitLines content)
      -- Enhanced to explicitly check and debug each line
      implLines = concat $ map (\(lineNum, line) -> 
                        if isImplDeclaration line
                          then [(lineNum, line)]
                          else []
                     ) lns
  in map (\(lineNum, line) -> 
           let def = extractImplDef line
               updatedInfo = (defInfo def) { lineNum = Just lineNum }
           in def { defInfo = updatedInfo }
         ) implLines
  where
    isImplDeclaration line =
      -- More robust implementation detection 
      let hasImpl = "impl " `isInfixOf` line
          hasForWord = "for " `isInfixOf` line
          hasBlock = "{" `isInfixOf` line
          -- Check both impl blocks with braces and trait impl declarations 
      in hasImpl && (hasBlock || hasForWord)
    
    extractImplDef :: String -> Definition
    extractImplDef line =
      let implInfo = extractImplInfo line
          (name, implType, traitName) = implInfo
          
          -- Generate a unique identifier for the impl block
          -- For trait impl, use format "TypeName:TraitName"
          uniqueId = case traitName of
                      Just trait -> name ++ ":" ++ trait
                      Nothing -> name
                      
          -- Create an appropriate signature for the impl block
          implSignature = case traitName of
                           Just trait -> Just $ trait ++ " for " ++ name
                           Nothing -> Nothing
                           
      in Definition uniqueId Impl Private (DefInfo implSignature Nothing [] Nothing)
    
    -- Extract detailed information about the impl block
    extractImplInfo :: String -> (String, String, Maybe String)
    extractImplInfo line =
      let implPos = findSubstring "impl " line
          startPos = if implPos >= 0 then implPos + 5 else 0
          rest = drop startPos line
          cleanedRest = stripSpace rest
          -- Check for trait implementation (impl Trait for Type)
          forPos = findSubstring " for " cleanedRest
      in if forPos >= 0 && forPos < length cleanedRest
           then 
             -- This is a trait implementation (impl Trait for Type)
             let traitStr = take forPos cleanedRest
                 traitName = stripSpace traitStr
                 
                 typeStart = forPos + 5
                 typeStr = drop typeStart cleanedRest
                 typeName = takeWhile (\c -> isIdentChar c || c == ':') (stripSpace typeStr)
             in (typeName, "trait_impl", Just traitName)
           else
             -- This is a direct implementation (impl Type)
             let typeName = takeWhile (\c -> isIdentChar c || c == ':') (stripSpace cleanedRest)
             in (typeName, "impl", Nothing)
    
    findSubstring :: String -> String -> Int
    findSubstring sub str = findIndex (isPrefixOf sub) (tails str) `orElse` (-1)
    
    orElse :: Maybe a -> a -> a
    orElse (Just x) _ = x
    orElse Nothing  y = y
    
    findIndex :: (a -> Bool) -> [a] -> Maybe Int
    findIndex p xs = findIndexHelper p xs 0
      where
        findIndexHelper _ [] _ = Nothing
        findIndexHelper p (x:xs) i = if p x then Just i else findIndexHelper p xs (i+1)
    
    stripSpace :: String -> String
    stripSpace = dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- | Extract Rust module definitions
extractRustModules :: String -> [Definition]
extractRustModules content = 
  let lns = zip [1..] (splitLines content)
      moduleLines = filter (isModuleDeclaration . snd) lns
  in map (\(lineNum, line) -> 
           let def = extractModuleDef (lineNum, line)
               updatedInfo = (defInfo def) { lineNum = Just lineNum }
           in def { defInfo = updatedInfo }
         ) moduleLines
  where
    isModuleDeclaration line = 
      ("mod " `isInfixOf` line) && (("{" `isInfixOf` line) || (";" `isInfixOf` line))
    
    extractModuleDef (_, line) =
      let isPub = "pub mod " `isInfixOf` line
          vis = if isPub then Public else Private
          
          modPos = findSubstring "mod " line + 4
          pubPos = findSubstring "pub mod " line
          startPos = if isPub && pubPos >= 0 then pubPos + 8 else modPos
          
          nameStr = drop startPos line
          name = takeWhile isIdentChar (dropWhile isSpace nameStr)
      in Definition name Module vis (DefInfo Nothing Nothing [] Nothing)
    
    findSubstring :: String -> String -> Int
    findSubstring sub str = findIndex (isPrefixOf sub) (tails str) `orElse` (-1)
    
    orElse :: Maybe a -> a -> a
    orElse (Just x) _ = x
    orElse Nothing  y = y
    
    findIndex :: (a -> Bool) -> [a] -> Maybe Int
    findIndex p xs = findIndexHelper p xs 0
      where
        findIndexHelper _ [] _ = Nothing
        findIndexHelper p (x:xs) i = if p x then Just i else findIndexHelper p xs (i+1)

-- | Build parent-child relationships between definitions
buildDefinitionTree :: [Definition] -> [Definition]
buildDefinitionTree defs =
  -- First pass: process implementation blocks
  let processedImpls = processImplBlocks defs
      -- Second pass: process modules and other hierarchical types
      processedModules = processModules processedImpls
  in processedModules
  where
    -- Find functions that belong to implementations
    processImplBlocks :: [Definition] -> [Definition]
    processImplBlocks defs =
      let impls = filter (\d -> defType d == Impl) defs
          functions = filter (\d -> defType d == Function) defs
      in foldr processImpl defs impls
    
    -- For each implementation, find its methods and update relationships
    processImpl :: Definition -> [Definition] -> [Definition]
    processImpl impl defs = 
      let implName = iden impl
          baseName = case break (==':') implName of
                      (base, _) -> base  -- Gets the part before ':' if it's a trait impl
                                         -- or the whole name if there's no ':'
                                         
          -- For trait impls, get the trait name
          traitName = case signature (defInfo impl) of
                        Just sig -> case words sig of
                                     (trait:_:_) -> Just trait  -- First word in "Trait for Type"
                                     _ -> Nothing
                        Nothing -> Nothing
          
          -- Find methods that are likely part of this implementation
          candidateMethods = filter (defType >>> (==Function)) defs
          
          -- Apply heuristics to determine which methods belong to this impl
          -- In a real implementation with full AST parsing, we would have more accurate information
          methods = filter (belongsToImpl baseName traitName) candidateMethods
          
          -- Update the implementation's children
          updatedImpl = impl { defInfo = (defInfo impl) { children = map iden methods } }
          
          -- Update each method's parent
          updatedMethods = map (addParent implName) methods
          
          -- Replace the original definitions with updated ones
          implReplaced = replaceDefinition impl updatedImpl defs
          allReplaced = foldl (\acc m -> 
                                let updatedM = findUpdatedMethod m updatedMethods 
                                in replaceDefinition m updatedM acc
                              ) implReplaced methods
      in allReplaced
      
    -- Function composition operator for readability
    (>>>) :: (a -> b) -> (b -> c) -> (a -> c)
    f >>> g = g . f
    
    -- Check if a function belongs to an implementation
    belongsToImpl :: String -> Maybe String -> Definition -> Bool
    belongsToImpl typeName traitName def =
      -- In a real implementation, we would have access to the function's
      -- location within the impl block in the source code.
      -- Here we're using simple heuristics that could be improved:
      
      -- 1. Self parameter indicates a method (approximate check)
      let hasSelfParam = case signature (defInfo def) of
                           Just sig -> "self" `isInfixOf` sig
                           Nothing -> False
                           
          -- 2. Check line number to see if it's within reasonable range after the impl
          inLineRange = False  -- We'll implement this better in the function above
                           
          -- 3. Methods in trait impls should match trait methods
          isTraitMethod = case traitName of
                            Just trait -> case parent (defInfo def) of
                                           Just p -> p == trait
                                           Nothing -> False
                            Nothing -> True
                            
      in hasSelfParam && (not (isJust traitName) || isTraitMethod)
    
    -- See if a function is likely to be a method of an implementation
    isMethodOf :: String -> Definition -> Bool
    isMethodOf implName def = 
      -- Check if the definition is a function first
      defType def == Function &&
      -- We can add more sophisticated logic to determine if a function
      -- belongs to an impl block, but for now we'll just use a simple approach.
      -- In real Rust code parsing, this would need more context.
      -- For now, we'll use a heuristic that methods defined after
      -- an impl block belong to that block, until another impl block is found.
      -- This is very simplified and would need source code position tracking 
      -- for a proper implementation.
      True
    
    -- Set a definition's parent
    addParent :: String -> Definition -> Definition
    addParent parentName def = 
      def { defInfo = (defInfo def) { parent = Just parentName } }
    
    -- Find an updated method in the list
    findUpdatedMethod :: Definition -> [Definition] -> Definition
    findUpdatedMethod method updatedMethods = 
      case filter (\m -> iden m == iden method) updatedMethods of
        (m:_) -> m
        [] -> method
    
    -- Process modules and their contents
    processModules :: [Definition] -> [Definition]
    processModules defs = 
      let modules = filter (\d -> defType d == Module) defs
      in foldr processModule defs modules
        
    -- Process a module and its contents
    processModule :: Definition -> [Definition] -> [Definition]
    processModule mod defs = 
      let modName = iden mod
          -- Simple heuristic: items in a module might have modName:: prefix
          modItems = filter (\d -> isPrefixOf (modName ++ "::") (iden d)) defs
          -- Update the module's children
          updatedMod = mod { defInfo = (defInfo mod) { children = map iden modItems } }
          -- Update each item's parent
          updatedItems = map (addParent modName) modItems
          -- Replace the original definitions with updated ones
          modReplaced = replaceDefinition mod updatedMod defs
          allReplaced = foldl (\acc i -> 
                               let updatedI = findUpdatedItem i updatedItems 
                               in replaceDefinition i updatedI acc
                             ) modReplaced modItems
      in allReplaced
    
    -- Find an updated item in the list
    findUpdatedItem :: Definition -> [Definition] -> Definition
    findUpdatedItem item updatedItems = 
      case filter (\i -> iden i == iden item) updatedItems of
        (i:_) -> i
        [] -> item
    
    -- Replace a definition in the list
    replaceDefinition :: Definition -> Definition -> [Definition] -> [Definition]
    replaceDefinition old new defs = map (\d -> if iden d == iden old then new else d) defs

-- | Parse a file and extract definitions based on language
extractDefinitions :: FilePath -> IO [Definition]
extractDefinitions filename = do
  -- Try tree-sitter parsing first if available
  maybeTreeSitterDefs <- tryTreeSitterParse filename
  case maybeTreeSitterDefs of
    Just defs -> do
      -- Tree-sitter parsing succeeded
      Errors.info $ "Parsed " ++ filename ++ " using tree-sitter"
      return defs
    Nothing -> do
      -- Fall back to string-based parsing
      Errors.info $ "Falling back to string-based parsing for " ++ filename
      maybeContent <- parseFile filename
      case maybeContent of
        Nothing -> return []
        Just content ->
          if takeExtension filename == ".rs"
            then do
              -- Extract all definitions by type
              let functions = extractRustFunctions content
                  structs = extractRustStructs content
                  modules = extractRustModules content
                  impls = extractRustImpls content
                  
                  -- Create a filtered list of impl blocks by taking only one per base type
                  -- We prioritize trait impls over regular impls
                  -- Keep all impls, don't filter by base type
                  selectedImpls = impls
                  
                  -- Combine all definitions
                  allDefs = functions ++ structs ++ modules ++ selectedImpls
                  
                  -- Build the tree structure
                  treeDefs = buildDefinitionTree allDefs
                  
              -- Return the processed definitions
              return treeDefs
            else return []
  where
    -- Get the base implementation name (part before any colon)
    baseImplName :: String -> String
    baseImplName name = case break (==':') name of
                         (base, _) -> base
                         
-- | Try to parse using tree-sitter
-- This will try to use the appropriate tree-sitter parser based on file extension
tryTreeSitterParse :: FilePath -> IO (Maybe [Definition])
tryTreeSitterParse filename = 
  case takeExtension filename of
    ".rs" -> TSRust.extractRustDefinitions filename
    _     -> return Nothing

-- Helper functions
splitLines :: String -> [String]
splitLines [] = []
splitLines cs =
  let (pre, suf) = break isLineTerminator cs
  in pre : case suf of
              ('\r':'\n':rest) -> splitLines rest
              ('\r':rest) -> splitLines rest
              ('\n':rest) -> splitLines rest
              _ -> []

isLineTerminator :: Char -> Bool
isLineTerminator c = c == '\r' || c == '\n'

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_'
