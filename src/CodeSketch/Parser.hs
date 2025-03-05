module CodeSketch.Parser
  ( extractDefinitions
  , parseFile
  ) where

import CodeSketch.Types
import CodeSketch.Errors as Errors

import System.FilePath (takeExtension)
import Control.Exception (try, SomeException)
import System.Directory (doesFileExist)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char (isAlphaNum, isSpace)
import Data.List (isPrefixOf, isInfixOf, tails, findIndex)
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

-- | Extract Rust function definitions
extractRustFunctions :: String -> [Definition]
extractRustFunctions content = 
  let lns = zip [1..] (splitLines content)
      functionLines = filter (isFunctionDeclaration . snd) lns
  in map extractFunctionDef functionLines
  where
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
      in Definition name Function vis (DefInfo signature)
    
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
  in map extractStructDef structLines ++ 
     map extractEnumDef enumLines ++
     map extractTraitDef traitLines
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
      in Definition name Struct vis emptyDefInfo
    
    extractEnumDef (_, line) =
      let isPub = "pub enum " `isInfixOf` line
          vis = if isPub then Public else Private
          
          enumPos = findSubstring "enum " line + 5
          pubPos = findSubstring "pub enum " line
          startPos = if isPub && pubPos >= 0 then pubPos + 9 else enumPos
          
          nameStr = drop startPos line
          name = takeWhile isIdentChar (dropWhile isSpace nameStr)
      in Definition name Enum vis emptyDefInfo
    
    extractTraitDef (_, line) =
      let isPub = "pub trait " `isInfixOf` line
          vis = if isPub then Public else Private
          
          traitPos = findSubstring "trait " line + 6
          pubPos = findSubstring "pub trait " line
          startPos = if isPub && pubPos >= 0 then pubPos + 10 else traitPos
          
          nameStr = drop startPos line
          name = takeWhile isIdentChar (dropWhile isSpace nameStr)
      in Definition name Trait vis emptyDefInfo
      
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

-- | Extract Rust module definitions
extractRustModules :: String -> [Definition]
extractRustModules content = 
  let lns = zip [1..] (splitLines content)
      moduleLines = filter (isModuleDeclaration . snd) lns
  in map extractModuleDef moduleLines
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
      in Definition name Module vis emptyDefInfo
    
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

-- | Parse a file and extract definitions based on language
extractDefinitions :: FilePath -> IO [Definition]
extractDefinitions filename = do
  maybeContent <- parseFile filename
  case maybeContent of
    Nothing -> return []
    Just content ->
      if takeExtension filename == ".rs"
        then do
          let functions = extractRustFunctions content
              structs = extractRustStructs content
              modules = extractRustModules content
          return $ functions ++ structs ++ modules
        else return []

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