module CodeSketch.TreeSitter.Common
  ( parseFile
  , queryNodes
  , nodeText
  , nodeLineNumber
  , extractDefinition
  , tryTreeSitterParse
  ) where

import CodeSketch.Types
import CodeSketch.Errors as Errors

import TreeSitter
import TreeSitter.Language
import TreeSitter.Node
import System.FilePath (takeExtension)
import Control.Exception (try, SomeException)
import System.Directory (doesFileExist)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import Data.Maybe (mapMaybe, catMaybes)
import Data.List (find)

-- | Parse a file using tree-sitter
parseFile :: Language -> FilePath -> IO (Maybe (Tree, BS.ByteString))
parseFile language filename = do
  fileExists <- doesFileExist filename
  if not fileExists
    then do
      Errors.error $ "File does not exist: " ++ filename
      return Nothing
    else do
      result <- try (BS.readFile filename) :: IO (Either SomeException BS.ByteString)
      case result of
        Left e -> do
          Errors.error $ "Failed to read file " ++ filename ++ ": " ++ show e
          return Nothing
        Right source -> do
          parser <- newParser
          setLanguage parser language
          tree <- parseByteString parser source
          return $ Just (tree, source)

-- | Query nodes matching the pattern
queryNodes :: Language -> Node -> String -> BS.ByteString -> IO [Node]
queryNodes language node queryStr source = do
  query <- newQuery language queryStr
  matches <- execQuery query node source
  return $ concatMap (\(Match _ nodes _) -> nodes) matches

-- | Get text from a node
nodeText :: Node -> BS.ByteString -> IO T.Text
nodeText node source = do
  startByte <- nodeStartByte node
  endByte <- nodeEndByte node
  let content = BS.take (endByte - startByte) (BS.drop startByte source)
  return $ TE.decodeUtf8With lenientDecode content

-- | Get line number from a node
nodeLineNumber :: Node -> IO Integer
nodeLineNumber node = do
  (line, _) <- nodeStartPoint node
  -- Tree-sitter line numbers are 0-based, but we want 1-based
  return $ toInteger (line + 1)

-- | Generic function to extract definition from a parsed file
-- This will be implemented by language-specific modules
extractDefinition :: Language -> FilePath -> IO (Maybe [Definition])
extractDefinition _ _ = do
  Errors.warn "Extracting definitions using tree-sitter not implemented for this language"
  return Nothing

-- | Try to parse a file using tree-sitter, return Nothing if not supported or fails
tryTreeSitterParse :: FilePath -> IO (Maybe [Definition])
tryTreeSitterParse filename = do
  -- Check if we have a parser for this file extension
  -- This will be called by specific language modules like TreeSitter.Rust
  Errors.warn "Tree-sitter parsing attempted but not implemented for this file"
  return Nothing