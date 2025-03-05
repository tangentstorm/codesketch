module CodeSketch.Scanner
  ( processPath
  , shouldProcessFile
  , processFile
  ) where

import CodeSketch.Types
import CodeSketch.Parser (extractDefinitions)
import CodeSketch.Errors as Errors

import System.Directory (doesFileExist, doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, takeFileName, (</>))
import Control.Monad (filterM)

-- | Check if a file should be processed based on extension
shouldProcessFile :: FilePath -> Bool
shouldProcessFile filename = case takeExtension filename of
  ".rs" -> True  -- Rust files
  _     -> False -- Skip unsupported files

-- | Check if a file or directory should be skipped (hidden files)
shouldSkip :: FilePath -> Bool
shouldSkip path = case takeFileName path of
  [] -> False
  (c:_) -> c == '.'

-- | Process a single file
processFile :: FilePath -> IO (Maybe PathInfo)
processFile filename = do
  Errors.debug $ "Processing file: " ++ filename
  
  if shouldProcessFile filename
    then do
      defs <- extractDefinitions filename
      if not (null defs)
        then return $ Just $ PathInfo filename defs
        else return Nothing
    else return Nothing

-- | Recursively scan a directory for processable files
scanDirectory :: FilePath -> IO [PathInfo]
scanDirectory dirPath = do
  entries <- listDirectory dirPath
  
  -- Filter out entries that should be skipped
  let fullPaths = map (dirPath </>) entries
  let validPaths = filter (not . shouldSkip) fullPaths
  
  -- Separate files and directories
  files <- filterM doesFileExist validPaths
  dirs <- filterM doesDirectoryExist validPaths
  
  -- Process files
  fileResults <- mapM processFile files
  let validResults = concat $ map (\r -> case r of Just x -> [x]; Nothing -> []) fileResults
  
  -- Recursively process directories
  dirResults <- mapM scanDirectory dirs
  
  -- Combine results
  return $ validResults ++ concat dirResults

-- | Process a path (file or directory)
processPath :: FilePath -> IO [PathInfo]
processPath path = do
  isFile <- doesFileExist path
  isDir <- doesDirectoryExist path
  
  if isFile then do
    result <- processFile path
    case result of
      Just info -> return [info]
      Nothing -> return []
  else if isDir then
    scanDirectory path
  else do
    Errors.error $ "Path does not exist: " ++ path
    return []