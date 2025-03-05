module Main (main) where

import CodeSketch.Types
import CodeSketch.Scanner (processPath)
import CodeSketch.Json (rootToJSONString)
import CodeSketch.Errors as Errors

import Options.Applicative
import System.Directory (canonicalizePath)
import System.IO (hSetEncoding, stdout, utf8)
import Data.Version (showVersion)
import Paths_codesketch (version)

-- | Command line options
data Options = Options
  { optPath :: FilePath
  , optDebug :: Bool
  }

-- | Command line parser
optionsParser :: Parser Options
optionsParser = Options
  <$> argument str
      ( metavar "PATH"
      <> help "Path to file or directory to analyze"
      )
  <*> switch
      ( long "debug"
      <> short 'd'
      <> help "Enable debug output"
      )

-- | Main entry point
main :: IO ()
main = do
  -- Parse command line options
  options <- execParser opts
  
  -- Set up UTF-8 encoding for output
  hSetEncoding stdout utf8
  
  -- Set debug mode if requested
  Errors.setDebugEnabled (optDebug options)
  
  -- Get absolute path
  absPath <- canonicalizePath (optPath options)
  
  -- Process the path and get results
  Errors.debug $ "Starting codesketch on: " ++ absPath
  results <- processPath absPath
  
  -- Print warning if no results found
  when (null results) $
    Errors.warn "No definitions found or no supported files in path."
  
  -- Output JSON to stdout
  putStrLn $ rootToJSONString results
  
  where
    opts = info (optionsParser <**> helper)
      ( fullDesc
      <> progDesc "Output a JSON outline of code definitions"
      <> header ("codesketch " ++ showVersion version ++ " - Code structure analyzer")
      )
    
    when :: Bool -> IO () -> IO ()
    when True action = action
    when False _ = pure ()