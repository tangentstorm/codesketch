module CodeSketch.Errors
  ( warn
  , error
  , debug
  , debugEnabled
  , setDebugEnabled
  ) where

import System.IO (hPutStrLn, stderr)
import System.Console.ANSI
  ( setSGR
  , SGR(..)
  , ConsoleLayer(..)
  , ColorIntensity(..)
  , Color(..)
  )
import Prelude hiding (error)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (when)

-- | Debug mode flag
{-# NOINLINE debugEnabled #-}
debugEnabled :: IORef Bool
debugEnabled = unsafePerformIO $ newIORef False

-- | Set debug mode
setDebugEnabled :: Bool -> IO ()
setDebugEnabled = writeIORef debugEnabled

-- | Output a warning to stderr in yellow
warn :: String -> IO ()
warn message = do
  setSGR [SetColor Foreground Vivid Yellow]
  hPutStrLn stderr ("Warning: " ++ message)
  setSGR [Reset]

-- | Output an error to stderr in red
error :: String -> IO ()
error message = do
  setSGR [SetColor Foreground Vivid Red]
  hPutStrLn stderr ("Error: " ++ message)
  setSGR [Reset]

-- | Output a debug message to stderr in blue (when debugging is enabled)
debug :: String -> IO ()
debug message = do
  isEnabled <- readIORef debugEnabled
  when isEnabled $ do
    setSGR [SetColor Foreground Vivid Blue]
    hPutStrLn stderr ("Debug: " ++ message)
    setSGR [Reset]