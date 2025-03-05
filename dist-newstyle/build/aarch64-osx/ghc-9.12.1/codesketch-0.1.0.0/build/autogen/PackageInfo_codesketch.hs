{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_codesketch (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "codesketch"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Tool for quickly getting a rough outline of the code in your files"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
