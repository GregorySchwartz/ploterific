{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_ploterific (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/gw/.cabal/bin"
libdir     = "/home/gw/.cabal/lib/x86_64-linux-ghc-8.10.4/ploterific-0.1.0.0-inplace"
dynlibdir  = "/home/gw/.cabal/lib/x86_64-linux-ghc-8.10.4"
datadir    = "/home/gw/.cabal/share/x86_64-linux-ghc-8.10.4/ploterific-0.1.0.0"
libexecdir = "/home/gw/.cabal/libexec/x86_64-linux-ghc-8.10.4/ploterific-0.1.0.0"
sysconfdir = "/home/gw/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ploterific_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ploterific_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "ploterific_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "ploterific_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ploterific_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ploterific_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
