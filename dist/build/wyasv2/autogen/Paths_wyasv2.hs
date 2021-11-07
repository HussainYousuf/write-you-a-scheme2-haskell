{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_wyasv2 (
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

bindir     = "/home/hussain/.cabal/bin"
libdir     = "/home/hussain/.cabal/lib/x86_64-linux-ghc-8.6.5/wyasv2-0.1.0.0-EwLkcUIfG9O8NHkdaFISYF-wyasv2"
dynlibdir  = "/home/hussain/.cabal/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/hussain/.cabal/share/x86_64-linux-ghc-8.6.5/wyasv2-0.1.0.0"
libexecdir = "/home/hussain/.cabal/libexec/x86_64-linux-ghc-8.6.5/wyasv2-0.1.0.0"
sysconfdir = "/home/hussain/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "wyasv2_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "wyasv2_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "wyasv2_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "wyasv2_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "wyasv2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "wyasv2_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
