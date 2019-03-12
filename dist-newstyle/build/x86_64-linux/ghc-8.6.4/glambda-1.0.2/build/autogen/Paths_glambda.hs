{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_glambda (
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
version = Version [1,0,2] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/jrp/.cabal/bin"
libdir     = "/home/jrp/.cabal/lib/x86_64-linux-ghc-8.6.4/glambda-1.0.2-inplace"
dynlibdir  = "/home/jrp/.cabal/lib/x86_64-linux-ghc-8.6.4"
datadir    = "/home/jrp/.cabal/share/x86_64-linux-ghc-8.6.4/glambda-1.0.2"
libexecdir = "/home/jrp/.cabal/libexec/x86_64-linux-ghc-8.6.4/glambda-1.0.2"
sysconfdir = "/home/jrp/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "glambda_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "glambda_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "glambda_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "glambda_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "glambda_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "glambda_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
