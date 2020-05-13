{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_geometry (
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

bindir     = "/home/sergey/.cabal/bin"
libdir     = "/home/sergey/.cabal/lib/x86_64-linux-ghc-8.6.4/geometry-0.1.0.0-3c4Fn5u7T1F8ANYk48Wjmo-geometry"
dynlibdir  = "/home/sergey/.cabal/lib/x86_64-linux-ghc-8.6.4"
datadir    = "/home/sergey/.cabal/share/x86_64-linux-ghc-8.6.4/geometry-0.1.0.0"
libexecdir = "/home/sergey/.cabal/libexec/x86_64-linux-ghc-8.6.4/geometry-0.1.0.0"
sysconfdir = "/home/sergey/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "geometry_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "geometry_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "geometry_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "geometry_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "geometry_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "geometry_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
