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
version = Version [0,1,2,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\karas\\geometry\\haskell\\geometry\\.stack-work\\install\\b56a5b61\\bin"
libdir     = "C:\\Users\\karas\\geometry\\haskell\\geometry\\.stack-work\\install\\b56a5b61\\lib\\x86_64-windows-ghc-8.8.3\\geometry-0.1.2.0-G0xTfCsTmmOCIGCDN4B9nE"
dynlibdir  = "C:\\Users\\karas\\geometry\\haskell\\geometry\\.stack-work\\install\\b56a5b61\\lib\\x86_64-windows-ghc-8.8.3"
datadir    = "C:\\Users\\karas\\geometry\\haskell\\geometry\\.stack-work\\install\\b56a5b61\\share\\x86_64-windows-ghc-8.8.3\\geometry-0.1.2.0"
libexecdir = "C:\\Users\\karas\\geometry\\haskell\\geometry\\.stack-work\\install\\b56a5b61\\libexec\\x86_64-windows-ghc-8.8.3\\geometry-0.1.2.0"
sysconfdir = "C:\\Users\\karas\\geometry\\haskell\\geometry\\.stack-work\\install\\b56a5b61\\etc"

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
  return (dir ++ "\\" ++ name)
