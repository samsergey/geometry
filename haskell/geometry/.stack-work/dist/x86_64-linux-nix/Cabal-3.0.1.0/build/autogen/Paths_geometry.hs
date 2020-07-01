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

bindir     = "/home/sergey/work/webinar/webfigs/haskell/geometry/.stack-work/install/x86_64-linux-nix/b34d52e73bf06c6df136196c2603e7456f87d90c59d9d570bc0959ec2c5dbb81/8.8.3/bin"
libdir     = "/home/sergey/work/webinar/webfigs/haskell/geometry/.stack-work/install/x86_64-linux-nix/b34d52e73bf06c6df136196c2603e7456f87d90c59d9d570bc0959ec2c5dbb81/8.8.3/lib/x86_64-linux-ghc-8.8.3/geometry-0.1.2.0-3CTFcSZW00g4K978Q4vX2H"
dynlibdir  = "/home/sergey/work/webinar/webfigs/haskell/geometry/.stack-work/install/x86_64-linux-nix/b34d52e73bf06c6df136196c2603e7456f87d90c59d9d570bc0959ec2c5dbb81/8.8.3/lib/x86_64-linux-ghc-8.8.3"
datadir    = "/home/sergey/work/webinar/webfigs/haskell/geometry/.stack-work/install/x86_64-linux-nix/b34d52e73bf06c6df136196c2603e7456f87d90c59d9d570bc0959ec2c5dbb81/8.8.3/share/x86_64-linux-ghc-8.8.3/geometry-0.1.2.0"
libexecdir = "/home/sergey/work/webinar/webfigs/haskell/geometry/.stack-work/install/x86_64-linux-nix/b34d52e73bf06c6df136196c2603e7456f87d90c59d9d570bc0959ec2c5dbb81/8.8.3/libexec/x86_64-linux-ghc-8.8.3/geometry-0.1.2.0"
sysconfdir = "/home/sergey/work/webinar/webfigs/haskell/geometry/.stack-work/install/x86_64-linux-nix/b34d52e73bf06c6df136196c2603e7456f87d90c59d9d570bc0959ec2c5dbb81/8.8.3/etc"

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
