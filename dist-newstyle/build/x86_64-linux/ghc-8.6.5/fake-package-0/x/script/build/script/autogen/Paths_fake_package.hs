{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_fake_package (
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
version = Version [0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/bolt/.cabal/bin"
libdir     = "/home/bolt/.cabal/lib/x86_64-linux-ghc-8.6.5/fake-package-0-inplace-script"
dynlibdir  = "/home/bolt/.cabal/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/bolt/.cabal/share/x86_64-linux-ghc-8.6.5/fake-package-0"
libexecdir = "/home/bolt/.cabal/libexec/x86_64-linux-ghc-8.6.5/fake-package-0"
sysconfdir = "/home/bolt/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "fake_package_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "fake_package_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "fake_package_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "fake_package_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "fake_package_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "fake_package_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
