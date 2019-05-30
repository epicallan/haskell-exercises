{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_exercise11 (
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
version = Version [0,0,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/allan/apps/haskell-exercises/11-Singletons/.stack-work/install/x86_64-osx/lts-13.23/8.6.5/bin"
libdir     = "/Users/allan/apps/haskell-exercises/11-Singletons/.stack-work/install/x86_64-osx/lts-13.23/8.6.5/lib/x86_64-osx-ghc-8.6.5/exercise11-0.0.1-3SjySgEIOBO56ChexbuSgG"
dynlibdir  = "/Users/allan/apps/haskell-exercises/11-Singletons/.stack-work/install/x86_64-osx/lts-13.23/8.6.5/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/allan/apps/haskell-exercises/11-Singletons/.stack-work/install/x86_64-osx/lts-13.23/8.6.5/share/x86_64-osx-ghc-8.6.5/exercise11-0.0.1"
libexecdir = "/Users/allan/apps/haskell-exercises/11-Singletons/.stack-work/install/x86_64-osx/lts-13.23/8.6.5/libexec/x86_64-osx-ghc-8.6.5/exercise11-0.0.1"
sysconfdir = "/Users/allan/apps/haskell-exercises/11-Singletons/.stack-work/install/x86_64-osx/lts-13.23/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "exercise11_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "exercise11_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "exercise11_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "exercise11_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "exercise11_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "exercise11_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
