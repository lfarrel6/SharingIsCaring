{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_FileServerV2 (
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

bindir     = "C:\\Users\\Rory\\Desktop\\Haskell\\FileServerV2\\.stack-work\\install\\3573363d\\bin"
libdir     = "C:\\Users\\Rory\\Desktop\\Haskell\\FileServerV2\\.stack-work\\install\\3573363d\\lib\\x86_64-windows-ghc-8.0.2\\FileServerV2-0.1.0.0-3sOZFy9Sb6rIPqFM3ZvKEk"
dynlibdir  = "C:\\Users\\Rory\\Desktop\\Haskell\\FileServerV2\\.stack-work\\install\\3573363d\\lib\\x86_64-windows-ghc-8.0.2"
datadir    = "C:\\Users\\Rory\\Desktop\\Haskell\\FileServerV2\\.stack-work\\install\\3573363d\\share\\x86_64-windows-ghc-8.0.2\\FileServerV2-0.1.0.0"
libexecdir = "C:\\Users\\Rory\\Desktop\\Haskell\\FileServerV2\\.stack-work\\install\\3573363d\\libexec"
sysconfdir = "C:\\Users\\Rory\\Desktop\\Haskell\\FileServerV2\\.stack-work\\install\\3573363d\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "FileServerV2_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "FileServerV2_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "FileServerV2_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "FileServerV2_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "FileServerV2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "FileServerV2_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
