{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_DirectoryServer (
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

bindir     = "C:\\Users\\Rory\\Desktop\\Haskell\\FileServerV2\\DirectoryServer\\.stack-work\\install\\1e01afab\\bin"
libdir     = "C:\\Users\\Rory\\Desktop\\Haskell\\FileServerV2\\DirectoryServer\\.stack-work\\install\\1e01afab\\lib\\x86_64-windows-ghc-8.0.2\\DirectoryServer-0.1.0.0-GGjLyrMJzlZ266JlofVYF2"
dynlibdir  = "C:\\Users\\Rory\\Desktop\\Haskell\\FileServerV2\\DirectoryServer\\.stack-work\\install\\1e01afab\\lib\\x86_64-windows-ghc-8.0.2"
datadir    = "C:\\Users\\Rory\\Desktop\\Haskell\\FileServerV2\\DirectoryServer\\.stack-work\\install\\1e01afab\\share\\x86_64-windows-ghc-8.0.2\\DirectoryServer-0.1.0.0"
libexecdir = "C:\\Users\\Rory\\Desktop\\Haskell\\FileServerV2\\DirectoryServer\\.stack-work\\install\\1e01afab\\libexec"
sysconfdir = "C:\\Users\\Rory\\Desktop\\Haskell\\FileServerV2\\DirectoryServer\\.stack-work\\install\\1e01afab\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "DirectoryServer_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "DirectoryServer_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "DirectoryServer_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "DirectoryServer_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "DirectoryServer_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "DirectoryServer_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
