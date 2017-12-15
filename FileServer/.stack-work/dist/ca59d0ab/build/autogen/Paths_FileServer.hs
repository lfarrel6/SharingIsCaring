{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_FileServer (
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

bindir     = "C:\\Users\\Rory\\Desktop\\Haskell\\FileServerV2\\FileServer\\.stack-work\\install\\1e01afab\\bin"
libdir     = "C:\\Users\\Rory\\Desktop\\Haskell\\FileServerV2\\FileServer\\.stack-work\\install\\1e01afab\\lib\\x86_64-windows-ghc-8.0.2\\FileServer-0.1.0.0-LSdxTuyTfMXL09wHtZIXxn"
dynlibdir  = "C:\\Users\\Rory\\Desktop\\Haskell\\FileServerV2\\FileServer\\.stack-work\\install\\1e01afab\\lib\\x86_64-windows-ghc-8.0.2"
datadir    = "C:\\Users\\Rory\\Desktop\\Haskell\\FileServerV2\\FileServer\\.stack-work\\install\\1e01afab\\share\\x86_64-windows-ghc-8.0.2\\FileServer-0.1.0.0"
libexecdir = "C:\\Users\\Rory\\Desktop\\Haskell\\FileServerV2\\FileServer\\.stack-work\\install\\1e01afab\\libexec"
sysconfdir = "C:\\Users\\Rory\\Desktop\\Haskell\\FileServerV2\\FileServer\\.stack-work\\install\\1e01afab\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "FileServer_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "FileServer_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "FileServer_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "FileServer_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "FileServer_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "FileServer_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
