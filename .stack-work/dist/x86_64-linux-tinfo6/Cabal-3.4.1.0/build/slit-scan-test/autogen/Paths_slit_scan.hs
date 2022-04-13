{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_slit_scan (
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

bindir     = "/Isar/development/haskell/slit-scan/.stack-work/install/x86_64-linux-tinfo6/d8e273812ea2e1ae1c9cbd4fbde958b5e0fdb0fdbb3a24361b019a7a5043006d/9.0.2/bin"
libdir     = "/Isar/development/haskell/slit-scan/.stack-work/install/x86_64-linux-tinfo6/d8e273812ea2e1ae1c9cbd4fbde958b5e0fdb0fdbb3a24361b019a7a5043006d/9.0.2/lib/x86_64-linux-ghc-9.0.2/slit-scan-0.1.0.0-DljyHAlNaAx1hwsZwPqjiA-slit-scan-test"
dynlibdir  = "/Isar/development/haskell/slit-scan/.stack-work/install/x86_64-linux-tinfo6/d8e273812ea2e1ae1c9cbd4fbde958b5e0fdb0fdbb3a24361b019a7a5043006d/9.0.2/lib/x86_64-linux-ghc-9.0.2"
datadir    = "/Isar/development/haskell/slit-scan/.stack-work/install/x86_64-linux-tinfo6/d8e273812ea2e1ae1c9cbd4fbde958b5e0fdb0fdbb3a24361b019a7a5043006d/9.0.2/share/x86_64-linux-ghc-9.0.2/slit-scan-0.1.0.0"
libexecdir = "/Isar/development/haskell/slit-scan/.stack-work/install/x86_64-linux-tinfo6/d8e273812ea2e1ae1c9cbd4fbde958b5e0fdb0fdbb3a24361b019a7a5043006d/9.0.2/libexec/x86_64-linux-ghc-9.0.2/slit-scan-0.1.0.0"
sysconfdir = "/Isar/development/haskell/slit-scan/.stack-work/install/x86_64-linux-tinfo6/d8e273812ea2e1ae1c9cbd4fbde958b5e0fdb0fdbb3a24361b019a7a5043006d/9.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "slit_scan_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "slit_scan_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "slit_scan_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "slit_scan_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "slit_scan_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "slit_scan_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
