{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_AdventOfCode2018 (
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

bindir     = "/media/saaffa/Storage/Code/Haskell/AdventOfCode/AdventOfCode2018/.stack-work/install/x86_64-linux/lts-12.20/8.4.4/bin"
libdir     = "/media/saaffa/Storage/Code/Haskell/AdventOfCode/AdventOfCode2018/.stack-work/install/x86_64-linux/lts-12.20/8.4.4/lib/x86_64-linux-ghc-8.4.4/AdventOfCode2018-0.1.0.0-Krgv0rbpzUIac5J6t3Jc8"
dynlibdir  = "/media/saaffa/Storage/Code/Haskell/AdventOfCode/AdventOfCode2018/.stack-work/install/x86_64-linux/lts-12.20/8.4.4/lib/x86_64-linux-ghc-8.4.4"
datadir    = "/media/saaffa/Storage/Code/Haskell/AdventOfCode/AdventOfCode2018/.stack-work/install/x86_64-linux/lts-12.20/8.4.4/share/x86_64-linux-ghc-8.4.4/AdventOfCode2018-0.1.0.0"
libexecdir = "/media/saaffa/Storage/Code/Haskell/AdventOfCode/AdventOfCode2018/.stack-work/install/x86_64-linux/lts-12.20/8.4.4/libexec/x86_64-linux-ghc-8.4.4/AdventOfCode2018-0.1.0.0"
sysconfdir = "/media/saaffa/Storage/Code/Haskell/AdventOfCode/AdventOfCode2018/.stack-work/install/x86_64-linux/lts-12.20/8.4.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "AdventOfCode2018_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "AdventOfCode2018_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "AdventOfCode2018_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "AdventOfCode2018_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "AdventOfCode2018_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "AdventOfCode2018_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
