module Paths_mon (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/jacobbaumbach/Desktop/Haskell/monoid/.stack-work/install/x86_64-osx/lts-6.16/7.10.3/bin"
libdir     = "/Users/jacobbaumbach/Desktop/Haskell/monoid/.stack-work/install/x86_64-osx/lts-6.16/7.10.3/lib/x86_64-osx-ghc-7.10.3/mon-0.1.0.0-J8mKw0VvtKWCyxLvCbywqx"
datadir    = "/Users/jacobbaumbach/Desktop/Haskell/monoid/.stack-work/install/x86_64-osx/lts-6.16/7.10.3/share/x86_64-osx-ghc-7.10.3/mon-0.1.0.0"
libexecdir = "/Users/jacobbaumbach/Desktop/Haskell/monoid/.stack-work/install/x86_64-osx/lts-6.16/7.10.3/libexec"
sysconfdir = "/Users/jacobbaumbach/Desktop/Haskell/monoid/.stack-work/install/x86_64-osx/lts-6.16/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "mon_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "mon_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "mon_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "mon_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "mon_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
