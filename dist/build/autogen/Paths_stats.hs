module Paths_stats (
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
version = Version {versionBranch = [1,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/x86_64-linux-ghc-7.6.3/stats-1.0"
datadir    = "/usr/local/share/x86_64-linux-ghc-7.6.3/stats-1.0"
libexecdir = "/usr/local/libexec"
sysconfdir = "/usr/local/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "stats_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "stats_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "stats_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "stats_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "stats_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
