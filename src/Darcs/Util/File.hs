module Darcs.Util.File
    (
    -- * Files and directories
      getFileStatus
    , withCurrentDirectory
    , doesDirectoryReallyExist
    , removeFileMayNotExist
    , getRecursiveContents
    , getRecursiveContentsFullPath
    -- * OS-dependent special directories
    , xdgCacheDir
    , osxCacheDir
    ) where

import Darcs.Prelude

import Control.Exception ( bracket )
import Control.Monad ( when, unless, forM )

import Data.List ( lookup )

import GHC.Stack ( HasCallStack )

import System.Environment ( getEnvironment )
import System.Directory ( removeFile, getHomeDirectory,
                          getAppUserDataDirectory, doesDirectoryExist,
                          createDirectory, listDirectory )
import System.IO.Error ( catchIOError )
import System.Posix.Files( getSymbolicLinkStatus, FileStatus, isDirectory )
import System.Posix.Files( setFileMode, ownerModes )
import System.FilePath.Posix ( (</>) )

import Darcs.Util.Exception ( catchall, ifDoesNotExistError )
import Darcs.Util.Path( FilePathLike, getCurrentDirectory, setCurrentDirectory, toFilePath )

withCurrentDirectory :: (HasCallStack, FilePathLike p)
                     => p
                     -> IO a
                     -> IO a
withCurrentDirectory name m =
    bracket
        (do cwd <- getCurrentDirectory
            when (toFilePath name /= "") (setCurrentDirectory name)
            return cwd)
        (\oldwd -> setCurrentDirectory oldwd `catchall` return ())
        (const m)

getFileStatus :: FilePath -> IO (Maybe FileStatus)
getFileStatus f =
  Just `fmap` getSymbolicLinkStatus f `catchIOError` (\_-> return Nothing)

doesDirectoryReallyExist :: FilePath -> IO Bool
doesDirectoryReallyExist f =
    ifDoesNotExistError False (isDirectory `fmap` getSymbolicLinkStatus f)

removeFileMayNotExist :: FilePathLike p => p -> IO ()
removeFileMayNotExist f = ifDoesNotExistError () (removeFile $ toFilePath f)

-- |osxCacheDir assumes @~/Library/Caches/@ exists.
osxCacheDir :: IO (Maybe FilePath)
osxCacheDir = do
    home <- getHomeDirectory
    return $ Just $ home </> "Library" </> "Caches"
    `catchall` return Nothing

-- |xdgCacheDir returns the $XDG_CACHE_HOME environment variable,
-- or @~/.cache@ if undefined. See the FreeDesktop specification:
-- http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html
xdgCacheDir :: IO (Maybe FilePath)
xdgCacheDir = do
    env <- getEnvironment
    d <- case lookup "XDG_CACHE_HOME" env of
           Just d  -> return d
           Nothing -> getAppUserDataDirectory "cache"
    exists <- doesDirectoryExist d

    -- If directory does not exist, create it with permissions 0700
    -- as specified by the FreeDesktop standard.
    unless exists $ do createDirectory d
                       setFileMode d ownerModes
    return $ Just d
    `catchall` return Nothing

-- |getRecursiveContents returns all files under topdir that aren't
-- directories.
getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  entries <- listDirectory topdir
  paths <- forM entries $ \name -> do
    let path = topdir </> name
    isDir <- doesDirectoryExist path
    if isDir
      then getRecursiveContents path
      else return [name]
  return (concat paths)

-- |getRecursiveContentsFullPath returns all files under topdir
-- that aren't directories.
-- Unlike getRecursiveContents this function returns the full path.
getRecursiveContentsFullPath :: FilePath -> IO [FilePath]
getRecursiveContentsFullPath topdir = do
  entries <- listDirectory topdir
  paths <- forM entries $ \name -> do
    let path = topdir </> name
    isDir <- doesDirectoryExist path
    if isDir
      then getRecursiveContentsFullPath path
      else return [path]
  return (concat paths)
