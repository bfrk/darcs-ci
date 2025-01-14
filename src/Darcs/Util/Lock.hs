-- Copyright (C) 2003 David Roundy
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; see the file COPYING.  If not, write to
-- the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
-- Boston, MA 02110-1301, USA.

module Darcs.Util.Lock
    ( withLock
    , withLockCanFail
    , environmentHelpLocks
    , withTempDir
    , withPermDir
    , withDelayedDir
    , withNamedTemp
    , writeBinFile
    , writeTextFile
    , writeDocBinFile
    , appendDocBinFile
    , readBinFile
    , readTextFile
    , readDocBinFile
    , writeAtomicFilePS
    , gzWriteAtomicFilePS
    , gzWriteAtomicFilePSs
    , gzWriteDocFile
    , removeFileMayNotExist
    , maybeRelink
    , tempdirLoc
    , environmentHelpTmpdir
    , environmentHelpKeepTmpdir
    , addToErrorLoc
    , withNewDirectory
    ) where

import Darcs.Prelude

import Data.List ( inits )
import Data.Maybe ( fromJust, isJust, listToMaybe )
import System.Exit ( exitWith, ExitCode(..) )
import System.IO
    ( withFile, withBinaryFile
    , Handle, hPutStr, hSetEncoding
    , IOMode(WriteMode, AppendMode), hFlush, stdout
    )
import System.IO.Error
    ( isAlreadyExistsError
    , annotateIOError
    , catchIOError
    )
import Control.Exception
    ( IOException
    , bracket
    , throwIO
    , catch
    , SomeException
    )
import System.Directory
    ( doesFileExist
    , doesDirectoryExist
    , createDirectory
    , getTemporaryDirectory
    , makeAbsolute
    , removePathForcibly
    , renameFile
    , renameDirectory
    )
import System.FilePath.Posix ( splitDirectories, splitFileName )
import System.Directory ( withCurrentDirectory )
import System.Environment ( lookupEnv )
import System.IO.Temp ( createTempDirectory )

import Control.Concurrent ( threadDelay )
import Control.Monad ( unless, when )

import System.Posix.Files ( fileMode, getFileStatus, setFileMode )

import GHC.IO.Encoding ( getFileSystemEncoding )

import Safe ( headErr )

import Darcs.Util.URL ( isRelative )
import Darcs.Util.Exception
    ( firstJustIO
    , catchall
    )
import Darcs.Util.File ( removeFileMayNotExist )
import Darcs.Util.Path ( AbsolutePath, FilePathLike, toFilePath,
                        getCurrentDirectory, setCurrentDirectory )

import Darcs.Util.ByteString ( gzWriteFilePSs )
import qualified Data.ByteString as B (null, readFile, hPut, ByteString)

import Darcs.Util.SignalHandler ( withSignalsBlocked )
import Darcs.Util.Printer ( Doc, hPutDoc, packedString, empty, renderPSs )
import Darcs.Util.AtExit ( atexit )
import Darcs.Util.Global ( darcsdir )
import Darcs.Util.Compat
    ( maybeRelink
    , atomicCreate
    , sloppyAtomicCreate
    )
import Darcs.Util.Progress ( debugMessage )
import Darcs.Util.Prompt ( askUser )

withLock :: String -> IO a -> IO a
withLock s job = bracket (getLock s 30) releaseLock (const job)

releaseLock :: String -> IO ()
releaseLock = removeFileMayNotExist

-- | Tries to perform some task if it can obtain the lock,
-- Otherwise, just gives up without doing the task
withLockCanFail :: String -> IO a -> IO (Either () a)
withLockCanFail s job =
  bracket (takeLock s `catchall` return False)
          (\l -> when l $ releaseLock s)
          (\l -> if l then Right <$> job
                      else return $ Left ())

getLock :: String -> Int -> IO String
getLock l 0 = do yorn <- askUser $ "Couldn't get lock "++l++". Abort (yes or anything else)? "
                 case yorn of
                    ('y':_) -> exitWith $ ExitFailure 1
                    _ -> getLock l 30
getLock lbad tl = do l <- makeAbsolute lbad
                     gotit <- takeLock l
                     if gotit then return l
                              else do putStrLn $ "Waiting for lock "++l
                                      hFlush stdout -- for Windows
                                      threadDelay 2000000
                                      getLock l (tl - 1)


takeLock :: FilePathLike p => p -> IO Bool
takeLock fp =
    do atomicCreate $ toFilePath fp
       return True
  `catch` \e -> if isAlreadyExistsError e
                then return False
                else do pwd <- getCurrentDirectory
                        throwIO $ addToErrorLoc e
                                   ("takeLock "++toFilePath fp++" in "++toFilePath pwd)

takeFile :: FilePath -> IO Bool
takeFile fp =
    do sloppyAtomicCreate fp
       return True
  `catch` \e -> if isAlreadyExistsError e
                then return False
                else do pwd <- getCurrentDirectory
                        throwIO $ addToErrorLoc e
                                   ("takeFile "++fp++" in "++toFilePath pwd)

environmentHelpLocks :: ([String],[String])
environmentHelpLocks = (["DARCS_SLOPPY_LOCKS"],[
 "If on some filesystems you get an error of the kind:",
 "",
 "    darcs: takeLock [...]: atomic_create [...]: unsupported operation",
 "",
 "you may want to try to export DARCS_SLOPPY_LOCKS=True."])

tempdirLoc :: IO FilePath
tempdirLoc = fromJust <$>
    firstJustIO [ fmap (Just . headErr . words) (readFile (darcsdir++"/prefs/tmpdir")) >>= chkdir,
                  lookupEnv "DARCS_TMPDIR" >>= chkdir,
                  getTemporaryDirectory >>= chkdir . Just,
                  getCurrentDirectorySansDarcs,
                  return $ Just "."  -- always returns a Just
                ]
    where chkdir Nothing = return Nothing
          chkdir (Just d) = (\e -> if e then Just (d++"/") else Nothing) <$> doesDirectoryExist d

environmentHelpTmpdir :: ([String], [String])
environmentHelpTmpdir = (["DARCS_TMPDIR", "TMPDIR"], [
 "Darcs often creates temporary directories.  For example, the `darcs",
 "diff` command creates two for the working trees to be diffed.  By",
 "default temporary directories are created in /tmp, or if that doesn't",
 "exist, in _darcs (within the current repo).  This can be overridden by",
 "specifying some other directory in the file _darcs/prefs/tmpdir or the",
 "environment variable $DARCS_TMPDIR or $TMPDIR."])

getCurrentDirectorySansDarcs :: IO (Maybe FilePath)
getCurrentDirectorySansDarcs = do
  c <- getCurrentDirectory
  return $ listToMaybe $ drop 5 $ reverse $ takeWhile no_darcs $ inits $ toFilePath c
  where no_darcs x = darcsdir `notElem` splitDirectories x

data WithDirKind = Perm | Temp | Delayed

-- | Creates a directory based on the path parameter;
-- if a relative path is given the dir is created in the darcs temp dir.
-- If an absolute path is given this dir will be created if it doesn't exist.
-- If it is specified as a temporary dir, it is deleted after finishing the job.
withDir :: WithDirKind  -- specifies if and when directory will be deleted
        -> FilePath     -- path parameter
        -> (AbsolutePath -> IO a) -> IO a
withDir _ "" _ = error "withDir called with empty directory name"
withDir kind absoluteOrRelativeName job = do
  absoluteName <- if isRelative absoluteOrRelativeName
                   then fmap (++ absoluteOrRelativeName) tempdirLoc
                   else return absoluteOrRelativeName
  formerdir <- getCurrentDirectory
  bracket (createDir absoluteName)
          (\dir -> do
              setCurrentDirectory formerdir
              k <- keepTempDir
              unless k $
                  case kind of
                      Perm -> return ()
                      Temp -> cleanup (toFilePath dir)
                      Delayed -> atexit $ cleanup (toFilePath dir))
          job
    where createDir :: FilePath -> IO AbsolutePath
          createDir name
              = do let (parent,dir) = splitFileName name
                   createTempDirectory parent dir >>= setCurrentDirectory
                   getCurrentDirectory
          keepTempDir = isJust `fmap` lookupEnv "DARCS_KEEP_TMPDIR"
          toDelete dir = dir ++ "_done"
          cleanup path = do
              -- so asynchronous threads cannot add any more
              -- files while we are deleting
              debugMessage $ unwords ["atexit: renaming",path,"to",toDelete path]
              renameDirectory path (toDelete path)
              debugMessage $ unwords ["atexit: deleting",toDelete path]
              removePathForcibly (toDelete path) `catchIOError` const (return ())

environmentHelpKeepTmpdir :: ([String], [String])
environmentHelpKeepTmpdir = (["DARCS_KEEP_TMPDIR"],[
 "If the environment variable DARCS_KEEP_TMPDIR is defined, darcs will",
 "not remove the temporary directories it creates.  This is intended",
 "primarily for debugging Darcs itself, but it can also be useful, for",
 "example, to determine why your test preference (see `darcs setpref`)",
 "is failing when you run `darcs record`, but working when run manually."])

-- |'withPermDir' is like 'withTempDir', except that it doesn't
-- delete the directory afterwards.
withPermDir :: FilePath -> (AbsolutePath -> IO a) -> IO a
withPermDir = withDir Perm

-- |'withTempDir' creates a temporary directory, runs the action and then
-- removes the directory. The
-- location of that directory is determined by the contents of
-- _darcs/prefs/tmpdir, if it exists, otherwise by @$DARCS_TMPDIR@, and if
-- that doesn't exist then whatever your operating system considers to be a
-- a temporary directory (e.g. @$TMPDIR@ under Unix, @$TEMP@ under
-- Windows).
--
-- If none of those exist it creates the temporary directory
-- in the current directory, unless the current directory is under a _darcs
-- directory, in which case the temporary directory in the parent of the highest
-- _darcs directory to avoid accidentally corrupting darcs's internals.
-- This should not fail, but if it does indeed fail, we go ahead and use the
-- current directory anyway. If @$DARCS_KEEP_TMPDIR@ variable is set
-- temporary directory is not removed, this can be useful for debugging.
withTempDir :: FilePath -> (AbsolutePath -> IO a) -> IO a
withTempDir = withDir Temp

withDelayedDir :: FilePath -> (AbsolutePath -> IO a) -> IO a
withDelayedDir = withDir Delayed

worldReadableTemp :: FilePath -> IO FilePath
worldReadableTemp f = wrt 0
    where wrt :: Int -> IO FilePath
          wrt 100 = fail $ "Failure creating temp named "++f
          wrt n = let f_new = f++"-"++show n
                  in do ok <- takeFile f_new
                        if ok then return f_new
                              else wrt (n+1)

withNamedTemp :: FilePath -> (FilePath -> IO a) -> IO a
withNamedTemp n f = do
    when False $ debugMessage $ "withNamedTemp: " ++ show n
    bracket (worldReadableTemp n) removeFileMayNotExist f

readBinFile :: FilePathLike p => p -> IO B.ByteString
readBinFile = B.readFile . toFilePath

-- NOTE using 'seq' on the last element of the result causes the content to be
-- fully evaluated, so the file is read strictly; this is more efficient than
-- counting the number of characters; and in the (few) places where we use this
-- function we need the lines anyway.
readTextFile :: FilePathLike p => p -> IO [String]
readTextFile f = do
  result <- lines <$> readFile (toFilePath f)
  case result of
    [] -> return result
    xs -> last xs `seq` return result

readDocBinFile :: FilePathLike p => p -> IO Doc
readDocBinFile fp = do ps <- B.readFile $ toFilePath fp
                       return $ if B.null ps then empty else packedString ps

appendDocBinFile :: FilePathLike p => p -> Doc -> IO ()
appendDocBinFile f d = appendToFile Binary f $ \h -> hPutDoc h d

data FileType = Text | Binary

writeBinFile :: FilePathLike p => p -> B.ByteString -> IO ()
writeBinFile f s = writeToFile Binary f $ \h -> B.hPut h s

writeTextFile :: FilePathLike p => p -> String -> IO ()
writeTextFile f s = writeToFile Text f $ \h -> do
  getFileSystemEncoding >>= hSetEncoding h
  hPutStr h s

writeDocBinFile :: FilePathLike p => p -> Doc -> IO ()
writeDocBinFile f d = writeToFile Binary f $ \h -> hPutDoc h d

writeAtomicFilePS :: FilePathLike p => p -> B.ByteString -> IO ()
writeAtomicFilePS f ps = writeToFile Binary f $ \h -> B.hPut h ps

gzWriteAtomicFilePS :: FilePathLike p => p -> B.ByteString -> IO ()
gzWriteAtomicFilePS f ps = gzWriteAtomicFilePSs f [ps]

gzWriteAtomicFilePSs :: FilePathLike p => p -> [B.ByteString] -> IO ()
gzWriteAtomicFilePSs f pss =
    withSignalsBlocked $ withNamedTemp (toFilePath f) $ \newf -> do
    gzWriteFilePSs newf pss
    already_exists <- doesFileExist $ toFilePath f
    when already_exists $ do mode <- fileMode `fmap` getFileStatus (toFilePath f)
                             setFileMode newf mode
             `catchall` return ()
    renameFile newf (toFilePath f)

gzWriteDocFile :: FilePathLike p => p -> Doc -> IO ()
gzWriteDocFile f d = gzWriteAtomicFilePSs f $ renderPSs d

writeToFile :: FilePathLike p => FileType -> p -> (Handle -> IO ()) -> IO ()
writeToFile t f job =
    withSignalsBlocked $ withNamedTemp (toFilePath f) $ \newf -> do
    (case t of
      Text -> withFile
      Binary -> withBinaryFile) newf WriteMode job
    already_exists <- doesFileExist (toFilePath f)
    when already_exists $ do mode <- fileMode `fmap` getFileStatus (toFilePath f)
                             setFileMode newf mode
             `catchall` return ()
    renameFile newf (toFilePath f)

appendToFile :: FilePathLike p => FileType -> p -> (Handle -> IO ()) -> IO ()
appendToFile t f job = withSignalsBlocked $
    (case t of
      Binary -> withBinaryFile
      Text -> withFile) (toFilePath f) AppendMode job


addToErrorLoc :: IOException
              -> String
              -> IOException
addToErrorLoc ioe s = annotateIOError ioe s Nothing Nothing

-- | Do an action in a newly created directory of the given name. If the
-- directory is successfully created but the action raises an exception, the
-- directory and all its content is deleted. Caught exceptions are re-thrown.
withNewDirectory :: FilePath -> IO () -> IO ()
withNewDirectory name action = do
  createDirectory name
  withCurrentDirectory name action `catch` \e -> do
    removePathForcibly name `catchIOError` const (return ())
    throwIO (e :: SomeException)
