-- Copyright (C) 2002-2005,2007 David Roundy
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

{-# LANGUAGE MultiParamTypeClasses #-}

module Darcs.Repository.ApplyPatches
    ( runTolerantly
    , runSilently
    , DefaultIO, runDefault
    ) where

import Control.Exception ( IOException, SomeException, catch )
import Control.Monad ( unless )
import Control.Monad.Catch ( MonadThrow )
import qualified Data.ByteString as B ( empty, null, readFile )
import Data.Char ( toLower )
import Data.List ( isSuffixOf )
import System.Directory
    ( createDirectory
    , doesDirectoryExist
    , doesFileExist
    , removeDirectory
    , removeFile
    , renamePath
    )
import System.IO ( hPutStrLn, stderr )
import System.IO.Error
    ( catchIOError
    , isAlreadyExistsError
    , isDoesNotExistError
    , isPermissionError
    )

import Darcs.Prelude

import Darcs.Patch.ApplyMonad ( ApplyMonad(..), ApplyMonadTree(..) )
import Darcs.Repository.Prefs ( changePrefval )
import Darcs.Util.Exception ( prettyException )
import Darcs.Util.File ( backupByCopying, backupByRenaming )
import Darcs.Util.Lock ( writeAtomicFilePS )
import Darcs.Util.Path ( AnchoredPath, realPath )
import Darcs.Util.Tree ( Tree )

newtype DefaultIO a = DefaultIO { runDefaultIO :: IO a }
    deriving (Functor, Applicative, Monad, MonadThrow)

instance ApplyMonad Tree DefaultIO where
    readFilePS path = mReadFilePS path

instance ApplyMonadTree DefaultIO where
    mDoesFileExist = DefaultIO . doesFileExist . realPath
    mDoesDirectoryExist = DefaultIO . doesDirectoryExist . realPath
    mChangePref a b c = DefaultIO $ changePrefval a b c
    mReadFilePS = DefaultIO . B.readFile . realPath
    mModifyFilePS f j = DefaultIO $ B.readFile (realPath f) >>= runDefaultIO . j >>= writeAtomicFilePS (realPath f)
    mCreateDirectory = DefaultIO . createDirectory . realPath
    mCreateFile f = DefaultIO $
                    do exf <- doesFileExist (realPath f)
                       if exf then fail $ "File '"++realPath f++"' already exists!"
                              else do exd <- doesDirectoryExist $ realPath f
                                      if exd then fail $ "File '"++realPath f++"' already exists!"
                                             else writeAtomicFilePS (realPath f) B.empty
    mRemoveFile f = DefaultIO $
                    do let fp = realPath f
                       x <- B.readFile fp
                       unless (B.null x) $
                            fail $ "Cannot remove non-empty file "++fp
                       removeFile fp
    mRemoveDirectory = DefaultIO . removeDirectory . realPath
    mRename a b = DefaultIO $ renamePath x y
      where x = realPath a
            y = realPath b

class (Functor m, MonadThrow m) => TolerantMonad m where
    warning :: IO () -> m ()
    runIO :: m a -> IO a
    runTM :: IO a -> m a

newtype TolerantIO a = TIO { runTIO :: IO a }
    deriving (Functor, Applicative, Monad, MonadThrow)

instance TolerantMonad TolerantIO where
    warning io = TIO $ io `catch` \e -> hPutStrLn stderr $ "Warning: " ++ prettyException e
    runIO (TIO io) = io
    runTM = TIO

newtype SilentIO a = SIO { runSIO :: IO a }
    deriving (Functor, Applicative, Monad, MonadThrow)

instance TolerantMonad SilentIO where
    warning io = SIO $ io `catch` \(_ :: SomeException) -> return ()
    runIO (SIO io) = io
    runTM = SIO

newtype TolerantWrapper m a = TolerantWrapper { runTolerantWrapper :: m a }
    deriving (Functor, Applicative, Monad, TolerantMonad)

deriving instance MonadThrow m => MonadThrow (TolerantWrapper m)

-- | Apply patches, emitting warnings if there are any IO errors
runTolerantly :: TolerantWrapper TolerantIO a -> IO a
runTolerantly = runTIO . runTolerantWrapper

-- | Apply patches, ignoring all errors
runSilently :: TolerantWrapper SilentIO a -> IO a
runSilently = runSIO . runTolerantWrapper

-- | The default mode of applying patches: fail if the directory is not
-- as we expect
runDefault :: DefaultIO a -> IO a
runDefault action =
  catchIOError (runDefaultIO action) $ \e ->
    fail $ "Cannot apply some patch:\n"++show e++
      "\nYou may want to run 'darcs check' to find out if there are broken"++
      "\npatches in your repo, and perhaps 'darcs repair' to fix them."

instance TolerantMonad m => ApplyMonad Tree (TolerantWrapper m) where
    readFilePS path = mReadFilePS path

instance TolerantMonad m => ApplyMonadTree (TolerantWrapper m) where
    mDoesFileExist = runTM . runDefaultIO . mDoesFileExist
    mDoesDirectoryExist d = runTM $ runDefaultIO $ mDoesDirectoryExist d
    mReadFilePS f = runTM $ runDefaultIO $ mReadFilePS f
    mChangePref a b c = warning $ runDefaultIO $ mChangePref a b c
    mModifyFilePS f j = warning $ runDefaultIO $ mModifyFilePS f (DefaultIO . runIO . j)
    mCreateFile f = warning $ backup f >> runDefaultIO (mCreateFile f)
    mCreateDirectory d = warning $ backup d >> runDefaultIO (mCreateDirectory d)
    mRemoveFile f = warning $ runDefaultIO (mRemoveFile f)
    mRemoveDirectory d = warning $ catch
                                 (runDefaultIO (mRemoveDirectory d))
                                 (\(e :: IOException) ->
                                   if "(Directory not empty)" `isSuffixOf` show e
                                   then ioError $ userError $
                                            "Not deleting " ++ realPath d ++ " because it is not empty."
                                   else ioError $ userError $
                                            "Not deleting " ++ realPath d ++ " because:\n" ++ show e)
    mRename a b = warning $ catch
                          (let do_backup = if map toLower x == map toLower y
                                           then backupByCopying (realPath b) -- avoid making the original vanish
                                           else backupByRenaming (realPath b)
                           in do_backup >> runDefaultIO (mRename a b))
                          (\e -> case () of
                                 _ | isPermissionError e -> ioError $ userError $
                                       couldNotRename ++ "."
                                   | isDoesNotExistError e -> ioError $ userError $
                                       couldNotRename ++ " because " ++ x ++ " does not exist."
                                   | isAlreadyExistsError e -> ioError $ userError $
                                       couldNotRename ++ " because " ++ y ++ " already exists."
                                   | otherwise -> ioError e
                          )
       where
        x = realPath a
        y = realPath b
        couldNotRename = "Could not rename " ++ x ++ " to " ++ y

backup :: AnchoredPath -> IO ()
backup f = backupByRenaming (realPath f)
