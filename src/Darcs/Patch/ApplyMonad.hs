{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
-- Copyright (C) 2010, 2011 Petr Rockai
--
-- Permission is hereby granted, free of charge, to any person
-- obtaining a copy of this software and associated documentation
-- files (the "Software"), to deal in the Software without
-- restriction, including without limitation the rights to use, copy,
-- modify, merge, publish, distribute, sublicense, and/or sell copies
-- of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
-- NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
-- BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
-- ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
-- CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.
module Darcs.Patch.ApplyMonad
  ( ApplyMonad(..), ApplyMonadTrans(..), ApplyMonadOperations
  , withFileNames
  , ApplyMonadTree(..)
  , evalApplyMonad
  ) where

import Darcs.Prelude

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import qualified Darcs.Util.Tree.Monad as TM
import Darcs.Patch.Object ( ObjectIdOf )
import Darcs.Util.StrictIdentity (StrictIdentity(..) )
import Darcs.Util.Tree ( Tree )
import Data.Maybe ( fromMaybe )
import Darcs.Util.Path ( AnchoredPath, movedirfilename, isPrefix )
import Control.Monad.Catch ( MonadThrow(..) )
import Control.Monad.State.Strict

import GHC.Exts ( Constraint )

class (Monad m, ApplyMonad state (ApplyMonadOver state m))
      => ApplyMonadTrans state m where
  type ApplyMonadOver state m :: Type -> Type
  runApplyMonad :: (ApplyMonadOver state m) x -> state m -> m (x, state m)

instance MonadThrow m => ApplyMonadTrans Tree m where
  type ApplyMonadOver Tree m = TM.TreeMonad m
  runApplyMonad = TM.virtualTreeMonad

evalApplyMonad
  :: ApplyMonadTrans state m => ApplyMonadOver state m a -> state m -> m a
evalApplyMonad action st = fst <$> runApplyMonad action st

type family ApplyMonadOperations (state :: (Type -> Type) -> Type) :: (Type -> Type) -> Constraint

class MonadThrow m => ApplyMonadTree m where
    -- a semantic, Tree-based interface for patch application
    mDoesDirectoryExist ::  AnchoredPath -> m Bool
    mDoesFileExist ::  AnchoredPath -> m Bool
    mReadFilePS ::  AnchoredPath -> m B.ByteString
    mCreateDirectory ::  AnchoredPath -> m ()
    mRemoveDirectory ::  AnchoredPath -> m ()
    mCreateFile ::  AnchoredPath -> m ()
    mRemoveFile ::  AnchoredPath -> m ()
    mRename ::  AnchoredPath -> AnchoredPath -> m ()
    mModifyFilePS ::  AnchoredPath -> (B.ByteString -> m B.ByteString) -> m ()
    mChangePref ::  String -> String -> String -> m ()
    mChangePref _ _ _ = return ()

type instance ApplyMonadOperations Tree = ApplyMonadTree

class ( Monad m
      , ApplyMonadOperations state m
      )
      => ApplyMonad (state :: (Type -> Type) -> Type) m | m -> state where

    readFilePS :: ObjectIdOf state -> m B.ByteString

instance MonadThrow m => ApplyMonad Tree (TM.TreeMonad m) where
    readFilePS path = mReadFilePS path

instance MonadThrow m => ApplyMonadTree (TM.TreeMonad m) where
    mDoesDirectoryExist p = TM.directoryExists p
    mDoesFileExist p = TM.fileExists p
    mReadFilePS p = BL.toStrict <$> TM.readFile p
    mModifyFilePS p j =
      TM.writeFile p . BL.fromStrict =<< j . BL.toStrict =<< TM.readFile p
    mCreateFile p = TM.writeFile p BL.empty
    mCreateDirectory p = TM.createDirectory p
    mRename from to = TM.rename from to
    mRemoveDirectory = TM.unlink
    mRemoveFile = TM.unlink

-- Latest name, current original name.
type OrigFileNameOf = (AnchoredPath, AnchoredPath)
-- Touched files, new file list (after removes etc.) and rename details
type FilePathMonadState = ([AnchoredPath], [AnchoredPath], [OrigFileNameOf])
type FilePathMonad = StateT FilePathMonadState Pure

newtype Pure a = Pure (StrictIdentity a)
  deriving (Functor, Applicative, Monad)

runPure :: Pure a -> a
runPure (Pure (StrictIdentity x)) = x

-- With "Data.Functor.Identity" this instance would not satisfy the law
-- @throwM e >> x = throwM e@, which is why we use 'StrictIdentity'.
instance MonadThrow Pure where
  throwM e = Pure (error (show e))

-- |trackOrigRename takes an old and new name and attempts to apply the mapping
-- to the OrigFileNameOf pair. If the old name is the most up-to-date name of
-- the file in question, the first element of the OFNO will match, otherwise if
-- the up-to-date name was originally old, the second element will match.
trackOrigRename :: AnchoredPath -> AnchoredPath -> OrigFileNameOf -> OrigFileNameOf
trackOrigRename old new pair@(latest, from)
    | old `isPrefix` latest = (latest, movedirfilename old new latest)
    | old `isPrefix` from = (latest, movedirfilename old new from)
    | otherwise = pair

-- |withFileNames takes a maybe list of existing rename-pairs, a list of
-- filenames and an action, and returns the resulting triple of affected files,
-- updated filename list and new rename details. If the rename-pairs are not
-- present, a new list is generated from the filesnames.
withFileNames :: Maybe [OrigFileNameOf] -> [AnchoredPath] -> FilePathMonad a
    -> FilePathMonadState
withFileNames mbofnos fps x = runPure $ execStateT x ([], fps, ofnos)
  where
    ofnos = fromMaybe (map (\y -> (y, y)) fps) mbofnos

instance ApplyMonad Tree FilePathMonad where
    readFilePS = error "readFilePS not defined for FilePathMonad"

instance ApplyMonadTree FilePathMonad where
    -- We can't check it actually is a directory here
    mDoesDirectoryExist p = gets $ \(_, fs, _) -> p `elem` fs
    mDoesFileExist = mDoesDirectoryExist
    mCreateDirectory = mCreateFile
    mCreateFile f = modify $ \(ms, fs, rns) -> (f : ms, fs, rns)
    mRemoveFile f = modify $ \(ms, fs, rns) -> (f : ms, filter (/= f) fs, rns)
    mRemoveDirectory = mRemoveFile
    mRename a b =
        modify $ \(ms, fs, rns) -> ( a : b : ms
                                   , map (movedirfilename a b) fs
                                   , map (trackOrigRename a b) rns)
    mModifyFilePS f _ = mCreateFile f
    mReadFilePS = error "mReadFilePS not defined for FilePathMonad"
