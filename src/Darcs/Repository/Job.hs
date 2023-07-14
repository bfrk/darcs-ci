-- Copyright (C) 2002-2004,2007-2008 David Roundy
-- Copyright (C) 2005 Juliusz Chroboczek
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

{-# LANGUAGE ForeignFunctionInterface #-}

module Darcs.Repository.Job
    ( RepoJob(..)
    , IsPrimV1(..)
    , withRepoLock
    , withOldRepoLock
    , withRepoLockCanFail
    , withRepository
    , withRepositoryLocation
    , withUMaskFlag
    ) where

import Darcs.Prelude

import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch.V1 ( RepoPatchV1 )
import Darcs.Patch.V2 ( RepoPatchV2 )
import Darcs.Patch.V3 ( RepoPatchV3 )
import qualified Darcs.Patch.V1.Prim as V1 ( Prim(..) )
import qualified Darcs.Patch.V2.Prim as V2 ( Prim(..) )
import Darcs.Patch ( PrimOf )
import Darcs.Patch.Prim.V1 ( Prim )
import Darcs.Patch.RepoPatch ( RepoPatch )

import Darcs.Repository.Flags ( UMask(..), UseCache(..) )
import Darcs.Repository.Format
    ( RepoProperty( Darcs2
                  , Darcs3
                  , RebaseInProgress
                  , RebaseInProgress_2_16
                  , HashedInventory
                  )
    , formatHas
    , writeProblem
    )
import Darcs.Repository.Identify ( identifyRepository )
import Darcs.Repository.Transaction( revertRepositoryChanges )
import Darcs.Repository.InternalTypes
    ( Repository
    , AccessType(..)
    , repoFormat
    , unsafeCoercePatchType
    , unsafeStartTransaction
    )
import Darcs.Repository.Paths ( lockPath )
import Darcs.Repository.Rebase
    ( startRebaseJob
    , rebaseJob
    , maybeDisplaySuspendedStatus
    , checkOldStyleRebaseStatus
    )
import Darcs.Util.Lock ( withLock, withLockCanFail )

import Darcs.Util.Progress ( debugMessage )

import Control.Monad ( when )
import Control.Exception ( bracket_, finally )
import Data.Constraint ( Dict(..) )

import Foreign.C.String ( CString, withCString )
import Foreign.C.Error ( throwErrno )
import Foreign.C.Types ( CInt(..) )

import Darcs.Util.Tree ( Tree )

withUMaskFlag :: UMask -> IO a -> IO a
withUMaskFlag NoUMask = id
withUMaskFlag (YesUMask umask) = withUMask umask

foreign import ccall unsafe "umask.h set_umask" set_umask
    :: CString -> IO CInt
foreign import ccall unsafe "umask.h reset_umask" reset_umask
    :: CInt -> IO CInt

withUMask :: String
          -> IO a
          -> IO a
withUMask umask job =
    do rc <- withCString umask set_umask
       when (rc < 0) (throwErrno "Couldn't set umask")
       bracket_
           (return ())
           (reset_umask rc)
           job

-- |A @RepoJob@ wraps up an action to be performed with a repository. Because repositories
-- can contain different types of patches, such actions typically need to be polymorphic
-- in the kind of patch they work on. @RepoJob@ is used to wrap up the polymorphism,
-- and the various functions that act on a @RepoJob@ are responsible for instantiating
-- the underlying action with the appropriate patch type.
data RepoJob rt a
    -- = RepoJob (forall p wR wU . RepoPatch p => Repository p wU wR -> IO a)
    -- TODO: Unbind Tree from RepoJob, possibly renaming existing RepoJob
    =
    -- |The most common @RepoJob@; the underlying action can accept any patch type that
    -- a darcs repository may use.
      RepoJob (forall p wR wU . (RepoPatch p, ApplyState p ~ Tree) => Repository rt p wU wR -> IO a)
    -- |A job that only works on darcs 1 patches
    | V1Job (forall wR wU . Repository rt (RepoPatchV1 V1.Prim) wU wR -> IO a)
    -- |A job that only works on darcs 2 patches
    | V2Job (forall wR wU . Repository rt (RepoPatchV2 V2.Prim) wU wR -> IO a)
    -- |A job that works on any repository where the patch type @p@ has 'PrimOf' @p@ = 'Prim'.
    --
    -- This was added to support darcsden, which inspects the internals of V1 prim patches.
    --
    -- In future this should be replaced with a more abstract inspection API as part of 'PrimPatch'.
    | PrimV1Job (forall p wR wU . (RepoPatch p, ApplyState p ~ Tree, IsPrimV1 (PrimOf p))
               => Repository rt p wU wR -> IO a)
    -- A job that works on normal darcs repositories, but will want access to the rebase patch if it exists.
    | RebaseAwareJob (forall p wR wU . (RepoPatch p, ApplyState p ~ Tree) => Repository rt p wU wR -> IO a)
    | RebaseJob (forall p wR wU . (RepoPatch p, ApplyState p ~ Tree) => Repository rt p wU wR -> IO a)
    | OldRebaseJob (forall p wR wU . (RepoPatch p, ApplyState p ~ Tree) => Repository rt p wU wR -> IO a)
    | StartRebaseJob (forall p wR wU . (RepoPatch p, ApplyState p ~ Tree) => Repository rt p wU wR -> IO a)

onRepoJob :: RepoJob rt1 a -- original repojob passed to withXxx
          -> (forall p wR wU . (RepoPatch p, ApplyState p ~ Tree) =>
              (Repository rt1 p wU wR -> IO a)
              -> Repository rt2 p wU wR -> IO a)
          -> RepoJob rt2 a -- result job takes a Repo rt2
onRepoJob (RepoJob job) f = RepoJob (f job)
onRepoJob (V1Job job) f = V1Job (f job)
onRepoJob (V2Job job) f = V2Job (f job)
onRepoJob (PrimV1Job job) f = PrimV1Job (f job)
onRepoJob (RebaseAwareJob job) f = RebaseAwareJob (f job)
onRepoJob (RebaseJob job) f = RebaseJob (f job)
onRepoJob (OldRebaseJob job) f = OldRebaseJob (f job)
onRepoJob (StartRebaseJob job) f = StartRebaseJob (f job)

-- | apply a given RepoJob to a repository in the current working directory
withRepository :: UseCache -> RepoJob 'RO a -> IO a
withRepository useCache = withRepositoryLocation useCache "."

-- | This is just an internal type to Darcs.Repository.Job for
-- calling runJob in a strongly-typed way
data RepoPatchType p where
  RepoV1 :: RepoPatchType (RepoPatchV1 V1.Prim)
  RepoV2 :: RepoPatchType (RepoPatchV2 V2.Prim)
  RepoV3 :: RepoPatchType (RepoPatchV3 V2.Prim)

-- | Check multiple patch types against the
-- constraints required by most repository jobs
checkTree :: RepoPatchType p -> Dict (ApplyState p ~ Tree)
checkTree RepoV1 = Dict
checkTree RepoV2 = Dict
checkTree RepoV3 = Dict

class IsPrimV1 p where
  toPrimV1 :: p wX wY -> Prim wX wY
instance IsPrimV1 V1.Prim where
  toPrimV1 = V1.unPrim
instance IsPrimV1 V2.Prim where
  toPrimV1 = V2.unPrim

-- | Check multiple patch types against the
-- constraints required by 'PrimV1Job'
checkPrimV1 :: RepoPatchType p -> Dict (IsPrimV1 (PrimOf p))
checkPrimV1 RepoV1 = Dict
checkPrimV1 RepoV2 = Dict
checkPrimV1 RepoV3 = Dict

-- | apply a given RepoJob to a repository in a given url
withRepositoryLocation :: UseCache -> String -> RepoJob 'RO a -> IO a
withRepositoryLocation useCache url repojob = do
    repo <- identifyRepository useCache url

    let
        rf = repoFormat repo

        -- in order to pass SRepoType and RepoPatchType at different types, we need a polymorphic
        -- function that we call in two different ways, rather than directly varying the argument.
        runJob1 :: Bool -> Repository rt pDummy wU wR -> RepoJob rt a -> IO a
        runJob1 hasRebase =
          if formatHas Darcs3 rf
          then runJob RepoV3 hasRebase
          else
            if formatHas Darcs2 rf
            then runJob RepoV2 hasRebase
            else runJob RepoV1 hasRebase

    runJob1
      (formatHas RebaseInProgress rf || formatHas RebaseInProgress_2_16 rf)
      repo
      repojob

runJob
  :: forall rt p pDummy wR wU a
   . RepoPatch p
  => RepoPatchType p
  -> Bool
  -> Repository rt pDummy wU wR
  -> RepoJob rt a
  -> IO a
runJob patchType hasRebase repo repojob = do

  -- The actual type the repository should have is only known when
  -- when this function is called, so we need to "cast" it to its proper type
  let
    therepo = unsafeCoercePatchType repo :: Repository rt p wU wR

  Dict <- return $ checkTree patchType
  case repojob of
    RepoJob job -> do
      checkOldStyleRebaseStatus therepo
      job therepo
        `finally`
          maybeDisplaySuspendedStatus therepo

    PrimV1Job job -> do
      Dict <- return $ checkPrimV1 patchType
      checkOldStyleRebaseStatus therepo
      job therepo `finally` maybeDisplaySuspendedStatus therepo

    V2Job job ->
      case patchType of
        RepoV2 -> job therepo
        RepoV1 ->
          fail $    "This repository contains darcs v1 patches,"
                 ++ " but the command requires darcs v2 patches."
        RepoV3 ->
          fail $    "This repository contains darcs v3 patches,"
                 ++ " but the command requires darcs v2 patches."

    V1Job job ->
      case patchType of
        RepoV1 -> job therepo
        RepoV2 ->
          fail $    "This repository contains darcs v2 patches,"
                 ++ " but the command requires darcs v1 patches."
        RepoV3 ->
          fail $    "This repository contains darcs v3 patches,"
                 ++ " but the command requires darcs v1 patches."

    RebaseAwareJob job ->
      case hasRebase of
        False -> job therepo
        True -> do
          checkOldStyleRebaseStatus therepo
          rebaseJob job therepo

    RebaseJob job ->
      case hasRebase of
        False ->
          fail "No rebase in progress. Try 'darcs rebase suspend' first."
        True -> do
          checkOldStyleRebaseStatus therepo
          rebaseJob job therepo

    OldRebaseJob job -> do
      -- no checkOldStyleRebaseStatus, this is for 'rebase upgrade'
      job therepo `finally` maybeDisplaySuspendedStatus therepo

    StartRebaseJob job -> do
      checkOldStyleRebaseStatus therepo
      startRebaseJob job therepo

-- | Apply a given RepoJob to a repository in the current working directory.
-- However, before doing the job, take the repo lock and initializes a repo
-- transaction.
withRepoLock :: UseCache -> UMask -> RepoJob 'RW a -> IO a
withRepoLock useCache um repojob =
  withLock lockPath $
    withRepository useCache $ onRepoJob repojob $ \job repository -> do
      maybe (return ()) fail $ writeProblem (repoFormat repository)
      withUMaskFlag um $ revertRepositoryChanges repository >>= job

-- | run a lock-taking job in an old-fashion repository.
--   only used by `darcs optimize upgrade`.
withOldRepoLock :: RepoJob 'RW a -> IO a
withOldRepoLock repojob =
  withRepository NoUseCache $ onRepoJob repojob $ \job repository ->
    withLock lockPath $ job $ unsafeStartTransaction repository

-- | Apply a given RepoJob to a repository in the current working directory,
-- taking a lock. If lock not takeable, do nothing. If old-fashioned
-- repository, do nothing. The job must not touch pending or pending.tentative,
-- because there is no call to revertRepositoryChanges. This entry point is
-- currently only used for attemptCreatePatchIndex.
withRepoLockCanFail :: UseCache -> RepoJob 'RO () -> IO ()
withRepoLockCanFail useCache repojob = do
  eitherDone <-
    withLockCanFail lockPath $
      withRepository useCache $ onRepoJob repojob $ \job repository -> do
        let rf = repoFormat repository
        if formatHas HashedInventory rf then do
          maybe (return ()) fail $ writeProblem rf
          job repository
        else
          debugMessage
            "Not doing the job because this is an old-fashioned repository."
  case eitherDone of
    Left  _ -> debugMessage "Lock could not be obtained, not doing the job."
    Right _ -> return ()
