-- Copyright (C) 2002-2004 David Roundy
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

module Darcs.Repository
    ( Repository
    , repoLocation
    , repoFormat
    , repoPristineType
    , repoCache
    , PristineType(..)
    , HashedDir(..)
    , Cache
    , CacheLoc(..)
    , CacheType(..)
    , WritableOrNot(..)
    , cacheEntries
    , mkCache
    , reportBadSources
    , RepoJob(..)
    , maybeIdentifyRepository
    , identifyRepositoryFor
    , ReadingOrWriting(..)
    , withRepoLock
    , withRepoLockCanFail
    , withRepository
    , withRepositoryLocation
    , withUMaskFlag
    , findRepository
    , amInRepository
    , amNotInRepository
    , amInHashedRepository
    , writePristine
    , readPatches
    , prefsUrl
    , addToPending
    , addPendingDiffToPending
    , tentativelyAddPatch
    , tentativelyRemovePatches
    , tentativelyAddToPending
    , readTentativePatches
    , withManualRebaseUpdate
    , tentativelyMergePatches
    , considerMergeToWorking
    , revertRepositoryChanges
    , finalizeRepositoryChanges
    , createRepository
    , createRepositoryV1
    , createRepositoryV2
    , EmptyRepository(..)
    , cloneRepository
    , applyToWorking
    , createPristineDirectoryTree
    , reorderInventory
    , cleanRepository
    , PatchSet
    , SealedPatchSet
    , PatchInfoAnd
    , setScriptsExecutable
    , setScriptsExecutablePatches
    , modifyCache
    -- * Recorded and unrecorded and pending.
    , readPristine
    , readUnrecorded
    , unrecordedChanges
    , readPendingAndWorking
    , filterOutConflicts
    , readPristineAndPending
    ) where

import Darcs.Repository.State
    ( readPristine
    , readUnrecorded
    , unrecordedChanges
    , readPendingAndWorking
    , readPristineAndPending
    , filterOutConflicts
    , addPendingDiffToPending
    , addToPending
    )

import Darcs.Repository.Prefs ( prefsUrl )

import Darcs.Repository.Identify
    ( maybeIdentifyRepository
    , identifyRepositoryFor
    , ReadingOrWriting(..)
    , findRepository
    , amInRepository
    , amNotInRepository
    , amInHashedRepository
    )
import Darcs.Repository.Hashed
    ( readPatches
    , readTentativePatches
    , tentativelyAddPatch
    , tentativelyRemovePatches
    , revertRepositoryChanges
    , finalizeRepositoryChanges
    , reorderInventory
    )
import Darcs.Repository.Pristine
    ( createPristineDirectoryTree
    , writePristine
    )
import Darcs.Repository.Traverse ( cleanRepository )
import Darcs.Repository.Pending
    ( tentativelyAddToPending
    )
import Darcs.Repository.Working
    ( applyToWorking
    , setScriptsExecutable
    , setScriptsExecutablePatches
    )
import Darcs.Repository.Job
    ( RepoJob(..)
    , withRepoLock
    , withRepoLockCanFail
    , withRepository
    , withRepositoryLocation
    , withUMaskFlag
    )
import Darcs.Repository.Rebase ( withManualRebaseUpdate )
import Darcs.Repository.Merge( tentativelyMergePatches
                             , considerMergeToWorking
                             )
import Darcs.Util.Cache
    ( Cache
    , CacheLoc(..)
    , CacheType(..)
    , HashedDir(..)
    , WritableOrNot(..)
    , cacheEntries
    , mkCache
    , reportBadSources
    )
import Darcs.Repository.InternalTypes
    ( Repository
    , PristineType(..)
    , modifyCache
    , repoLocation
    , repoFormat
    , repoPristineType
    , repoCache
    )
import Darcs.Repository.Clone
    ( cloneRepository
    )
import Darcs.Repository.Create
    ( createRepository
    , createRepositoryV1
    , createRepositoryV2
    , EmptyRepository(..)
    )

import Darcs.Patch.Set ( PatchSet, SealedPatchSet )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd )
