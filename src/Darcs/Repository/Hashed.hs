-- Copyright (C) 2006-2007 David Roundy
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
-- along with this program; if not, write to the Free Software Foundation,
-- Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
{-# LANGUAGE OverloadedStrings #-}
module Darcs.Repository.Hashed
    ( revertTentativeChanges
    , finalizeTentativeChanges
    , addToTentativeInventory
    , readPatches
    , readTentativePatches
    , writeAndReadPatch
    , writeTentativeInventory
    , copyHashedInventory
    , writePatchIfNecessary
    , tentativelyAddPatch
    , tentativelyRemovePatches
    , tentativelyRemovePatches_
    , tentativelyAddPatch_
    , tentativelyAddPatches
    , tentativelyAddPatches_
    , reorderInventory
    , UpdatePristine(..)
    , repoXor
    ) where

import Darcs.Prelude

import Control.Monad ( unless, when )
import Data.List ( foldl' )
import System.Directory
    ( copyFile
    , createDirectoryIfMissing
    , renameFile
    )
import System.FilePath.Posix ( (</>) )
import System.IO.Unsafe ( unsafeInterleaveIO )

import Darcs.Patch ( RepoPatch, effect, invert, invertFL, readPatch )
import Darcs.Patch.Apply ( Apply(..) )
import Darcs.Patch.Depends
    ( cleanLatestTag
    , removeFromPatchSet
    , slightlyOptimizePatchset
    , fullyOptimizePatchSet
    )
import Darcs.Patch.Format ( PatchListFormat )
import Darcs.Patch.Info ( displayPatchInfo, makePatchname, piName )
import Darcs.Patch.Invertible ( mkInvertible )
import Darcs.Patch.PatchInfoAnd
    ( PatchInfoAnd
    , createHashed
    , hopefully
    , info
    , patchInfoAndPatch
    )
import Darcs.Patch.Progress ( progressFL )
import Darcs.Patch.Read ( ReadPatch )
import Darcs.Patch.Rebase.Suspended
    ( addFixupsToSuspended
    , removeFixupsFromSuspended
    )
import Darcs.Patch.Set ( Origin, PatchSet(..), patchSet2RL )
import Darcs.Patch.Witnesses.Ordered
    ( FL(..)
    , foldlwFL
    , foldrwFL
    , mapRL
    , sequenceFL_
    , (+>+)
    , (+>>+)
    )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..) )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP )

import Darcs.Repository.Flags
    ( OptimizeDeep(..)
    , RemoteDarcs
    , UpdatePending(..)
    , Verbosity(..)
    , remoteDarcs
    )
import Darcs.Repository.Format
    ( RepoProperty(HashedInventory)
    , formatHas
    )
import Darcs.Repository.InternalTypes
    ( AccessType(..)
    , Repository
    , SAccessType(..)
    , repoAccessType
    , repoCache
    , repoFormat
    , repoLocation
    , unsafeCoerceR
    , withRepoDir
    )
import Darcs.Repository.Inventory
    ( peekPristineHash
    , pokePristineHash
    , readPatchesFromInventoryFile
    , showInventoryEntry
    , writeInventory
    , writePatchIfNecessary
    )
import qualified Darcs.Repository.Old as Old ( oldRepoFailMsg, readOldRepo )
import Darcs.Repository.Paths
import Darcs.Repository.Pending
    ( readTentativePending
    , writeTentativePending
    )
import Darcs.Repository.Pristine
    ( applyToTentativePristine
    , convertSizePrefixedPristine
    )
import Darcs.Repository.Rebase
    ( withTentativeRebase
    )
import Darcs.Repository.Traverse ( cleanRepository )
import Darcs.Repository.Unrevert
    ( removeFromUnrevertContext
    )

import Darcs.Util.ByteString ( gzReadFilePS )
import Darcs.Util.Cache ( Cache, fetchFileUsingCache )
import Darcs.Util.File ( Cachable(Uncachable), copyFileOrUrl )
import Darcs.Util.Hash ( SHA1, sha1Xor, sha1zero )
import Darcs.Util.Lock
    ( appendDocBinFile
    , writeAtomicFilePS
    , writeDocBinFile
    )
import Darcs.Util.Printer ( renderString )
import Darcs.Util.Progress ( beginTedious, debugMessage, endTedious )
import Darcs.Util.SignalHandler ( withSignalsBlocked )
import Darcs.Util.Tree ( Tree )

-- |revertTentativeChanges swaps the tentative and "real" hashed inventory
-- files, and then updates the tentative pristine with the "real" inventory
-- hash.
revertTentativeChanges :: Repository 'RO p wU wR -> IO ()
revertTentativeChanges repo = do
    copyFile hashedInventoryPath tentativeHashedInventoryPath
    inv <- gzReadFilePS tentativeHashedInventoryPath
    pristineHash <- convertSizePrefixedPristine (repoCache repo) (peekPristineHash inv)
    writeDocBinFile tentativePristinePath $ pokePristineHash pristineHash mempty
{-
    -- this is not needed, as we never again access the pristine hash in
    -- tentativeHashedInventoryPath, only that in tentativePristinePath
    writeDocBinFile tentativeHashedInventoryPath $
      pokePristineHash pristineHash inv
-}

-- |finalizeTentativeChanges trys to atomically swap the tentative
-- inventory/pristine pointers with the "real" pointers; it first re-reads the
-- inventory to optimize it, presumably to take account of any new tags, and
-- then writes out the new tentative inventory, and finally does the atomic
-- swap. In general, we can't clean the pristine cache at the same time, since
-- a simultaneous get might be in progress.
finalizeTentativeChanges :: RepoPatch p
                         => Repository 'RW p wU wR -> IO ()
finalizeTentativeChanges r = do
    debugMessage "Optimizing the inventory..."
    -- Read the tentative patches
    ps <- readTentativePatches r
    writeTentativeInventory r ps
    i <- gzReadFilePS tentativeHashedInventoryPath
    p <- gzReadFilePS tentativePristinePath
    -- Write out the "optimised" tentative inventory.
    writeDocBinFile tentativeHashedInventoryPath $
        pokePristineHash (peekPristineHash p) i
    -- Atomically swap.
    renameFile tentativeHashedInventoryPath hashedInventoryPath

-- | Add (append) a patch to the tentative inventory.
-- Warning: this allows to add any arbitrary patch!
-- Used by convert import and 'tentativelyAddPatch_'.
addToTentativeInventory :: RepoPatch p => Cache
                        -> PatchInfoAnd p wX wY -> IO ()
addToTentativeInventory c p = do
    hash <- snd <$> writePatchIfNecessary c p
    appendDocBinFile tentativeHashedInventoryPath $ showInventoryEntry (info p, hash)

-- | Read the recorded 'PatchSet' of a hashed 'Repository'.
readPatchesHashed :: (PatchListFormat p, ReadPatch p) => Repository rt p wU wR
                  -> IO (PatchSet p Origin wR)
readPatchesHashed repo =
  case repoAccessType repo of
    SRO -> readPatchesFromInventoryFile hashedInventoryPath repo
    SRW -> readPatchesFromInventoryFile tentativeHashedInventoryPath repo

-- | Read the tentative 'PatchSet' of a (hashed) 'Repository'.
readTentativePatches :: (PatchListFormat p, ReadPatch p)
                     => Repository 'RW p wU wR
                     -> IO (PatchSet p Origin wR)
readTentativePatches = readPatchesHashed

-- |Copy the hashed inventory from the given location to the given repository,
-- possibly using the given remote darcs binary.
copyHashedInventory :: Repository 'RO p wU wR -> RemoteDarcs -> String -> IO ()
copyHashedInventory outrepo rdarcs inloc | remote <- remoteDarcs rdarcs = do
    let outloc = repoLocation outrepo
    createDirectoryIfMissing False (outloc </> inventoriesDirPath)
    copyFileOrUrl remote (inloc </> hashedInventoryPath)
                         (outloc </> hashedInventoryPath)
                  Uncachable
    debugMessage "Done copying hashed inventory."

-- |writeAndReadPatch makes a patch lazy, by writing it out to disk (thus
-- forcing it), and then re-reads the patch lazily.
writeAndReadPatch :: RepoPatch p => Cache
                  -> PatchInfoAnd p wX wY -> IO (PatchInfoAnd p wX wY)
writeAndReadPatch c p = do
    (i, h) <- writePatchIfNecessary c p
    unsafeInterleaveIO $ readp h i
  where
    parse i h = do
        debugMessage $ "Rereading patch file for: " ++ piName i
        (fn, ps) <- fetchFileUsingCache c h
        case readPatch ps of
            Right x -> return x
            Left e -> fail $ unlines
                [ "Couldn't parse patch file " ++ fn
                , "which is"
                , renderString $ displayPatchInfo i
                , e
                ]

    readp h i = do Sealed x <- createHashed h (parse i)
                   return . patchInfoAndPatch i $ unsafeCoerceP x

-- | Write a 'PatchSet' to the tentative inventory.
writeTentativeInventory :: RepoPatch p
                        => Repository 'RW p wU wR
                        -> PatchSet p Origin wX
                        -> IO ()
writeTentativeInventory repo patchSet = do
    debugMessage "in writeTentativeInventory..."
    createDirectoryIfMissing False inventoriesDirPath
    let cache = repoCache repo
        tediousName = "Writing inventory"
    beginTedious tediousName
    hash <-
      writeInventory tediousName cache $ slightlyOptimizePatchset patchSet
    endTedious tediousName
    debugMessage "still in writeTentativeInventory..."
    (_filepath, content) <- fetchFileUsingCache cache hash
    writeAtomicFilePS tentativeHashedInventoryPath content

tentativelyAddPatch :: (RepoPatch p, ApplyState p ~ Tree)
                    => Repository 'RW p wU wR
                    -> Verbosity
                    -> UpdatePending
                    -> PatchInfoAnd p wR wY
                    -> IO (Repository 'RW p wU wY)
tentativelyAddPatch = tentativelyAddPatch_ UpdatePristine

tentativelyAddPatches :: (RepoPatch p, ApplyState p ~ Tree)
                      => Repository 'RW p wU wR
                      -> Verbosity
                      -> UpdatePending
                      -> FL (PatchInfoAnd p) wR wY
                      -> IO (Repository 'RW p wU wY)
tentativelyAddPatches = tentativelyAddPatches_ UpdatePristine

data UpdatePristine = UpdatePristine 
                    | DontUpdatePristine
                    | DontUpdatePristineNorRevert deriving Eq

tentativelyAddPatches_ :: (RepoPatch p, ApplyState p ~ Tree)
                       => UpdatePristine
                       -> Repository 'RW p wU wR
                       -> Verbosity
                       -> UpdatePending
                       -> FL (PatchInfoAnd p) wR wY
                       -> IO (Repository 'RW p wU wY)
tentativelyAddPatches_ upr r v upe ps = do
    let r' = unsafeCoerceR r
    withTentativeRebase r r' (foldlwFL (removeFixupsFromSuspended . hopefully) ps)
    withRepoDir r $ do
       sequenceFL_ (addToTentativeInventory (repoCache r)) ps
       when (upr == UpdatePristine) $ do
          debugMessage "Applying to pristine cache..."
          applyToTentativePristine r v (mkInvertible ps)
       when (upe == YesUpdatePending) $ do
          debugMessage "Updating pending..."
          Sealed pend <- readTentativePending r
          writeTentativePending r' $ invertFL (effect ps) +>>+ pend
       return r'

tentativelyAddPatch_ :: (RepoPatch p, ApplyState p ~ Tree)
                     => UpdatePristine
                     -> Repository 'RW p wU wR
                     -> Verbosity
                     -> UpdatePending
                     -> PatchInfoAnd p wR wY
                     -> IO (Repository 'RW p wU wY)
tentativelyAddPatch_ upr r verb upe p =
    tentativelyAddPatches_ upr r verb upe (p :>: NilFL)

tentativelyRemovePatches :: (RepoPatch p, ApplyState p ~ Tree)
                         => Repository 'RW p wU wR
                         -> UpdatePending
                         -> FL (PatchInfoAnd p) wX wR
                         -> IO (Repository 'RW p wU wX)
tentativelyRemovePatches = tentativelyRemovePatches_ UpdatePristine

tentativelyRemovePatches_ :: (RepoPatch p, ApplyState p ~ Tree)
                          => UpdatePristine
                          -> Repository 'RW p wU wR
                          -> UpdatePending
                          -> FL (PatchInfoAnd p) wX wR
                          -> IO (Repository 'RW p wU wX)
tentativelyRemovePatches_ upr r upe ps
  | formatHas HashedInventory (repoFormat r) = do
      withRepoDir r $ do
        ref <- readTentativePatches r
        unless (upr == DontUpdatePristineNorRevert) $ removeFromUnrevertContext ref ps
        debugMessage "Removing changes from tentative inventory..."
        r' <- removeFromTentativeInventory r ps
        withTentativeRebase r r' (foldrwFL (addFixupsToSuspended . hopefully) ps)
        when (upr == UpdatePristine) $
          applyToTentativePristine r NormalVerbosity $
            invert $ mkInvertible $ progressFL "Applying inverse to pristine" ps
        when (upe == YesUpdatePending) $ do
          debugMessage "Adding changes to pending..."
          Sealed pend <- readTentativePending r
          writeTentativePending r' $ effect ps +>+ pend
        return r'
  | otherwise = fail Old.oldRepoFailMsg

-- | Attempt to remove an FL of patches from the tentative inventory.
--
-- Precondition: it must be possible to remove the patches, i.e.
--
-- * the patches are in the repository
--
-- * any necessary commutations will succeed
removeFromTentativeInventory :: forall p wU wR wX. RepoPatch p
                             => Repository 'RW p wU wR
                             -> FL (PatchInfoAnd p) wX wR
                             -> IO (Repository 'RW p wU wX)
removeFromTentativeInventory repo to_remove = do
    debugMessage $ "Start removeFromTentativeInventory"
    allpatches :: PatchSet p Origin wR <- readTentativePatches repo
    remaining :: PatchSet p Origin wX <-
      case removeFromPatchSet to_remove allpatches of
        Nothing -> error "Hashed.removeFromTentativeInventory: precondition violated"
        Just r -> return r
    let repo' = unsafeCoerceR repo
    writeTentativeInventory repo' remaining
    debugMessage $ "Done removeFromTentativeInventory"
    return repo'

-- | Writes out a fresh copy of the inventory that minimizes the
-- amount of inventory that need be downloaded when people pull from
-- the repository. The exact beavior depends on the 3rd parameter:
--
-- For 'OptimizeShallow' it breaks up the inventory on the most recent tag.
-- This speeds up most commands when run remotely, both because a
-- smaller file needs to be transfered (only the most recent
-- inventory).  It also gives a guarantee that all the patches prior
-- to a given tag are included in that tag, so less commutation and
-- history traversal is needed.  This latter issue can become very
-- important in large repositories.
--
-- For 'OptimizeDeep', the whole repo is traversed, from oldest to newest
-- patch. Every tag we encounter is made clean, but only if that doesn't make
-- any previous clean tag unclean. Every clean tags gets its own inventory.
-- This speeds up "deep" operations, too, such as cloning a specific tag.
-- It does not necessarily make the latest tag clean, but the benefits are
-- similar to the shallow case.
reorderInventory :: (RepoPatch p, ApplyState p ~ Tree)
                 => Repository 'RW p wU wR
                 -> OptimizeDeep
                 -> IO ()
reorderInventory r deep
  | formatHas HashedInventory (repoFormat r) = do
      let optimize =
            case deep of
              OptimizeDeep -> fullyOptimizePatchSet
              OptimizeShallow -> cleanLatestTag
      readPatches r >>= return . optimize >>= writeTentativeInventory r
      cleanRepository r
      withSignalsBlocked $ finalizeTentativeChanges r
  | otherwise = fail Old.oldRepoFailMsg

-- | Read inventories and patches from a 'Repository' and return them as a
-- 'PatchSet'. Note that patches and inventories are read lazily.
readPatches :: RepoPatch p
            => Repository rt p wU wR
            -> IO (PatchSet p Origin wR)
readPatches r
    | formatHas HashedInventory (repoFormat r) = readPatchesHashed r
    | otherwise = do Sealed ps <- Old.readOldRepo (repoLocation r)
                     return $ unsafeCoerceP ps

-- | XOR of all hashes of the patches' metadata.
-- It enables to quickly see whether two repositories
-- have the same patches, independently of their order.
-- It relies on the assumption that the same patch cannot
-- be present twice in a repository.
-- This checksum is not cryptographically secure,
-- see http://robotics.stanford.edu/~xb/crypto06b/ .
repoXor :: RepoPatch p => Repository rt p wU wR -> IO SHA1
repoXor repo = do
  hashes <- mapRL (makePatchname . info) . patchSet2RL <$> readPatches repo
  return $ foldl' sha1Xor sha1zero hashes
