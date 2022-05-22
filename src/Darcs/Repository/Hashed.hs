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
    , revertRepositoryChanges
    , finalizeTentativeChanges
    , addToTentativeInventory
    , readPatches
    , writeAndReadPatch
    , writeTentativeInventory
    , copyHashedInventory
    , writePatchIfNecessary
    , tentativelyAddPatch
    , tentativelyRemovePatches
    , tentativelyRemovePatches_
    , tentativelyAddPatch_
    , tentativelyAddPatches_
    , finalizeRepositoryChanges
    , reorderInventory
    , UpdatePristine(..)
    , repoXor
    , upgradeOldStyleRebase
    ) where

import Darcs.Prelude

import Control.Monad ( unless, when )
import Data.List ( foldl' )
import System.Directory
    ( copyFile
    , createDirectoryIfMissing
    , doesFileExist
    , removeFile
    , renameFile
    )
import System.FilePath.Posix ( (</>) )
import System.IO ( IOMode(..), hClose, hPutStrLn, openBinaryFile, stderr )
import System.IO.Error ( catchIOError )
import System.IO.Unsafe ( unsafeInterleaveIO )

import Darcs.Patch ( RepoPatch, effect, readPatch )
import Darcs.Patch.Apply ( Apply(..) )
import Darcs.Patch.Depends
    ( cleanLatestTag
    , removeFromPatchSet
    , slightlyOptimizePatchset
    )
import Darcs.Patch.Format ( PatchListFormat )
import Darcs.Patch.Info ( displayPatchInfo, makePatchname, piName )
import Darcs.Patch.PatchInfoAnd
    ( PatchInfoAnd
    , createHashed
    , hopefully
    , info
    , patchInfoAndPatch
    )
import Darcs.Patch.Progress ( progressFL )
import Darcs.Patch.Read ( ReadPatch )
import qualified Darcs.Patch.Rebase.Legacy.Wrapped as W
import Darcs.Patch.Rebase.Suspended
    ( Suspended(..)
    , addFixupsToSuspended
    , removeFixupsFromSuspended
    , showSuspended
    )
import Darcs.Patch.Set ( Origin, PatchSet(..), Tagged(..), patchSet2RL )
import Darcs.Patch.Show ( ShowPatchFor(..) )
import Darcs.Patch.Witnesses.Ordered
    ( (:>)(..)
    , FL(..)
    , RL(..)
    , foldFL_M
    , foldrwFL
    , mapRL
    , (+>+)
    )
import Darcs.Patch.Witnesses.Sealed ( Dup(..), Sealed(..) )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP )

import Darcs.Repository.Flags
    ( Compression
    , DryRun(..)
    , RemoteDarcs
    , UpdatePending(..)
    , Verbosity(..)
    , remoteDarcs
    )
import Darcs.Repository.Format
    ( RepoProperty(HashedInventory, RebaseInProgress, RebaseInProgress_2_16)
    , addToFormat
    , formatHas
    , removeFromFormat
    , writeRepoFormat
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
    , unsafeEndTransaction
    , unsafeStartTransaction
    , withRepoDir
    )
import Darcs.Repository.Inventory
    ( Inventory(..)
    , peekPristineHash
    , pokePristineHash
    , readInventoryPrivate
    , readPatchesFromInventoryEntries
    , readPatchesUsingSpecificInventory
    , showInventoryEntry
    , writeInventory
    , writePatchIfNecessary
    )
import qualified Darcs.Repository.Old as Old ( oldRepoFailMsg, readOldRepo )
import Darcs.Repository.PatchIndex
    ( createOrUpdatePatchIndexDisk
    , doesPatchIndexExist
    )
import Darcs.Repository.Paths
import Darcs.Repository.Pending
    ( finalizePending
    , readTentativePending
    , revertPending
    , tentativelyRemoveFromPending
    , writeTentativePending
    )
import Darcs.Repository.Pristine
    ( ApplyDir(..)
    , applyToTentativePristine
    , applyToTentativePristineCwd
    , convertSizePrefixedPristine
    )
import Darcs.Repository.Rebase
    ( extractOldStyleRebase
    , finalizeTentativeRebase
    , readTentativeRebase
    , revertTentativeRebase
    , withTentativeRebase
    , writeTentativeRebase
    )
import Darcs.Repository.State ( updateIndex )
import Darcs.Repository.Unrevert
    ( finalizeTentativeUnrevert
    , removeFromUnrevertContext
    , revertTentativeUnrevert
    )

import Darcs.Util.AtExit ( atexit )
import Darcs.Util.ByteString ( gzReadFilePS )
import Darcs.Util.Cache ( Cache, fetchFileUsingCache )
import Darcs.Util.File ( Cachable(Uncachable), copyFileOrUrl )
import Darcs.Util.Hash ( SHA1, sha1Xor, sha1zero )
import Darcs.Util.Lock
    ( appendDocBinFile
    , getLock
    , releaseLock
    , writeAtomicFilePS
    , writeDocBinFile
    )
import Darcs.Util.Printer ( renderString, text, ($$) )
import Darcs.Util.Printer.Color ( ePutDocLn )
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
                         => Repository 'RW p wU wR -> Compression -> IO ()
finalizeTentativeChanges r compr = do
    debugMessage "Optimizing the inventory..."
    -- Read the tentative patches
    ps <- readTentativePatches r
    writeTentativeInventory r compr ps
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
addToTentativeInventory :: RepoPatch p => Cache -> Compression
                        -> PatchInfoAnd p wX wY -> IO ()
addToTentativeInventory c compr p = do
    hash <- snd <$> writePatchIfNecessary c compr p
    appendDocBinFile tentativeHashedInventoryPath $ showInventoryEntry (info p, hash)

-- | Read the recorded 'PatchSet' of a hashed 'Repository'.
readPatchesHashed :: (PatchListFormat p, ReadPatch p) => Repository rt p wU wR
                  -> IO (PatchSet p Origin wR)
readPatchesHashed repo =
  case repoAccessType repo of
    SRO -> readPatchesUsingSpecificInventory hashedInventoryPath repo
    SRW -> readPatchesUsingSpecificInventory tentativeHashedInventoryPath repo

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
writeAndReadPatch :: RepoPatch p => Cache -> Compression
                  -> PatchInfoAnd p wX wY -> IO (PatchInfoAnd p wX wY)
writeAndReadPatch c compr p = do
    (i, h) <- writePatchIfNecessary c compr p
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
                        -> Compression
                        -> PatchSet p Origin wX
                        -> IO ()
writeTentativeInventory repo compr patchSet = do
    debugMessage "in writeTentativeInventory..."
    createDirectoryIfMissing False inventoriesDirPath
    let cache = repoCache repo
        tediousName = "Writing inventory"
    beginTedious tediousName
    hash <-
      writeInventory tediousName cache compr $ slightlyOptimizePatchset patchSet
    endTedious tediousName
    debugMessage "still in writeTentativeInventory..."
    (_filepath, content) <- fetchFileUsingCache cache hash
    writeAtomicFilePS tentativeHashedInventoryPath content

tentativelyAddPatch :: (RepoPatch p, ApplyState p ~ Tree)
                    => Repository 'RW p wU wR
                    -> Compression
                    -> Verbosity
                    -> UpdatePending
                    -> PatchInfoAnd p wR wY
                    -> IO (Repository 'RW p wU wY)
tentativelyAddPatch = tentativelyAddPatch_ UpdatePristine

data UpdatePristine = UpdatePristine 
                    | DontUpdatePristine
                    | DontUpdatePristineNorRevert deriving Eq

tentativelyAddPatches_ :: (RepoPatch p, ApplyState p ~ Tree)
                       => UpdatePristine
                       -> Repository 'RW p wU wR
                       -> Compression
                       -> Verbosity
                       -> UpdatePending
                       -> FL (PatchInfoAnd p) wR wY
                       -> IO (Repository 'RW p wU wY)
tentativelyAddPatches_ upr r c v upe ps =
    foldFL_M (\r' p -> tentativelyAddPatch_ upr r' c v upe p) r ps

tentativelyAddPatch_ :: (RepoPatch p, ApplyState p ~ Tree)
                     => UpdatePristine
                     -> Repository 'RW p wU wR
                     -> Compression
                     -> Verbosity
                     -> UpdatePending
                     -> PatchInfoAnd p wR wY
                     -> IO (Repository 'RW p wU wY)
tentativelyAddPatch_ upr r compr verb upe p = do
    let r' = unsafeCoerceR r
    withTentativeRebase r r' (removeFixupsFromSuspended $ hopefully p)
    withRepoDir r $ do
       addToTentativeInventory (repoCache r) compr p
       when (upr == UpdatePristine) $ do
          debugMessage "Applying to pristine cache..."
          applyToTentativePristine r ApplyNormal verb p
       when (upe == YesUpdatePending) $ do
          debugMessage "Updating pending..."
          tentativelyRemoveFromPending r' (effect p)
       return r'

tentativelyRemovePatches :: (RepoPatch p, ApplyState p ~ Tree)
                         => Repository 'RW p wU wR
                         -> Compression
                         -> UpdatePending
                         -> FL (PatchInfoAnd p) wX wR
                         -> IO (Repository 'RW p wU wX)
tentativelyRemovePatches = tentativelyRemovePatches_ UpdatePristine

tentativelyRemovePatches_ :: (RepoPatch p, ApplyState p ~ Tree)
                          => UpdatePristine
                          -> Repository 'RW p wU wR
                          -> Compression
                          -> UpdatePending
                          -> FL (PatchInfoAnd p) wX wR
                          -> IO (Repository 'RW p wU wX)
tentativelyRemovePatches_ upr r compr upe ps
  | formatHas HashedInventory (repoFormat r) = do
      withRepoDir r $ do
        ref <- readTentativePatches r
        unless (upr == DontUpdatePristineNorRevert) $ removeFromUnrevertContext ref ps
        debugMessage "Removing changes from tentative inventory..."
        r' <- removeFromTentativeInventory r compr ps
        withTentativeRebase r r' (foldrwFL (addFixupsToSuspended . hopefully) ps)
        when (upr == UpdatePristine) $
          applyToTentativePristineCwd (repoCache r) ApplyInverted $
            progressFL "Applying inverse to pristine" ps
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
                             -> Compression
                             -> FL (PatchInfoAnd p) wX wR
                             -> IO (Repository 'RW p wU wX)
removeFromTentativeInventory repo compr to_remove = do
    debugMessage $ "Start removeFromTentativeInventory"
    allpatches :: PatchSet p Origin wR <- readTentativePatches repo
    remaining :: PatchSet p Origin wX <-
      case removeFromPatchSet to_remove allpatches of
        Nothing -> error "Hashed.removeFromTentativeInventory: precondition violated"
        Just r -> return r
    let repo' = unsafeCoerceR repo
    writeTentativeInventory repo' compr remaining
    debugMessage $ "Done removeFromTentativeInventory"
    return repo'

-- | Atomically copy the tentative state to the recorded state,
-- thereby committing the tentative changes that were made so far.
-- This includes inventories, pending, rebase, and the index.
finalizeRepositoryChanges :: (RepoPatch p, ApplyState p ~ Tree)
                          => Repository 'RW p wU wR
                          -> UpdatePending
                          -> Compression
                          -> DryRun
                          -> IO (Repository 'RO p wU wR)
finalizeRepositoryChanges r updatePending compr dryrun
    | formatHas HashedInventory (repoFormat r) =
        withRepoDir r $ do
          let r' = unsafeEndTransaction $ unsafeCoerceR r
          when (dryrun == NoDryRun) $ do
            debugMessage "Finalizing changes..."
            withSignalsBlocked $ do
                finalizeTentativeRebase
                finalizeTentativeChanges r compr
                finalizePending r updatePending
                finalizeTentativeUnrevert
            debugMessage "Done finalizing changes..."
            ps <- readPatches r'
            pi_exists <- doesPatchIndexExist (repoLocation r')
            when pi_exists $
              createOrUpdatePatchIndexDisk r' ps
              `catchIOError` \e ->
                hPutStrLn stderr $ "Cannot create or update patch index: "++ show e
            updateIndex r'
          releaseLock lockPath
          return r'
    | otherwise = fail Old.oldRepoFailMsg

-- TODO: rename this and document the transaction protocol (revert/finalize)
-- clearly.
-- |Slightly confusingly named: as well as throwing away any tentative
-- changes, revertRepositoryChanges also re-initialises the tentative state.
-- It's therefore used before makign any changes to the repo.
revertRepositoryChanges :: RepoPatch p
                        => Repository 'RO p wU wR
                        -> UpdatePending
                        -> IO (Repository 'RW p wU wR)
revertRepositoryChanges r upe
  | formatHas HashedInventory (repoFormat r) =
      withRepoDir r $ do
        lock <- getLock lockPath 30
        atexit (releaseLock lock)
        checkIndexIsWritable
          `catchIOError` \e -> fail (unlines ["Cannot write index", show e])
        revertTentativeUnrevert
        revertPending r upe
        revertTentativeChanges r
        let r' = unsafeCoerceR r
        revertTentativeRebase r'
        return $ unsafeStartTransaction r'
  | otherwise = fail Old.oldRepoFailMsg

checkIndexIsWritable :: IO ()
checkIndexIsWritable = do
    checkWritable indexInvalidPath
    checkWritable indexPath
  where
    checkWritable path = do
      exists <- doesFileExist path
      touchFile path
      unless exists $ removeFile path
    touchFile path = openBinaryFile path AppendMode >>= hClose

-- | Writes out a fresh copy of the inventory that minimizes the
-- amount of inventory that need be downloaded when people pull from
-- the repository.
--
-- Specifically, it breaks up the inventory on the most recent tag.
-- This speeds up most commands when run remotely, both because a
-- smaller file needs to be transfered (only the most recent
-- inventory).  It also gives a guarantee that all the patches prior
-- to a given tag are included in that tag, so less commutation and
-- history traversal is needed.  This latter issue can become very
-- important in large repositories.
reorderInventory :: (RepoPatch p, ApplyState p ~ Tree)
                 => Repository 'RW p wU wR
                 -> Compression
                 -> IO ()
reorderInventory r compr
  | formatHas HashedInventory (repoFormat r) = do
      cleanLatestTag `fmap` readPatches r >>=
        writeTentativeInventory r compr
      withSignalsBlocked $ finalizeTentativeChanges r compr
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

-- | Upgrade a possible old-style rebase in progress to the new style.
upgradeOldStyleRebase :: forall p wU wR.
                         (RepoPatch p, ApplyState p ~ Tree)
                      => Repository 'RW p wU wR -> Compression -> IO ()
upgradeOldStyleRebase repo compr = do
  PatchSet (ts :: RL (Tagged p) Origin wX) _ <- readTentativePatches repo
  Inventory _ invEntries <- readInventoryPrivate tentativeHashedInventoryPath
  Sealed wps <-
    readPatchesFromInventoryEntries @(W.WrappedNamed p) (repoCache repo) invEntries
  case extractOldStyleRebase wps of
    Nothing ->
      ePutDocLn $ text "No old-style rebase state found, no upgrade needed."
    Just ((ps :: RL (PatchInfoAnd p) wX wZ) :> Dup r) -> do
      -- low-level call, must not try to update an existing rebase patch,
      -- nor update anything else beside the inventory
      writeTentativeInventory repo compr (PatchSet ts ps)
      Items old_r <- readTentativeRebase repo
      case old_r of
        NilFL -> do
          writeTentativeRebase (unsafeCoerceR repo) r
          writeRepoFormat
            ( addToFormat RebaseInProgress_2_16
            $ removeFromFormat RebaseInProgress
            $ repoFormat repo)
            formatPath
          _ <- finalizeRepositoryChanges repo NoUpdatePending compr NoDryRun
          return ()
        _ -> do
          ePutDocLn
            $  "A new-style rebase is already in progress, not overwriting it."
            $$ "This should not have happened! This is the old-style rebase I found"
            $$ "and removed from the repository:"
            $$ showSuspended ForDisplay r
