{-# LANGUAGE OverloadedStrings #-}
module Darcs.Repository.Transaction
    ( revertRepositoryChanges
    , finalizeRepositoryChanges
    , upgradeOldStyleRebase
    ) where

import Darcs.Prelude

import Control.Monad ( unless, void, when )
import System.Directory ( doesFileExist, removeFile )
import System.IO ( IOMode(..), hClose, hPutStrLn, openBinaryFile, stderr )
import System.IO.Error ( catchIOError )

import Darcs.Patch ( ApplyState, PatchInfoAnd, RepoPatch )
import qualified Darcs.Patch.Rebase.Legacy.Wrapped as W
import Darcs.Patch.Rebase.Suspended ( Suspended(..), showSuspended )
import Darcs.Patch.Set ( Origin, PatchSet(..), Tagged(..) )
import Darcs.Patch.Show ( ShowPatchFor(..) )
import Darcs.Patch.Witnesses.Ordered ( FL(..), RL(..), (:>)(..) )
import Darcs.Patch.Witnesses.Sealed ( Dup(..), Sealed(..) )

import Darcs.Repository.Flags ( DryRun(..) )
import Darcs.Repository.Format
    ( RepoProperty(HashedInventory, RebaseInProgress, RebaseInProgress_2_16)
    , addToFormat
    , formatHas
    , removeFromFormat
    )
import Darcs.Repository.Hashed
    ( finalizeTentativeChanges
    , readPatches
    , readTentativePatches
    , revertTentativeChanges
    , writeTentativeInventory
    )
import Darcs.Repository.InternalTypes
    ( AccessType(..)
    , Repository
    , modifyRepoFormat
    , repoCache
    , repoFormat
    , repoLocation
    , unsafeCoerceR
    , unsafeEndTransaction
    , unsafeStartTransaction
    , withRepoDir
    )
import Darcs.Repository.Inventory ( readOneInventory )
import qualified Darcs.Repository.Old as Old ( oldRepoFailMsg )
import Darcs.Repository.PatchIndex
    ( createOrUpdatePatchIndexDisk
    , doesPatchIndexExist
    )
import Darcs.Repository.Paths
    ( indexInvalidPath
    , indexPath
    , tentativeHashedInventoryPath
    )
import Darcs.Repository.Pending ( finalizePending, revertPending )
import Darcs.Repository.Rebase
    ( extractOldStyleRebase
    , finalizeTentativeRebase
    , readTentativeRebase
    , revertTentativeRebase
    , updateRebaseFormat
    , writeTentativeRebase
    )
import Darcs.Repository.State ( updateIndex )
import Darcs.Repository.Unrevert
    ( finalizeTentativeUnrevert
    , revertTentativeUnrevert
    )

import Darcs.Util.Printer ( text, ($$) )
import Darcs.Util.Printer.Color ( ePutDocLn )
import Darcs.Util.Progress ( debugMessage )
import Darcs.Util.SignalHandler ( withSignalsBlocked )
import Darcs.Util.Tree ( Tree )


-- TODO: rename this and document the transaction protocol (revert/finalize)
-- clearly.
-- |Slightly confusingly named: as well as throwing away any tentative
-- changes, revertRepositoryChanges also re-initialises the tentative state.
-- It's therefore used before makign any changes to the repo.
revertRepositoryChanges :: RepoPatch p
                        => Repository 'RO p wU wR
                        -> IO (Repository 'RW p wU wR)
revertRepositoryChanges r
  | formatHas HashedInventory (repoFormat r) =
      withRepoDir r $ do
        checkIndexIsWritable
          `catchIOError` \e -> fail (unlines ["Cannot write index", show e])
        revertTentativeUnrevert
        revertPending r
        revertTentativeChanges r
        let r' = unsafeCoerceR r
        revertTentativeRebase r'
        return $ unsafeStartTransaction r'
  | otherwise = fail Old.oldRepoFailMsg

-- | Atomically copy the tentative state to the recorded state,
-- thereby committing the tentative changes that were made so far.
-- This includes inventories, pending, rebase, and the index.
finalizeRepositoryChanges :: (RepoPatch p, ApplyState p ~ Tree)
                          => Repository 'RW p wU wR
                          -> DryRun
                          -> IO (Repository 'RO p wU wR)
finalizeRepositoryChanges r dryrun
    | formatHas HashedInventory (repoFormat r) =
        withRepoDir r $ do
          let r' = unsafeEndTransaction $ unsafeCoerceR r
          when (dryrun == NoDryRun) $ do
            debugMessage "Finalizing changes..."
            withSignalsBlocked $ do
                updateRebaseFormat r
                finalizeTentativeRebase
                finalizeTentativeChanges r
                finalizePending r
                finalizeTentativeUnrevert
            debugMessage "Done finalizing changes..."
            ps <- readPatches r'
            pi_exists <- doesPatchIndexExist (repoLocation r')
            when pi_exists $
              createOrUpdatePatchIndexDisk r' ps
              `catchIOError` \e ->
                hPutStrLn stderr $ "Cannot create or update patch index: "++ show e
            updateIndex r'
          return r'
    | otherwise = fail Old.oldRepoFailMsg

-- | Upgrade a possible old-style rebase in progress to the new style.
upgradeOldStyleRebase :: forall p wU wR.
                         (RepoPatch p, ApplyState p ~ Tree)
                      => Repository 'RW p wU wR -> IO ()
upgradeOldStyleRebase repo = do
  PatchSet (ts :: RL (Tagged p) Origin wX) _ <- readTentativePatches repo
  Sealed wps <-
    readOneInventory @(W.WrappedNamed p) (repoCache repo) tentativeHashedInventoryPath
  case extractOldStyleRebase wps of
    Nothing ->
      ePutDocLn $ text "No old-style rebase state found, no upgrade needed."
    Just ((ps :: RL (PatchInfoAnd p) wX wZ) :> Dup r) -> do
      -- low-level call, must not try to update an existing rebase patch,
      -- nor update anything else beside the inventory
      writeTentativeInventory repo (PatchSet ts ps)
      Items old_r <- readTentativeRebase repo
      case old_r of
        NilFL -> do
          writeTentativeRebase (unsafeCoerceR repo) r
          repo' <-
            modifyRepoFormat
              (addToFormat RebaseInProgress_2_16 . removeFromFormat RebaseInProgress)
              repo
          void $ finalizeRepositoryChanges repo' NoDryRun
        _ -> do
          ePutDocLn
            $  "A new-style rebase is already in progress, not overwriting it."
            $$ "This should not have happened! This is the old-style rebase I found"
            $$ "and removed from the repository:"
            $$ showSuspended ForDisplay r

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
