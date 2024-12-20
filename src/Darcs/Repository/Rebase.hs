--  Copyright (C) 2009-2012 Ganesh Sittampalam
--
--  BSD3
{-# LANGUAGE OverloadedStrings #-}
module Darcs.Repository.Rebase
    ( -- * Create/read/write rebase patch
      readTentativeRebase
    , writeTentativeRebase
    , withTentativeRebase
    , readRebase
    , finalizeTentativeRebase
    , revertTentativeRebase
    , withManualRebaseUpdate
      -- * Handle rebase format and status
    , checkHasRebase
    , displayRebaseStatus
    , updateRebaseFormat
      -- * Handle old-style rebase
    , extractOldStyleRebase
    , checkOldStyleRebaseStatus
    ) where

import Darcs.Prelude

import Control.Monad ( unless, void, when )
import System.Directory ( copyFile, renameFile )
import System.Exit ( exitFailure )
import System.FilePath.Posix ( (</>) )

import qualified Darcs.Patch.Rebase.Legacy.Wrapped as W
import Darcs.Patch.PatchInfoAnd
    ( PatchInfoAnd
    , PatchInfoAndG
    , fmapPIAP
    , hopefully
    )
import Darcs.Patch.Rebase.Suspended
    ( Suspended(Items)
    , countToEdit
    , readSuspended
    , formatSuspended
    , simplifyPushes
    , removeFixupsFromSuspended
    )
import Darcs.Patch.Rebase.Fixup ( RebaseFixup(..) )
import Darcs.Patch.RepoPatch ( RepoPatch, PrimOf )
import Darcs.Patch.Witnesses.Ordered
    ( (:>)(..)
    , FL(..)
    , RL(..)
    , foldlwFL
    , mapRL_RL
    , (+<<+)
    )
import Darcs.Patch.Witnesses.Sealed ( Dup(..) )

import Darcs.Repository.Format
    ( RepoProperty ( RebaseInProgress_2_16, RebaseInProgress )
    , formatHas
    , addToFormat
    , removeFromFormat
    )
import Darcs.Repository.InternalTypes
    ( Repository
    , AccessType(..)
    , modifyRepoFormat
    , repoFormat
    , repoLocation
    )
import Darcs.Repository.Paths
    ( rebasePath
    , tentativeRebasePath
    )

import Darcs.Util.Diff ( DiffAlgorithm(MyersDiff) )
import Darcs.Util.English ( englishNum, Noun(..) )
import Darcs.Util.Exception ( catchDoesNotExistError )
import Darcs.Util.Lock ( writeFormatBinFile, readBinFile )
import Darcs.Util.Parser ( parse )
import Darcs.Util.Printer ( text, hsep, vcat )
import Darcs.Util.Printer.Color ( ePutDocLn )
import Darcs.Util.URL ( isValidLocalPath )

withManualRebaseUpdate
   :: RepoPatch p
   => Repository rt p wU wR
   -> (Repository rt p wU wR -> IO (Repository rt p wU wR', FL (RebaseFixup (PrimOf p)) wR' wR, x))
   -> IO (Repository rt p wU wR', x)
withManualRebaseUpdate r subFunc = do
    susp <- readTentativeRebase r
    (r', fixups, x) <- subFunc r
    when (countToEdit susp > 0) $
      -- HACK overwrite the changes that were made by subFunc
      -- which may and indeed does call add/remove patch
      writeTentativeRebase r' (simplifyPushes MyersDiff fixups susp)
    return (r', x)

-- | Fail if there is an old-style rebase present.
-- To be called initially for every command except rebase upgrade.
checkOldStyleRebaseStatus :: Repository rt p wU wR -> IO ()
checkOldStyleRebaseStatus repo = do
  let rf = repoFormat repo
  when (formatHas RebaseInProgress rf) $ do
      ePutDocLn upgradeMsg
      exitFailure
  where
    upgradeMsg = vcat
      [ "An old-style rebase is in progress in this repository. You can upgrade it"
      , "to the new format using the 'darcs rebase upgrade' command. The repository"
      , "format is unaffected by this, but you won't be able to use a darcs version"
      , "older than 2.16 on this repository until the current rebase is finished."
      ]

-- | Fail unless we already have some suspended patches.
-- Not essential, since all rebase commands should be happy to work
-- with an empty rebase state.
checkHasRebase :: Repository rt p wU wR -> IO ()
checkHasRebase repo =
  unless (formatHas RebaseInProgress_2_16 $ repoFormat repo) $
    fail "No rebase in progress. Try 'darcs rebase suspend' first."

-- | Report the rebase status if there is (still) a rebase in progress
-- after the command has finished running.
-- To be called via 'finally' for every 'RepoJob'.
displayRebaseStatus :: RepoPatch p => Repository rt p wU wR -> IO ()
displayRebaseStatus repo = do
  -- The repoLocation may be a remote URL (e.g. darcs log). We neither can nor
  -- want to display anything in that case.
  when (isValidLocalPath $ repoLocation repo) $ do
    -- Why do we use 'readRebase' and not 'readTentativeRebase' here?
    -- There are three cases:
    -- * We had no transaction in the first place.
    -- * We had a successful transaction: then it will be finalized before we
    --   are called (because finalization is part of the RepoJob itself) and
    --   we want to report the new finalized state.
    -- * We had a transaction that was cancelled or failed: then we want to
    --   report the old (unmodified) rebase state.
    -- Thus, in all cases 'readRebase' is the correct choice. However, if there
    -- is no rebase in progress, then 'rebasePath' may not exist, so we must
    -- handle that.
    suspended <- readRebase repo `catchDoesNotExistError` return (Items NilFL)
    case countToEdit suspended of
      0 -> return ()
      count ->
        ePutDocLn $ hsep
          [ "Rebase in progress:"
          , text (show count)
          , "suspended"
          , text (englishNum count (Noun "patch") "")
          ]

-- | Rebase format update for all commands that modify the repo,
-- except rebase upgrade. This is called by 'finalizeRepositoryChanges'.
updateRebaseFormat :: RepoPatch p => Repository 'RW p wU wR -> IO ()
updateRebaseFormat repo = do
  let rf = repoFormat repo
      hadRebase = formatHas RebaseInProgress_2_16 rf
  suspended <-
    readTentativeRebase repo `catchDoesNotExistError` return (Items NilFL)
  case countToEdit suspended of
    0 ->
      when hadRebase $ do
        void $ modifyRepoFormat (removeFromFormat RebaseInProgress_2_16) repo
        putStrLn "Rebase finished!"
    _ ->
      unless hadRebase $
        void $ modifyRepoFormat (addToFormat RebaseInProgress_2_16) repo

withTentativeRebase
  :: RepoPatch p
  => Repository rt p wU wR
  -> Repository rt p wU wR'
  -> (Suspended p wR -> Suspended p wR')
  -> IO ()
withTentativeRebase r r' f =
  readTentativeRebase r >>= writeTentativeRebase r' . f

readTentativeRebase :: RepoPatch p
                    => Repository rt p wU wR -> IO (Suspended p wR)
readTentativeRebase = readRebaseFile tentativeRebasePath

writeTentativeRebase :: RepoPatch p
                     => Repository rt p wU wR -> Suspended p wR -> IO ()
writeTentativeRebase = writeRebaseFile tentativeRebasePath

readRebase :: RepoPatch p => Repository rt p wU wR -> IO (Suspended p wR)
readRebase = readRebaseFile rebasePath

createTentativeRebase :: RepoPatch p => Repository rt p wU wR -> IO ()
createTentativeRebase r = writeRebaseFile tentativeRebasePath r (Items NilFL)

revertTentativeRebase :: RepoPatch p => Repository rt p wU wR -> IO ()
revertTentativeRebase repo =
  copyFile rebasePath tentativeRebasePath
    `catchDoesNotExistError` createTentativeRebase repo

finalizeTentativeRebase :: IO ()
finalizeTentativeRebase = renameFile tentativeRebasePath rebasePath

-- unsafe witnesses, not exported
readRebaseFile :: RepoPatch p
               => FilePath -> Repository rt p wU wR -> IO (Suspended p wX)
readRebaseFile path r = do
  parsed <- parse readSuspended <$> readBinFile (repoLocation r </> path)
  case parsed of
    Left e -> fail $ unlines ["parse error in file " ++ path, e]
    Right (result, _) -> return result

-- unsafe witnesses, not exported
writeRebaseFile :: RepoPatch p
                => FilePath -> Repository rt p wU wR
                -> Suspended p wR -> IO ()
writeRebaseFile path r sp =
  writeFormatBinFile (repoLocation r </> path) (formatSuspended sp)

type PiaW p = PatchInfoAndG (W.WrappedNamed p)

extractOldStyleRebase :: forall p wA wB. RepoPatch p
                      => RL (PiaW p) wA wB
                      -> Maybe ((RL (PatchInfoAnd p) :> Dup (Suspended p)) wA wB)
extractOldStyleRebase ps = go (ps :> NilFL) where
  go :: (RL (PiaW p) :> FL (PatchInfoAnd p)) wA wB
     -> Maybe ((RL (PatchInfoAnd p) :> Dup (Suspended p)) wA wB)
  go (NilRL :> _) = Nothing
  go (xs :<: x :> ys)
    | W.RebaseP _ r <- hopefully x = do
      let xs' = mapRL_RL (fmapPIAP W.fromRebasing) xs
          rffs = foldlwFL (removeFixupsFromSuspended . hopefully) ys
      return ((xs' +<<+ ys) :> Dup (rffs r))
    | otherwise = go (xs :> fmapPIAP W.fromRebasing x :>: ys)
