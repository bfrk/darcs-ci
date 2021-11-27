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
     -- * Support for various 'RepoJob's
    , withManualRebaseUpdate
    , rebaseJob
    , startRebaseJob
    , maybeDisplaySuspendedStatus
      -- * Handle old-style rebase
    , extractOldStyleRebase
    , checkOldStyleRebaseStatus
    ) where

import Darcs.Prelude

import Control.Exception ( finally )
import Control.Monad ( unless )
import System.Directory ( copyFile, renameFile )
import System.Exit ( exitFailure )

import Darcs.Patch.Apply ( ApplyState )
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
    , showSuspended
    , simplifyPushes
    , removeFixupsFromSuspended
    )
import Darcs.Patch.Rebase.Fixup ( RebaseFixup(..) )
import Darcs.Patch.RepoPatch ( RepoPatch, PrimOf )
import Darcs.Patch.RepoType
  ( RepoType(..), IsRepoType(..), SRepoType(..)
  , RebaseType(..), SRebaseType(..)
  )
import Darcs.Patch.Show ( ShowPatchFor(ForStorage) )
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
    , writeRepoFormat
    )
import Darcs.Repository.InternalTypes
    ( Repository
    , repoFormat
    , withRepoDir
    )
import Darcs.Repository.Paths
    ( rebasePath
    , tentativeRebasePath
    , formatPath
    )

import Darcs.Util.Diff ( DiffAlgorithm(MyersDiff) )
import Darcs.Util.English ( englishNum, Noun(..) )
import Darcs.Util.Exception ( catchDoesNotExistError, ifDoesNotExistError )
import Darcs.Util.Lock ( writeDocBinFile, readBinFile )
import Darcs.Util.Parser ( parse )
import Darcs.Util.Printer ( text, hsep, vcat )
import Darcs.Util.Printer.Color ( ePutDocLn )
import Darcs.Util.Tree ( Tree )

withManualRebaseUpdate
   :: forall rt p x wR wU wT1 wT2
    . (IsRepoType rt, RepoPatch p, ApplyState p ~ Tree)
   => Repository rt p wR wU wT1
   -> (Repository rt p wR wU wT1 -> IO (Repository rt p wR wU wT2, FL (RebaseFixup (PrimOf p)) wT2 wT1, x))
   -> IO (Repository rt p wR wU wT2, x)
withManualRebaseUpdate r subFunc
  | SRepoType SIsRebase <- singletonRepoType :: SRepoType rt = do
      susp <- readTentativeRebase r
      (r', fixups, x) <- subFunc r
      -- HACK overwrite the changes that were made by subFunc
      -- which may and indeed does call add/remove patch
      writeTentativeRebase r' (simplifyPushes MyersDiff fixups susp)
      return (r', x)
  | otherwise = do
      (r', _, x) <- subFunc r
      return (r', x)

checkOldStyleRebaseStatus :: RepoPatch p
                          => SRebaseType rebaseType
                          -> Repository ('RepoType rebaseType) p wR wU wR
                          -> IO ()
checkOldStyleRebaseStatus SNoRebase _    = return ()
checkOldStyleRebaseStatus SIsRebase repo = do
    -- if the format says we have a rebase in progress,
    -- but initially we have zero new-style suspended patches
    -- this means an old-style rebase is in progress
    count <-
      ifDoesNotExistError 0 $ countToEdit <$> readRebase repo
    unless (count > 0) $ do
      ePutDocLn upgradeMsg
      exitFailure
  where
    upgradeMsg = vcat
      [ "An old-style rebase is in progress in this repository. You can upgrade it"
      , "to the new format using the 'darcs rebase upgrade' command. The repository"
      , "format is unaffected by this, but you won't be able to use a darcs version"
      , "older than 2.16 on this repository until the current rebase is finished."
      ]

-- | got a rebase operation to run where it is required that a rebase is
-- already in progress
rebaseJob :: (RepoPatch p, ApplyState p ~ Tree)
          => (Repository ('RepoType 'IsRebase) p wR wU wR -> IO a)
          -> Repository ('RepoType 'IsRebase) p wR wU wR
          -> IO a
rebaseJob job repo = do
    job repo
      -- The use of finally here is because various things in job
      -- might cause an "expected" early exit leaving us needing
      -- to remove the rebase-in-progress state (e.g. when suspending,
      -- conflicts with recorded, user didn't specify any patches).
      --
      -- The better fix would be to standardise expected early exits
      -- e.g. using a layer on top of IO or a common Exception type
      -- and then just catch those.
      `finally` checkSuspendedStatus repo

-- | Got a rebase operation to run where we may need to initialise the
-- rebase state first. Make sure you have taken the lock before calling this.
startRebaseJob :: (RepoPatch p, ApplyState p ~ Tree)
               => (Repository ('RepoType 'IsRebase) p wR wU wR -> IO a)
               -> Repository ('RepoType 'IsRebase) p wR wU wR
               -> IO a
startRebaseJob job repo = do
    let rf = repoFormat repo
    if formatHas RebaseInProgress rf then
      checkOldStyleRebaseStatus SIsRebase repo
    else
      unless (formatHas RebaseInProgress_2_16 rf) $
        writeRepoFormat (addToFormat RebaseInProgress_2_16 rf) formatPath
    rebaseJob job repo

checkSuspendedStatus :: (RepoPatch p, ApplyState p ~ Tree)
                     => Repository ('RepoType 'IsRebase) p wR wU wR
                     -> IO ()
checkSuspendedStatus _repo = do
    ps <- readTentativeRebase _repo `catchDoesNotExistError` readRebase _repo
    case countToEdit ps of
         0 -> do
               writeRepoFormat
                  (removeFromFormat RebaseInProgress_2_16 $
                    repoFormat _repo)
                  formatPath
               putStrLn "Rebase finished!"
         n -> displaySuspendedStatus n

displaySuspendedStatus :: Int -> IO ()
displaySuspendedStatus count =
  ePutDocLn $ hsep
    [ "Rebase in progress:"
    , text (show count)
    , "suspended"
    , text (englishNum count (Noun "patch") "")
    ]

-- | Generic status display for non-rebase commands.
maybeDisplaySuspendedStatus :: RepoPatch p
                            => SRebaseType rebaseType
                            -> Repository ('RepoType rebaseType) p wR wU wR
                            -> IO ()
maybeDisplaySuspendedStatus SIsRebase repo = do
  ps <- readTentativeRebase repo `catchDoesNotExistError` readRebase repo
  displaySuspendedStatus (countToEdit ps)
maybeDisplaySuspendedStatus SNoRebase _    = return ()

withTentativeRebase
  :: RepoPatch p
  => Repository rt p wR wU wT
  -> Repository rt p wR wU wY
  -> (Suspended p wT -> Suspended p wY)
  -> IO ()
withTentativeRebase r r' f =
  readTentativeRebase r >>= writeTentativeRebase r' . f

readTentativeRebase :: RepoPatch p
                    => Repository rt p wR wU wT -> IO (Suspended p wT)
readTentativeRebase = readRebaseFile tentativeRebasePath

writeTentativeRebase :: RepoPatch p
                     => Repository rt p wR wU wT -> Suspended p wT -> IO ()
writeTentativeRebase = writeRebaseFile tentativeRebasePath

readRebase :: RepoPatch p => Repository rt p wR wU wR -> IO (Suspended p wR)
readRebase = readRebaseFile rebasePath

createTentativeRebase :: RepoPatch p => Repository rt p wR wU wR -> IO ()
createTentativeRebase r = writeRebaseFile tentativeRebasePath r (Items NilFL)

revertTentativeRebase :: RepoPatch p => Repository rt p wR wU wR -> IO ()
revertTentativeRebase repo =
  copyFile rebasePath tentativeRebasePath
    `catchDoesNotExistError` createTentativeRebase repo

finalizeTentativeRebase :: IO ()
finalizeTentativeRebase = renameFile tentativeRebasePath rebasePath

-- unsafe witnesses, not exported
readRebaseFile :: RepoPatch p
               => FilePath -> Repository rt p wR wU wT -> IO (Suspended p wX)
readRebaseFile path r =
  withRepoDir r $ do
    parsed <- parse readSuspended <$> readBinFile path
    case parsed of
      Left e -> fail $ unlines ["parse error in file " ++ path, e]
      Right (result, _) -> return result

-- unsafe witnesses, not exported
writeRebaseFile :: RepoPatch p
                => FilePath -> Repository rt p wR wU wT
                -> Suspended p wT -> IO ()
writeRebaseFile path r sp =
  withRepoDir r $
    writeDocBinFile path (showSuspended ForStorage sp)

type PiaW rt p = PatchInfoAndG rt (W.WrappedNamed rt p)

extractOldStyleRebase :: forall rt p wA wB. RepoPatch p
                      => RL (PiaW rt p) wA wB
                      -> Maybe ((RL (PatchInfoAnd rt p) :> Dup (Suspended p)) wA wB)
extractOldStyleRebase ps = go (ps :> NilFL) where
  go :: (RL (PiaW rt p) :> FL (PatchInfoAnd rt p)) wA wB
     -> Maybe ((RL (PatchInfoAnd rt p) :> Dup (Suspended p)) wA wB)
  go (NilRL :> _) = Nothing
  go (xs :<: x :> ys)
    | W.RebaseP _ r <- hopefully x = do
      let xs' = mapRL_RL (fmapPIAP W.fromRebasing) xs
          rffs = foldlwFL (removeFixupsFromSuspended . hopefully) ys
      return ((xs' +<<+ ys) :> Dup (rffs r))
    | otherwise = go (xs :> fmapPIAP W.fromRebasing x :>: ys)
