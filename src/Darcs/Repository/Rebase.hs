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
import Control.Monad ( unless, when )
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
    , showSuspended
    , simplifyPushes
    , removeFixupsFromSuspended
    )
import Darcs.Patch.Rebase.Fixup ( RebaseFixup(..) )
import Darcs.Patch.RepoPatch ( RepoPatch, PrimOf )
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
    , repoLocation
    )
import Darcs.Repository.Paths
    ( rebasePath
    , tentativeRebasePath
    , formatPath
    )

import Darcs.Util.Diff ( DiffAlgorithm(MyersDiff) )
import Darcs.Util.English ( englishNum, Noun(..) )
import Darcs.Util.Exception ( catchDoesNotExistError )
import Darcs.Util.Lock ( writeDocBinFile, readBinFile )
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

-- | got a rebase operation to run where it is required that a rebase is
-- already in progress
rebaseJob :: RepoPatch p
          => (Repository rt p wU wR -> IO a)
          -> Repository rt p wU wR
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
startRebaseJob :: RepoPatch p
               => (Repository rt p wU wR -> IO a)
               -> Repository rt p wU wR
               -> IO a
startRebaseJob job repo = do
    let rf = repoFormat repo
    unless (formatHas RebaseInProgress_2_16 rf) $
      writeRepoFormat (addToFormat RebaseInProgress_2_16 rf) formatPath
    rebaseJob job repo

checkSuspendedStatus :: RepoPatch p => Repository rt p wU wR -> IO ()
checkSuspendedStatus repo =
  -- This check is currently not needed as we call it only for RebaseJob
  -- and RebaseAwareJob which don't work with remote repos. Still, better
  -- no not have to make assumptions on how things are used.
  when (isValidLocalPath (repoLocation repo)) $ do
    -- This may be executed after transaction has been finalized,
    -- which is why we fall back to readRebase here.
    ps <- readTentativeRebase repo `catchDoesNotExistError` readRebase repo
    case countToEdit ps of
      0 -> do
        writeRepoFormat
          (removeFromFormat RebaseInProgress_2_16 $ repoFormat repo)
          formatPath
        putStrLn "Rebase finished!"
      n -> displaySuspendedStatus n

displaySuspendedStatus :: Int -> IO ()
displaySuspendedStatus 0 = return ()
displaySuspendedStatus count =
  ePutDocLn $ hsep
    [ "Rebase in progress:"
    , text (show count)
    , "suspended"
    , text (englishNum count (Noun "patch") "")
    ]

-- | Generic status display for non-rebase commands.
maybeDisplaySuspendedStatus :: RepoPatch p
                            => Repository rt p wU wR
                            -> IO ()
maybeDisplaySuspendedStatus repo =
  -- Called after every RepoJob, so the repoLocation may indeed
  -- be a remote URL (e.g. darcs log)
  when (isValidLocalPath (repoLocation repo)) $ do
    -- This may be executed after transaction has been finalized,
    -- which is why we fall back to readRebase here.
    -- Furthermore, it may be called if there is no rebase in progress,
    -- which is why we can't even rely on 'rebasePath' to exist.
    ps <-
      readTentativeRebase repo `catchDoesNotExistError`
        readRebase repo `catchDoesNotExistError` return (Items NilFL)
    displaySuspendedStatus (countToEdit ps)

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
  writeDocBinFile (repoLocation r </> path) (showSuspended ForStorage sp)

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
