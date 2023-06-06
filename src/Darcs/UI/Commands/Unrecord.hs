--  Copyright (C) 2002-2005 David Roundy
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2, or (at your option)
--  any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; see the file COPYING.  If not, write to
--  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
--  Boston, MA 02110-1301, USA.

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module Darcs.UI.Commands.Unrecord
    ( unrecord
    , unpull
    , obliterate
    ) where

import Darcs.Prelude

import Control.Monad ( unless, void, when )
import Darcs.Util.Tree ( Tree )
import Data.Maybe ( fromJust, isJust )
import System.Directory ( doesPathExist )
import System.Exit ( exitSuccess )

import Darcs.Patch ( RepoPatch, commute, effect, invert )
import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch.Bundle ( makeBundle, minContext )
import Darcs.Patch.CommuteFn ( commuterFLId )
import Darcs.Patch.Depends ( removeFromPatchSet )
import Darcs.Patch.PatchInfoAnd ( hopefully, patchDesc )
import Darcs.Patch.Permutations ( genCommuteWhatWeCanFL )
import Darcs.Patch.Set ( Origin, PatchSet )
import Darcs.Patch.Witnesses.Ordered ( FL(..), mapFL_FL, nullFL, (:>)(..), (+>+) )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..) )
import Darcs.Repository
    ( PatchInfoAnd
    , RepoJob(..)
    , applyToWorking
    , finalizeRepositoryChanges
    , readPatches
    , setTentativePending
    , tentativelyRemovePatches
    , unrecordedChanges
    , withRepoLock
    )
import Darcs.Repository.Flags ( UpdatePending(..) )
import Darcs.UI.Commands
    ( DarcsCommand(..)
    , amInHashedRepository
    , commandAlias
    , nodefaults
    , putFinished
    , putInfo
    , putVerbose
    , setEnvDarcsPatches
    , withStdOpts
    )
import Darcs.UI.Commands.Util
    ( getUniqueDPatchName
    , historyEditHelp
    , preselectPatches
    , printDryRunMessageAndExit
    )
import Darcs.UI.Completion ( noArgs )
import Darcs.UI.Flags
    ( DarcsFlag
    , changesReverse
    , compress
    , diffingOpts
    , dryRun
    , getOutput
    , isInteractive
    , minimize
    , selectDeps
    , umask
    , useCache
    , verbosity
    , xmlOutput
    )
import Darcs.UI.Options ( (?), (^) )
import qualified Darcs.UI.Options.All as O
import Darcs.UI.PrintPatch ( printFriendly )
import Darcs.UI.SelectChanges ( WhichChanges(..), runSelection, selectionConfig )
import qualified Darcs.UI.SelectChanges as S ( PatchSelectionOptions(..) )
import Darcs.Util.English ( presentParticiple )
import Darcs.Util.Lock ( writeDocBinFile )
import Darcs.Util.Path ( AbsolutePath, toFilePath, useAbsoluteOrStd )
import Darcs.Util.Printer ( Doc, formatWords, putDoc, sentence, text, ($+$), (<+>) )
import Darcs.Util.Progress ( debugMessage )
import Darcs.Util.Prompt ( promptYorn )
import Darcs.Util.SignalHandler ( catchInterrupt, withSignalsBlocked )

unrecordDescription :: String
unrecordDescription =
  "Remove recorded patches without changing the working tree."

unrecordHelp :: Doc
unrecordHelp = formatWords
  [ "Unrecord does the opposite of record: it deletes patches from"
  , "the repository without changing the working tree. The changes"
  , "are now again visible with `darcs whatsnew` and you can record"
  , "or revert them as you please."
  ]
  $+$ historyEditHelp

unrecord :: DarcsCommand
unrecord = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "unrecord"
    , commandHelp = unrecordHelp
    , commandDescription = unrecordDescription
    , commandExtraArgs = 0
    , commandExtraArgHelp = []
    , commandCommand = unrecordCmd
    , commandPrereq = amInHashedRepository
    , commandCompleteArgs = noArgs
    , commandArgdefaults = nodefaults
    , commandOptions = unrecordOpts
    }
  where
    unrecordBasicOpts
      = O.notInRemote
      ^ O.matchSeveralOrLast
      ^ O.selectDeps
      ^ O.interactive -- True
      ^ O.repoDir
    unrecordAdvancedOpts
      = O.compress
      ^ O.umask
      ^ O.changesReverse
    unrecordOpts = unrecordBasicOpts `withStdOpts` unrecordAdvancedOpts

unrecordCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
unrecordCmd _ opts _ =
  withRepoLock (useCache ? opts) (umask ? opts) $ RepoJob $ \_repository -> do
    (_ :> removal_candidates) <- preselectPatches opts _repository
    let direction = if changesReverse ? opts then Last else LastReversed
        selection_config =
          selectionConfig direction "unrecord" (patchSelOpts opts) Nothing Nothing
    (_ :> to_unrecord) <- runSelection removal_candidates selection_config
    when (nullFL to_unrecord) $ do
      putInfo opts "No patches selected!"
      exitSuccess
    putVerbose opts $
      text "About to write out (potentially) modified patches..."
    setEnvDarcsPatches to_unrecord
    _repository <-
      tentativelyRemovePatches _repository (compress ? opts)
        YesUpdatePending to_unrecord
    _ <- finalizeRepositoryChanges _repository (compress ? opts) (O.dryRun ? opts)
    putInfo opts "Finished unrecording."

unpullDescription :: String
unpullDescription =
  "Opposite of pull; unsafe if patch is not in remote repository."

unpullHelp :: Doc
unpullHelp =
  text $ "Unpull is an alias for what is nowadays called `obliterate`."

unpull :: DarcsCommand
unpull =
  (commandAlias "unpull" Nothing obliterate)
    { commandHelp = unpullHelp
    , commandDescription = unpullDescription
    , commandCommand = obliterateCmd "unpull"
    }

obliterateDescription :: String
obliterateDescription = "Delete selected patches from the repository."

obliterateHelp :: Doc
obliterateHelp = formatWords
  [ "Obliterate completely removes recorded patches from your local"
  , "repository. The changes will be undone in your working tree and the"
  , "patches will not be shown in your changes list anymore. Beware that"
  , "you can lose precious code by obliterating!"
  ]
  $+$ formatWords
  [ "One way to save obliterated patches is to use the -O flag. A patch"
  , "bundle will be created locally, that you will be able to apply"
  , "later to your repository with `darcs apply`. See `darcs send` for"
  , "a more detailed description."
  ]
  $+$ historyEditHelp

obliterate :: DarcsCommand
obliterate = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "obliterate"
    , commandHelp = obliterateHelp
    , commandDescription = obliterateDescription
    , commandExtraArgs = 0
    , commandExtraArgHelp = []
    , commandCommand = obliterateCmd "obliterate"
    , commandPrereq = amInHashedRepository
    , commandCompleteArgs = noArgs
    , commandArgdefaults = nodefaults
    , commandOptions = obliterateOpts
    }
  where
    obliterateBasicOpts
      = O.notInRemote
      ^ O.matchSeveralOrLast
      ^ O.selectDeps
      ^ O.interactive
      ^ O.repoDir
      ^ O.withSummary
      ^ O.output
      ^ O.minimize
      ^ O.diffAlgorithm
      ^ O.dryRunXml
    obliterateAdvancedOpts
      = O.compress
      ^ O.umask
      ^ O.changesReverse
    obliterateOpts = obliterateBasicOpts `withStdOpts` obliterateAdvancedOpts

obliterateCmd
  :: String -> (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
obliterateCmd cmdname _ opts _ = do
  let verbOpt = verbosity ? opts
  withRepoLock (useCache ? opts) (umask ? opts) $ RepoJob $ \_repository -> do
    unrecorded <- unrecordedChanges (diffingOpts opts) _repository Nothing
    (_ :> removal_candidates) <- preselectPatches opts _repository
    let direction = if changesReverse ? opts then Last else LastReversed
        selection_config =
          selectionConfig direction cmdname (patchSelOpts opts) Nothing Nothing
    (_ :> removed) <- runSelection removal_candidates selection_config
    when (nullFL removed) $ do
      putInfo opts "No patches selected!"
      exitSuccess
    case genCommuteWhatWeCanFL (commuterFLId commute) (effect removed :> unrecorded) of
      unrecorded' :> removed_after_unrecorded :> to_revert -> do
        effect_removed <-
          case to_revert of
            NilFL -> return removed_after_unrecorded
            _ ->
              if isInteractive True opts then do
                putStrLn $
                  "These unrecorded changes conflict with the " ++ cmdname ++ ":"
                printFriendly O.Verbose O.NoSummary to_revert
                yes <- promptYorn "Do you want to revert these unrecorded changes?"
                if yes then
                  return $ removed_after_unrecorded +>+ to_revert
                else do
                  putStrLn $ "Okay, " ++ cmdname ++ " cancelled."
                  exitSuccess
              else
                fail $
                  "Can't " ++ cmdname ++
                    " these patches without reverting some unrecorded changes."
        printDryRunMessageAndExit
          "obliterate" verbOpt (O.withSummary ? opts) (dryRun ? opts)
          (xmlOutput ? opts) (isInteractive True opts) removed
        setEnvDarcsPatches removed
        when (isJust $ getOutput opts "") $
          -- The call to preselectPatches above may have unwrapped the latest
          -- clean tag. If we don't want to remove it, we lost information
          -- about that tag being clean, so we have to access it's inventory.
          -- To avoid that, and thus preserve laziness, we re-read our original
          -- patchset and use that to create the context for the bundle.
          readPatches _repository >>= savetoBundle opts removed
        _repository <-
          tentativelyRemovePatches _repository (compress ? opts)
            NoUpdatePending removed
        -- rely on sifting to commute out prims not belonging in pending:
        setTentativePending _repository unrecorded'
        withSignalsBlocked $ do
          _repository <-
            finalizeRepositoryChanges _repository (compress ? opts) (O.dryRun ? opts)
          debugMessage "Applying patches to working tree..."
          unless (O.yes (O.dryRun ? opts)) $
            void $ applyToWorking _repository verbOpt (invert effect_removed)
        putFinished opts (presentParticiple cmdname)

savetoBundle
  :: (RepoPatch p, ApplyState p ~ Tree)
  => [DarcsFlag]
  -> FL (PatchInfoAnd p) wX wR
  -> PatchSet p Origin wR
  -> IO ()
savetoBundle _ NilFL _ = return ()
savetoBundle opts removed@(x :>: _) orig = do
  let kept = fromJust $ removeFromPatchSet removed orig
      genFullBundle = makeBundle Nothing kept (mapFL_FL hopefully removed)
  bundle <-
    if not (minimize ? opts)
      then genFullBundle
      else do
        putInfo opts
          "Minimizing context, to generate bundle with full context hit ctrl-C..."
        (case minContext kept removed of
          Sealed (kept' :> removed') ->
            makeBundle Nothing kept' (mapFL_FL hopefully removed'))
          `catchInterrupt` genFullBundle
  filename <- getUniqueDPatchName (patchDesc x)
  let outname = fromJust (getOutput opts filename)
  exists <- useAbsoluteOrStd (doesPathExist . toFilePath) (return False) outname
  when exists $
    fail $ "Directory or file named '" ++ (show outname) ++ "' already exists."
  useAbsoluteOrStd writeDocBinFile putDoc outname bundle
  putInfo opts $ sentence $
    useAbsoluteOrStd
      (("Saved patch bundle" <+>) . text . toFilePath)
      (text "stdout")
      outname

patchSelOpts :: [DarcsFlag] -> S.PatchSelectionOptions
patchSelOpts flags =
  S.PatchSelectionOptions
    { S.verbosity = verbosity ? flags
    , S.matchFlags = O.matchSeveralOrLast ? flags
    , S.interactive = isInteractive True flags
    , S.selectDeps = selectDeps ? flags
    , S.withSummary = O.withSummary ? flags
    }
