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

import Control.Monad ( unless, when, void )
import Data.Maybe( fromJust, isJust )
import Darcs.Util.Tree( Tree )
import System.Exit ( exitSuccess )

import Darcs.Prelude

import Darcs.Patch ( RepoPatch, invert, commute, effect )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Patch.Bundle ( makeBundle, minContext )
import Darcs.Patch.Depends ( removeFromPatchSet )
import Darcs.Patch.PatchInfoAnd ( hopefully, patchDesc )
import Darcs.Patch.Set ( PatchSet, Origin )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..) )
import Darcs.Patch.Witnesses.Ordered ( (:>)(..), mapFL_FL, nullFL, FL(..) )
import Darcs.Util.Path( useAbsoluteOrStd, AbsolutePath, toFilePath, doesPathExist )
import Darcs.Util.SignalHandler ( catchInterrupt, withSignalsBlocked )
import Darcs.Repository
    ( PatchInfoAnd
    , RepoJob(..)
    , applyToWorking
    , finalizeRepositoryChanges
    , readPatches
    , tentativelyRemovePatches
    , unrecordedChanges
    , withRepoLock
    )
import Darcs.Repository.Flags( UpdatePending(..) )
import Darcs.Util.Lock( writeDocBinFile )
import Darcs.UI.Commands ( DarcsCommand(..), withStdOpts, nodefaults, commandAlias
                         , putVerbose
                         , setEnvDarcsPatches, amInHashedRepository
                         , putInfo, putFinished )
import Darcs.UI.Commands.Util
    ( getUniqueDPatchName
    , printDryRunMessageAndExit
    , preselectPatches
    , historyEditHelp
    )
import Darcs.UI.Completion ( noArgs )
import Darcs.UI.Flags
    ( DarcsFlag, changesReverse, compress, verbosity, getOutput
    , useCache, dryRun, umask, minimize
    , diffingOpts, xmlOutput, isInteractive, selectDeps )
import Darcs.UI.Options ( (^), parseFlags, (?) )
import qualified Darcs.UI.Options.All as O
import Darcs.UI.SelectChanges ( WhichChanges(..),
                                selectionConfig, runSelection )
import qualified Darcs.UI.SelectChanges as S ( PatchSelectionOptions (..) )
import Darcs.Util.English ( presentParticiple )
import Darcs.Util.Printer ( Doc, formatWords, text, putDoc, sentence, (<+>), ($+$) )
import Darcs.Util.Progress ( debugMessage )

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
    withRepoLock (useCache ? opts) (umask ? opts) $
        RepoJob $ \_repository -> do
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
            _repository <- tentativelyRemovePatches _repository (compress ? opts)
                     YesUpdatePending to_unrecord
            _ <- finalizeRepositoryChanges _repository YesUpdatePending
                  (compress ? opts) (O.dryRun ? opts)
            putInfo opts "Finished unrecording."

unpullDescription :: String
unpullDescription =
    "Opposite of pull; unsafe if patch is not in remote repository."

unpullHelp :: Doc
unpullHelp = text $ "Unpull is an alias for what is nowadays called `obliterate`."

unpull :: DarcsCommand
unpull = (commandAlias "unpull" Nothing obliterate)
             { commandHelp = unpullHelp
             , commandDescription = unpullDescription
             , commandCommand = unpullCmd
             }

unpullCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
unpullCmd = genericObliterateCmd "unpull"

obliterateDescription :: String
obliterateDescription =
    "Delete selected patches from the repository."

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
    , commandCommand = obliterateCmd
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

obliterateCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
obliterateCmd = genericObliterateCmd "obliterate"

-- | genericObliterateCmd is the function that executes the "obliterate" and
-- "unpull" commands. The first argument is the name under which the command is
-- invoked (@unpull@ or @obliterate@).
genericObliterateCmd :: String
                     -> (AbsolutePath, AbsolutePath)
                     -> [DarcsFlag]
                     -> [String]
                     -> IO ()
genericObliterateCmd cmdname _ opts _ =
    let cacheOpt = useCache ? opts
        verbOpt = verbosity ? opts
    in withRepoLock cacheOpt (umask ? opts) $
        RepoJob $ \_repository -> do
            pend <- unrecordedChanges (diffingOpts opts) _repository Nothing
            (_ :> removal_candidates) <- preselectPatches opts _repository

            let direction = if changesReverse ? opts then Last else LastReversed
                selection_config =
                  selectionConfig direction cmdname (patchSelOpts opts) Nothing Nothing
            (_ :> removed) <-
                runSelection removal_candidates selection_config
            when (nullFL removed) $ do
                putInfo opts "No patches selected!"
                exitSuccess
            case commute (effect removed :> pend) of
                Nothing -> fail $ "Can't " ++ cmdname
                                  ++ " patch without reverting some "
                                  ++ "unrecorded change."
                Just (_ :> p_after_pending) -> do
                    printDryRunMessageAndExit "obliterate"
                      verbOpt
                      (O.withSummary ? opts)
                      (dryRun ? opts)
                      (xmlOutput ? opts)
                      (isInteractive True opts)
                      removed
                    setEnvDarcsPatches removed
                    when (isJust $ getOutput opts "") $
                        -- The call to preselectPatches above may have
                        -- unwrapped the latest clean tag. If we don't want to
                        -- remove it, we lost information about that tag being
                        -- clean, so we have to access it's inventory. To avoid
                        -- that, and thus preserve laziness, we re-read our
                        -- original patchset and use that to create the context
                        -- for the bundle.
                        readPatches _repository >>= savetoBundle opts removed
                    _repository <- tentativelyRemovePatches _repository
                        (compress ? opts) NoUpdatePending removed
                    withSignalsBlocked $ do
                        _repository <- finalizeRepositoryChanges _repository
                                        YesUpdatePending (compress ? opts) (O.dryRun ? opts)
                        debugMessage "Applying patches to working tree..."
                        unless (O.yes (O.dryRun ? opts)) $
                          void $ applyToWorking _repository verbOpt (invert p_after_pending)
                    putFinished opts (presentParticiple cmdname)

savetoBundle :: (RepoPatch p, ApplyState p ~ Tree)
             => [DarcsFlag]
             -> FL (PatchInfoAnd p) wX wR
             -> PatchSet p Origin wR
             -> IO ()
savetoBundle _ NilFL _ = return ()
savetoBundle opts removed@(x :>: _) orig = do
    let kept = fromJust $ removeFromPatchSet removed orig
        genFullBundle = makeBundle Nothing kept (mapFL_FL hopefully removed)
    bundle <- if not (minimize ? opts)
               then genFullBundle
               else do putInfo opts "Minimizing context, to generate bundle with full context hit ctrl-C..."
                       ( case minContext kept removed of
                           Sealed (kept' :> removed') -> makeBundle Nothing kept' (mapFL_FL hopefully removed') )
                      `catchInterrupt` genFullBundle
    filename <- getUniqueDPatchName (patchDesc x)
    let outname = fromJust (getOutput opts filename)
    exists <- useAbsoluteOrStd (doesPathExist . toFilePath) (return False) outname
    when exists $ fail $ "Directory or file named '" ++ (show outname) ++ "' already exists."
    useAbsoluteOrStd writeDocBinFile putDoc outname bundle
    putInfo opts $ sentence $
      useAbsoluteOrStd (("Saved patch bundle" <+>) . text . toFilePath) (text "stdout") outname

patchSelOpts :: [DarcsFlag] -> S.PatchSelectionOptions
patchSelOpts flags = S.PatchSelectionOptions
    { S.verbosity = verbosity ? flags
    , S.matchFlags = parseFlags O.matchSeveralOrLast flags
    , S.interactive = isInteractive True flags
    , S.selectDeps = selectDeps ? flags
    , S.withSummary = O.withSummary ? flags
    , S.withContext = O.NoContext
    }
