--  Copyright (C) 2002-2004 David Roundy
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
module Darcs.UI.Commands.WhatsNew
    ( whatsnew
    , status
    ) where

import Darcs.Prelude

import Control.Monad ( void, when )
import Control.Monad.Reader ( runReaderT )
import Control.Monad.State ( evalStateT, liftIO )
import Data.Maybe ( isJust )
import System.Exit ( ExitCode (..), exitSuccess, exitWith )

import Darcs.Patch
    ( PrimOf, PrimPatch, RepoPatch
    , applyToTree, plainSummaryPrims
    )
import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch.Choices ( mkPatchChoices, labelPatches, unLabel )
import Darcs.Patch.Commute ( Commute )
import Darcs.Patch.FileHunk ( IsHunk (..) )
import Darcs.Patch.Inspect ( PatchInspect (..) )
import Darcs.Patch.Permutations ( partitionRL )
import Darcs.Patch.Prim.Class ( PrimDetails (..) )
import Darcs.Patch.Show
    ( ShowContextPatch
    , ShowPatch(..)
    , ShowPatchBasic(..)
    , showPatch
    )
import Darcs.Patch.TouchesFiles ( chooseTouching )
import Darcs.Patch.Witnesses.Ordered
    ( (:>) (..), FL (..)
    , reverseFL, reverseRL
    )
import Darcs.Patch.Witnesses.Sealed
    ( Sealed (..), Sealed2 (..)
    , unFreeLeft
    )
import Darcs.Repository
    ( RepoJob (..), Repository, AccessType(RO)
    , readPristine
    , unrecordedChanges, withRepository
    )
import Darcs.Repository.Diff ( treeDiff )
import Darcs.Repository.Prefs ( filetypeFunction )
import Darcs.UI.Commands
    ( DarcsCommand(..), withStdOpts, amInRepository
    , commandAlias, nodefaults
    )
import Darcs.UI.Completion ( modifiedFileArgs )
import Darcs.UI.Commands.Util ( announceFiles, filterExistingPaths )
import Darcs.UI.External ( viewDocWith )
import Darcs.UI.Flags
    ( DarcsFlag, diffAlgorithm
    , useCache, pathSetFromArgs
    , verbosity, isInteractive
    , diffingOpts
    )
import Darcs.UI.Options ( (^), parseFlags, (?), oid )
import qualified Darcs.UI.Options.All as O
import Darcs.UI.PrintPatch ( contextualPrintPatchWithPager )
import Darcs.UI.SelectChanges
    ( InteractiveSelectionM, KeyPress (..)
    , WhichChanges (..)
    , initialSelectionState
    , backAll
    , backOne, currentFile
    , currentPatch, decide
    , decideWholeFile, helpFor
    , keysFor, prompt
    , selectionConfigPrim, skipMundane
    , skipOne
    )
import qualified Darcs.UI.SelectChanges as S ( PatchSelectionOptions (..) )
import Darcs.Util.Path ( AbsolutePath, AnchoredPath )
import Darcs.Util.Printer
    ( Doc, formatWords, putDocLn, putDocLnWith, renderString
    , text, vcat, ($+$)
    )
import Darcs.Util.Printer.Color ( fancyPrinters )
import Darcs.Util.Prompt ( PromptConfig (..), promptChar )
import Darcs.Util.Tree ( Tree )

patchSelOpts :: [DarcsFlag] -> S.PatchSelectionOptions
patchSelOpts flags = S.PatchSelectionOptions
    { S.verbosity = verbosity ? flags
    , S.matchFlags = []
    , S.interactive = isInteractive True flags
    , S.selectDeps = O.PromptDeps -- option not supported, use default
    , S.withSummary = getSummary flags
    }

-- lookForAdds and machineReadable set YesSummary
-- unless NoSummary was given expressly
-- (or by default e.g. status)
getSummary :: [DarcsFlag] -> O.WithSummary
getSummary flags = case O.maybeSummary Nothing ? flags of
  Just O.NoSummary -> O.NoSummary
  Just O.YesSummary -> O.YesSummary
  Nothing
    | O.yes (O.lookforadds ? flags) -> O.YesSummary
    | O.machineReadable ? flags -> O.YesSummary
    | otherwise -> O.NoSummary

whatsnew :: DarcsCommand
whatsnew = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "whatsnew"
    , commandHelp = whatsnewHelp
    , commandDescription = whatsnewDescription
    , commandExtraArgs = -1
    , commandExtraArgHelp = ["[FILE or DIRECTORY]..."]
    , commandCommand = whatsnewCmd
    , commandPrereq = amInRepository
    , commandCompleteArgs = modifiedFileArgs
    , commandArgdefaults = nodefaults
    , commandOptions = whatsnewOpts
    }
  where
    whatsnewBasicOpts
      = O.maybeSummary Nothing
      ^ O.withContext
      ^ O.machineReadable
      ^ O.maybelookforadds O.NoLookForAdds
      ^ O.lookforreplaces
      ^ O.lookformoves
      ^ O.diffAlgorithm
      ^ O.repoDir
      ^ O.interactive -- False
    whatsnewOpts = withStdOpts whatsnewBasicOpts oid

whatsnewDescription :: String
whatsnewDescription = "List unrecorded changes in the working tree."

whatsnewHelp :: Doc
whatsnewHelp =
  formatWords
  [ "The `darcs whatsnew` command lists unrecorded changes to the working"
  , "tree.  If you specify a set of files and directories, only unrecorded"
  , "changes to those files and directories are listed."
  ]
  $+$ formatWords
  [ "With the `--summary` option, the changes are condensed to one line per"
  , "file, with mnemonics to indicate the nature and extent of the change."
  , "The `--look-for-adds` option causes candidates for `darcs add` to be"
  , "included in the summary output.  WithSummary mnemonics are as follows:"
  ]
  -- TODO autoformat bullet lists
  $+$ vcat
  [ "  * `A f` and `A d/` respectively mean an added file or directory."
  , "  * `R f` and `R d/` respectively mean a removed file or directory."
  , "  * `M f -N +M rP` means a modified file, with `N` lines deleted, `M`"
  , "    lines added, and `P` lexical replacements."
  , "  * `f -> g` means a moved file or directory."
  , "  * `a f` and `a d/` respectively mean a new, but unadded, file or"
  , "    directory, when using `--look-for-adds`."
  , "  * An exclamation mark (!) as in `R! foo.c`, means the change"
  , "    conflicts with a change in an earlier patch. The phrase `duplicated`"
  , "    means the change is identical to a change in an earlier patch."
  ]
  $+$ formatWords
  [ "The `--machine-readable` option implies `--summary` while making it more"
  , "parsable. Modified files are only shown as `M f`, and moves are shown in"
  , "two lines: `F f` and `T g` (as in 'From f To g')."
  ]
  $+$ formatWords
  [ "By default, `darcs whatsnew` uses Darcs' internal format for changes."
  , "To see some context (unchanged lines) around each change, use the"
  , "`--unified` option. (This option has no effect in interactive mode.)"
  , "To view changes in conventional `diff` format, use"
  , "the `darcs diff` command; but note that `darcs diff` cannot properly"
  , "display changes when file renames are involved."
  ]
  $+$ formatWords
  [ "This command exits unsuccessfully (returns a non-zero exit status) if"
  , "there are no unrecorded changes."
  ]

whatsnewCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
whatsnewCmd fps opts args =
   withRepository (useCache ? opts) $ RepoJob $ \(repo :: Repository 'RO p wU wR) -> do
    existing_files <- do
      files <- pathSetFromArgs fps args
      files' <-
        traverse
          (filterExistingPaths repo (verbosity ? opts) (diffingOpts opts)) files
      let files'' = fmap snd files'
      when (files'' == Just []) $
        fail "None of the files you specified exist."
      return files''

    -- get all unrecorded changes, possibly including unadded or even boring
    -- files if the appropriate options were supplied
    Sealed allInterestingChanges <-
      filteredUnrecordedChanges (diffingOpts opts) repo existing_files

    -- get the recorded state
    pristine <- readPristine repo

    -- the case --look-for-adds and --summary must be handled specially
    -- in order to distinguish added and unadded files

    -- TODO: it would be nice if we could return the pair
    -- (noLookChanges,unaddedNewPathsPs) in one go and also
    -- with proper witnesses (e.g. as noLookChanges +>+ unaddedNewPathsPs)
    -- This would also obviate the need for samePatchType.
    Sealed noLookChanges <-
      if haveLookForAddsAndSummary
        then
          -- do *not* look for adds here:
          let dopts = diffingOpts opts
          in filteredUnrecordedChanges
                dopts {O.lookForAdds = O.NoLookForAdds} repo existing_files
        else return (Sealed NilFL)
    Sealed unaddedNewPathsPs <-
      if haveLookForAddsAndSummary
        then do
          noLookAddsTree <- applyAddPatchesToPristine noLookChanges pristine
          lookAddsTree <- applyAddPatchesToPristine allInterestingChanges pristine
          ftf <- filetypeFunction
          -- Return the patches that create files/dirs that aren't yet added.
          unFreeLeft <$> treeDiff (diffAlgorithm ? opts) ftf noLookAddsTree lookAddsTree
        else return (Sealed NilFL)
    -- avoid ambiguous typing for unaddedNewPathsPs:
    samePatchType noLookChanges unaddedNewPathsPs

    exitOnNoChanges allInterestingChanges
    announceFiles (verbosity ? opts) existing_files "What's new in"
    if maybeIsInteractive opts
      then
        runInteractive interactiveHunks (patchSelOpts opts) allInterestingChanges
      else
        if haveLookForAddsAndSummary
          then do
            printChanges pristine noLookChanges
            printUnaddedPaths unaddedNewPathsPs
          else do
            printChanges pristine allInterestingChanges
  where
    haveSummary = O.yes (getSummary opts)
    haveLookForAddsAndSummary = haveSummary && O.yes (O.lookforadds ? opts)

    -- Filter out hunk patches (leaving add patches) and return the tree
    -- resulting from applying the filtered patches to the pristine tree.
    applyAddPatchesToPristine ps pristine = do
        adds :> _ <- return $ partitionRL (isJust . isHunk) $ reverseFL ps
        applyToTree (reverseRL adds) pristine

    exitOnNoChanges :: FL p wX wY -> IO ()
    exitOnNoChanges NilFL = do putStrLn "No changes!"
                               exitWith $ ExitFailure 1
    exitOnNoChanges _ = return ()

    -- This function does nothing. Its purpose is to enforce the
    -- same patch type for the two passed FLs. This is necessary
    -- in order to avoid ambiguous typing for unaddedNewPathsPs.
    samePatchType :: FL p wX wY -> FL p wU wV -> IO ()
    samePatchType _ _ = return ()

    printUnaddedPaths :: PrimPatch p => FL p wX wY -> IO ()
    printUnaddedPaths NilFL = return ()
    printUnaddedPaths ps =
        putDocLn . lowercaseAs . renderString . (plainSummaryPrims False) $ ps

    -- Make any add markers lowercase, to distinguish new-but-unadded files
    -- from those that are unrecorded, but added.
    lowercaseAs x = vcat $ map (text . lowercaseA) $ lines x
    lowercaseA ('A' : x) = 'a' : x
    lowercaseA x = x

    -- Appropriately print changes, according to the passed flags.
    -- Note this cannot make distinction between unadded and added files.
    printChanges :: ( PrimPatch p, ApplyState p ~ Tree)
                 => Tree IO -> FL p wX wY
                 -> IO ()
    printChanges pristine changes
        | haveSummary = putDocLn $ plainSummaryPrims machineReadable changes
        | O.yes (O.withContext ? opts) = contextualPrintPatchWithPager pristine changes
        | otherwise = printPatchPager changes
     where machineReadable = parseFlags O.machineReadable opts

    -- return the unrecorded changes that affect an optional list of paths.
    filteredUnrecordedChanges :: forall rt p wR wU. (RepoPatch p, ApplyState p ~ Tree)
                              => O.DiffOpts
                              -> Repository rt p wU wR
                              -> Maybe [AnchoredPath]
                              -> IO (Sealed (FL (PrimOf p) wR))
    filteredUnrecordedChanges diffing repo paths =
        chooseTouching paths <$> unrecordedChanges diffing repo paths

-- | Runs the 'InteractiveSelectionM' code
runInteractive :: InteractiveSelectionM p wX wY () -- Selection to run
               -> S.PatchSelectionOptions
               -> FL p wX wY      -- A list of patches
               -> IO ()
runInteractive i patchsel ps' = do
    let lps' = labelPatches Nothing ps'
        choices' = mkPatchChoices lps'
        ps = evalStateT i (initialSelectionState lps' choices')
    void $
      runReaderT ps $
        selectionConfigPrim First "view" patchsel Nothing Nothing

-- | The interactive part of @darcs whatsnew@
interactiveHunks :: (IsHunk p, ShowPatch p, ShowContextPatch p, Commute p,
                     PatchInspect p, PrimDetails p, ApplyState p ~ Tree)
                 => InteractiveSelectionM p wX wY ()
interactiveHunks = do
    c <- currentPatch
    case c of
        Nothing -> liftIO $ putStrLn "No more changes!"
        Just (Sealed2 lp) -> do
            liftIO $ printPatchPager (unLabel lp)
            repeatThis lp
  where
    repeatThis lp = do
        thePrompt <- prompt -- "Shall I view this change? (n/m)"
        yorn <- liftIO $ promptChar
                (PromptConfig thePrompt (keysFor basic_options) (keysFor adv_options)
                 (Just 'n') "?h")
        case yorn of
            -- View change
            'v' -> liftIO (printPatch (unLabel lp))
                   >> repeatThis lp
            -- View summary of the change
            'x' -> liftIO (putDocLn $ summary $ unLabel lp)
                   >> repeatThis lp
            -- View change and move on
            'y' -> liftIO (printPatch (unLabel lp))
                   >> decide True lp >> next_hunk
            -- Go to the next patch
            'n' -> decide False lp >> next_hunk
            -- Skip the whole file
            's' -> do
                currentFile >>= maybe
                    (return ())
                    (\f -> decideWholeFile f False)
                next_hunk
            -- View change in a pager
            'p' -> liftIO (printPatchPager $ unLabel lp)
                   >> repeatThis lp
            -- Next change
            'j' -> next_hunk
            -- Previous change
            'k' -> prev_hunk
            -- Start from the first change
            'g' -> start_over
            -- Quit whatsnew
            'q' -> liftIO $ exitSuccess
            _ -> do liftIO . putStrLn $
                        helpFor "whatsnew" basic_options adv_options
                    repeatThis lp
    start_over = backAll >> interactiveHunks
    next_hunk  = skipOne >> skipMundane >> interactiveHunks
    prev_hunk  = backOne >> interactiveHunks
    options_yn =
        [ KeyPress 'v' "view this change"
        , KeyPress 'y' "view this change and go to the next one"
        , KeyPress 'n' "skip this change and its dependencies"
        ]
    optionsView =
        [ KeyPress 'p' "view this change with pager"
        , KeyPress 'x' "view a summary of this change"
        ]
    optionsNav =
        [ KeyPress 'q' "quit whatsnew"
        , KeyPress 's' "skip the rest of the changes to this file"
        , KeyPress 'j' "go to the next change"
        , KeyPress 'k' "back up to previous change"
        , KeyPress 'g' "start over from the first change"
        ]
    basic_options = [ options_yn ]
    adv_options = [ optionsView, optionsNav ]

printPatchPager :: ShowPatchBasic p => p wX wY -> IO ()
printPatchPager = viewDocWith fancyPrinters . showPatch

printPatch :: ShowPatchBasic p => p wX wY -> IO ()
printPatch = putDocLnWith fancyPrinters . showPatch

-- | An alias for 'whatsnew', with implicit @-l@ (and thus implicit @-s@)
-- flags. We override the default description, to include these flags.
status :: DarcsCommand
status = statusAlias
    { commandDescription = statusDesc
    , commandOptions = statusOpts
    }
  where
    statusAlias = commandAlias "status" Nothing whatsnew
    statusDesc = "Alias for `darcs " ++ commandName whatsnew ++ " -ls`."
    statusBasicOpts
      = O.maybeSummary (Just O.YesSummary)
      ^ O.withContext
      ^ O.machineReadable
      ^ O.maybelookforadds O.YesLookForAdds
      ^ O.lookforreplaces
      ^ O.lookformoves
      ^ O.diffAlgorithm
      ^ O.repoDir
      ^ O.interactive
    statusOpts = withStdOpts statusBasicOpts oid

maybeIsInteractive :: [DarcsFlag] -> Bool
maybeIsInteractive = maybe False id . parseFlags O.interactive
