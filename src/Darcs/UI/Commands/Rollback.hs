--  Copyright (C) 2002-2004,2007 David Roundy
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

module Darcs.UI.Commands.Rollback ( rollback ) where

import Darcs.Prelude

import Control.Monad ( unless, when, void )
import System.Exit ( exitSuccess )

import Darcs.Patch.Match ( firstMatch )
import Darcs.Patch.PatchInfoAnd ( n2pia )
import Darcs.Patch ( canonizeFL, effect, invert )
import Darcs.Patch.Named ( anonymous )
import Darcs.Patch.Set ( emptyPatchSet, patchSet2FL )
import Darcs.Patch.Split ( reversePrimSplitter )
import Darcs.Patch.Witnesses.Ordered ( Fork(..), FL(..), (:>)(..), nullFL )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..) )
import Darcs.Repository ( withRepoLock, RepoJob(..),
                          applyToWorking, readPatches,
                          finalizeRepositoryChanges, addToPending,
                          considerMergeToWorking )
import Darcs.UI.Commands ( DarcsCommand(..), withStdOpts, nodefaults, setEnvDarcsPatches,
                           amInHashedRepository, putInfo )
import Darcs.UI.Commands.Util ( announceFiles, getLastPatches )
import Darcs.UI.Completion ( knownFileArgs )
import Darcs.UI.Flags ( DarcsFlag, verbosity, umask, useCache,
                        wantGuiPause, diffingOpts,
                        diffAlgorithm, isInteractive, pathSetFromArgs )
import Darcs.UI.Options ( parseFlags, (?), (^) )
import qualified Darcs.UI.Options.All as O
import Darcs.UI.SelectChanges ( WhichChanges(..),
                                selectionConfig, selectionConfigPrim,
                                runSelection, runInvertibleSelection )
import qualified Darcs.UI.SelectChanges as S ( PatchSelectionOptions(..) )
import Darcs.Util.Path ( AbsolutePath )
import Darcs.Util.Printer ( Doc, text )
import Darcs.Util.Progress ( debugMessage )
import Darcs.Util.SignalHandler ( withSignalsBlocked )


rollbackDescription :: String
rollbackDescription =
 "Apply the inverse of recorded changes to the working tree."

rollbackHelp :: Doc
rollbackHelp = text $ unlines
    [ "Rollback is used to undo the effects of some changes from patches"
    , "in the repository. The selected changes are undone in your working"
    , "tree, but the repository is left unchanged. First you are offered a"
    , "choice of which patches to undo, then which changes within the"
    , "patches to undo."
    , ""
    , "Before doing `rollback`, you may want to temporarily undo the changes"
    , "of your working tree (if there are) and save them for later use."
    , "To do so, you can run `revert`, then run `rollback`, record a patch,"
    , "and run `unrevert` to restore the saved changes into your working tree."
    ]

patchSelOpts :: [DarcsFlag] -> S.PatchSelectionOptions
patchSelOpts flags = S.PatchSelectionOptions
    { S.verbosity = verbosity ? flags
    , S.matchFlags = parseFlags O.matchSeveralOrLast flags
    , S.interactive = isInteractive True flags
    , S.selectDeps = O.PromptDeps
    , S.withSummary = O.NoSummary
    }

rollback :: DarcsCommand
rollback = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "rollback"
    , commandHelp = rollbackHelp
    , commandDescription = rollbackDescription
    , commandExtraArgs = -1
    , commandExtraArgHelp = ["[FILE or DIRECTORY]..."]
    , commandCommand = rollbackCmd
    , commandPrereq = amInHashedRepository
    , commandCompleteArgs = knownFileArgs
    , commandArgdefaults = nodefaults
    , commandOptions = rollbackOpts
    }
  where
    rollbackBasicOpts
      = O.matchSeveralOrLast
      ^ O.interactive -- True
      ^ O.repoDir
      ^ O.diffAlgorithm
    rollbackAdvancedOpts = O.umask
    rollbackOpts = rollbackBasicOpts `withStdOpts` rollbackAdvancedOpts

exitIfNothingSelected :: FL p wX wY -> String -> IO ()
exitIfNothingSelected ps what =
    when (nullFL ps) $ putStrLn ("No " ++ what ++ " selected!") >> exitSuccess

rollbackCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
rollbackCmd fps opts args = withRepoLock (useCache ? opts)
    (umask ? opts) $ RepoJob $ \_repo -> do
        files <- pathSetFromArgs fps args
        announceFiles (verbosity ? opts) files "Rolling back changes in"
        allpatches <- readPatches _repo
        let matchFlags = parseFlags O.matchSeveralOrLast opts
        (_ :> patches) <- return $
            if firstMatch matchFlags
                then getLastPatches matchFlags allpatches
                else emptyPatchSet :> patchSet2FL allpatches
        (_ :> ps) <-
            runSelection patches $
                selectionConfig LastReversed "rollback" (patchSelOpts opts) Nothing files

        exitIfNothingSelected ps "patches"
        setEnvDarcsPatches ps
        let prim_selection_context =
              selectionConfigPrim
                  Last "rollback" (patchSelOpts opts)
                  (Just (reversePrimSplitter (diffAlgorithm ? opts)))
                  files
            hunks = canonizeFL (diffAlgorithm ? opts) . effect
        _ :> to_undo <- runInvertibleSelection (hunks ps) prim_selection_context
        exitIfNothingSelected to_undo "changes"
        -- Note: use of anonymous is unproblematic here because we
        -- only store effects by adding them to pending and working)
        rbp <- n2pia `fmap` anonymous (invert to_undo)
        Sealed pw <- considerMergeToWorking _repo "rollback"
                         (O.YesAllowConflicts O.MarkConflicts)
                         (wantGuiPause opts)
                         O.NoReorder
                         (diffingOpts opts)
                         (Fork allpatches NilFL (rbp :>: NilFL))
        addToPending _repo (diffingOpts opts) pw
        withSignalsBlocked $ do
            _repo <- finalizeRepositoryChanges _repo (O.dryRun ? opts)
            unless (O.yes (O.dryRun ? opts)) $
              void $ applyToWorking _repo (verbosity ? opts) pw
        debugMessage "Finished applying unrecorded rollback patch"
        putInfo opts $ text "Changes rolled back in working tree"
