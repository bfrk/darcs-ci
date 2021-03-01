--  Copyright (C) 2003-2005 David Roundy
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

module Darcs.UI.Commands.Unrevert ( unrevert ) where

import Darcs.Prelude

import Control.Monad ( when )

import Darcs.Patch ( commute )
import Darcs.Patch.Depends ( mergeThem )
import Darcs.Patch.Witnesses.Ordered ( (:>)(..), FL(..), Fork(..), (+>+) )
import Darcs.Patch.Witnesses.Sealed ( Sealed(Sealed) )
import Darcs.Repository
    ( RepoJob(..)
    , applyToWorking
    , considerMergeToWorking
    , finalizeRepositoryChanges
    , readPristine
    , readPatches
    , tentativelyAddToPending
    , unrecordedChanges
    , withRepoLock
    )
import Darcs.Repository.Flags
    ( AllowConflicts(..)
    , DryRun(NoDryRun)
    , ExternalMerge(..)
    , Reorder(..)
    , UpdatePending(..)
    , WantGuiPause(..)
    )
import Darcs.Repository.Unrevert ( unrevertPatchBundle, writeUnrevert )
import Darcs.UI.Commands
    ( DarcsCommand(..)
    , amInHashedRepository
    , nodefaults
    , putFinished
    , withStdOpts
    )
import Darcs.UI.Completion ( noArgs )
import Darcs.UI.Flags
    ( compress
    , diffingOpts
    , isInteractive
    , umask
    , useCache
    , verbosity
    , withContext
    )
import Darcs.UI.Flags ( DarcsFlag )
import Darcs.UI.Options ( (?), (^) )
import qualified Darcs.UI.Options.All as O
import Darcs.UI.SelectChanges
    ( WhichChanges(First)
    , runInvertibleSelection
    , selectionConfigPrim
    )
import qualified Darcs.UI.SelectChanges as S ( PatchSelectionOptions(..) )
import Darcs.Util.Path ( AbsolutePath )
import Darcs.Util.Printer ( Doc, text )
import Darcs.Util.Progress ( debugMessage )
import Darcs.Util.Prompt ( promptYorn )
import Darcs.Util.SignalHandler ( withSignalsBlocked )

unrevertDescription :: String
unrevertDescription =
 "Undo the last revert."

unrevertHelp :: Doc
unrevertHelp = text $
 "Unrevert is a rescue command in case you accidentally reverted\n" ++
 "something you wanted to keep (for example, typing `darcs rev -a`\n" ++
 "instead of `darcs rec -a`).\n" ++
 "\n" ++
 "This command may fail if the repository has changed since the revert\n" ++
 "took place.  Darcs will ask for confirmation before executing an\n" ++
 "interactive command that will DEFINITELY prevent unreversion.\n"

patchSelOpts :: [DarcsFlag] -> S.PatchSelectionOptions
patchSelOpts flags = S.PatchSelectionOptions
    { S.verbosity = verbosity ? flags
    , S.matchFlags = []
    , S.interactive = isInteractive True flags
    , S.selectDeps = O.PromptDeps -- option not supported, use default
    , S.withSummary = O.NoSummary -- option not supported, use default
    , S.withContext = withContext ? flags
    }

unrevert :: DarcsCommand
unrevert = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "unrevert"
    , commandHelp = unrevertHelp
    , commandDescription = unrevertDescription
    , commandExtraArgs = 0
    , commandExtraArgHelp = []
    , commandCommand = unrevertCmd
    , commandPrereq = amInHashedRepository
    , commandCompleteArgs = noArgs
    , commandArgdefaults = nodefaults
    , commandOptions = unrevertOpts
    }
  where
    unrevertBasicOpts
      = O.interactive -- True
      ^ O.repoDir
      ^ O.withContext
      ^ O.diffAlgorithm
    unrevertAdvancedOpts = O.umask
    unrevertOpts = unrevertBasicOpts `withStdOpts` unrevertAdvancedOpts

unrevertCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
unrevertCmd _ opts [] =
 withRepoLock NoDryRun (useCache ? opts) (umask ? opts) $ RepoJob $ \_repository -> do
  us <- readPatches _repository
  Sealed them <- unrevertPatchBundle us
  pristine <- readPristine _repository
  unrecorded <- unrecordedChanges (diffingOpts opts) _repository Nothing
  Sealed h_them <- return $ mergeThem us them
  Sealed pw <- considerMergeToWorking _repository "unrevert"
                      YesAllowConflictsAndMark
                      NoExternalMerge NoWantGuiPause
                      (compress ? opts) (verbosity ? opts) NoReorder
                      (diffingOpts opts)
                      (Fork us NilFL h_them)
  let selection_config =
        selectionConfigPrim
            First "unrevert" (patchSelOpts opts)
            Nothing Nothing (Just pristine)
  (to_unrevert :> to_keep) <- runInvertibleSelection pw selection_config
  tentativelyAddToPending _repository to_unrevert
  withSignalsBlocked $
      do _repository <- finalizeRepositoryChanges _repository YesUpdatePending (compress ? opts)
         _ <- applyToWorking _repository (verbosity ? opts) to_unrevert
         recorded <- readPatches _repository
         debugMessage "I'm about to writeUnrevert."
         case commute ((unrecorded +>+ to_unrevert) :> to_keep) of
           Nothing -> do
             yes <-
               promptYorn "You will not be able to unrevert this operation! Proceed?"
             when yes $ writeUnrevert recorded pristine NilFL -- i.e. remove unrevert
           Just (to_keep' :> _) -> writeUnrevert recorded pristine to_keep'
  putFinished opts "unreverting"
unrevertCmd _ _ _ = error "impossible case"
