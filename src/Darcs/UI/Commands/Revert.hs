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
module Darcs.UI.Commands.Revert ( revert, clean ) where

import Darcs.Prelude

import Control.Monad ( unless, when, void )

import Darcs.UI.Flags
    ( DarcsFlag
    , diffAlgorithm
    , diffingOpts
    , isInteractive
    , pathSetFromArgs
    , umask
    , useCache
    )
import Darcs.UI.Options ( (^), (?) )
import qualified Darcs.UI.Options.All as O
import Darcs.UI.Commands
    ( DarcsCommand(..)
    , amInHashedRepository
    , commandAlias
    , nodefaults
    , putInfo
    , putFinished
    , withStdOpts
    )
import Darcs.UI.Commands.Util ( announceFiles, filterExistingPaths )
import Darcs.Repository.Unrevert ( writeUnrevert )
import Darcs.UI.Completion ( modifiedFileArgs )

import Darcs.Util.Global ( debugMessage )
import Darcs.Util.Path ( AbsolutePath )
import Darcs.Util.Printer ( Doc, formatWords, vsep )
import Darcs.Util.SignalHandler ( withSignalsBlocked )
import Darcs.Repository
    ( RepoJob(..)
    , addToPending
    , finalizeRepositoryChanges
    , applyToWorking
    , readPatches
    , unrecordedChanges
    , withRepoLock
    )
import Darcs.Patch ( invert, commuteFL )
import Darcs.Patch.Permutations ( genCommuteWhatWeCanRL )
import Darcs.Patch.Split ( reversePrimSplitter )
import Darcs.Patch.Witnesses.Ordered
    ( FL(..)
    , (:>)(..)
    , nullFL
    , (+>>+)
    , reverseFL
    )
import Darcs.UI.SelectChanges
    ( WhichChanges(Last)
    , selectionConfigPrim
    , runInvertibleSelection
    )
import qualified Darcs.UI.SelectChanges as S ( PatchSelectionOptions(..) )


revertDescription :: String
revertDescription = "Discard unrecorded changes."

revertHelp :: Doc
revertHelp = vsep $ map formatWords
  [ [ "The `darcs revert` command discards unrecorded changes the working"
    , "tree.  As with `darcs record`, you will be asked which hunks (changes)"
    , "to revert.  The `--all` switch can be used to avoid such prompting. If"
    , "files or directories are specified, other parts of the working tree"
    , "are not reverted."
    ]
  , [ "In you accidentally reverted something you wanted to keep (for"
    , "example, typing `darcs rev -a` instead of `darcs rec -a`), you can"
    , "immediately run `darcs unrevert` to restore it.  This is only"
    , "guaranteed to work if the repository has not changed since `darcs"
    , "revert` ran."
    ]
  ]

patchSelOpts :: [DarcsFlag] -> S.PatchSelectionOptions
patchSelOpts flags = S.PatchSelectionOptions
    { S.verbosity = O.verbosity ? flags
    , S.matchFlags = []
    , S.interactive = isInteractive True flags
    , S.selectDeps = O.PromptDeps -- option not supported, use default
    , S.withSummary = O.NoSummary -- option not supported, use default
    }

revert :: DarcsCommand
revert = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "revert"
    , commandHelp = revertHelp
    , commandDescription = revertDescription
    , commandExtraArgs = -1
    , commandExtraArgHelp = ["[FILE or DIRECTORY]..."]
    , commandCommand = revertCmd
    , commandPrereq = amInHashedRepository
    , commandCompleteArgs = modifiedFileArgs
    , commandArgdefaults = nodefaults
    , commandOptions = opts
    }
  where
    basicOpts
      = O.interactive -- True
      ^ O.repoDir
      ^ O.diffAlgorithm
      ^ O.maybelookforadds False
    advancedOpts = O.umask
    opts = withStdOpts basicOpts advancedOpts

revertCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
revertCmd fps opts args =
  withRepoLock (useCache ? opts) (umask ? opts) $ RepoJob $ \_repository -> do
    existing_paths <- existingPaths _repository =<< pathSetFromArgs fps args
    announceFiles verbosity existing_paths "Reverting changes in"
    changes <- unrecordedChanges diffOpts _repository existing_paths
    case changes of
      NilFL -> putInfo opts "There are no changes to revert!"
      _ -> do
        let selection_config =
              selectionConfigPrim Last "revert" (patchSelOpts opts)
                (Just (reversePrimSplitter (diffAlgorithm ? opts)))
                existing_paths
        norevert :> torevert <- runInvertibleSelection changes selection_config
        if nullFL torevert
          then
            putInfo opts $
              "If you don't want to revert after all, that's fine with me!"
          else do
            withSignalsBlocked $ do
              addToPending _repository diffOpts $ invert torevert
              debugMessage "About to write the unrevert file."
              {- The user has split unrecorded into the sequence 'norevert'
                 then 'torevert', which is natural as the bit we keep in
                 unrecorded should have pristine as the context.

                 But the unrevert patch also needs to have pristine as the
                 context, not unrecorded (which can be changed by the user
                 at any time).

                 So we need to commute 'torevert' with 'norevert', and if
                 that fails then we need to keep some of 'norevert' in the
                 actual unrevert patch so it still makes sense. The use of
                 genCommuteWhatWeCanRL minimises the amount of 'norevert'
                 that we need to keep.
              -}
              case genCommuteWhatWeCanRL commuteFL (reverseFL norevert :> torevert) of
                deps :> torevert' :> _ -> do
                  recorded <- readPatches _repository
                  writeUnrevert recorded (deps +>>+ torevert')
              _repository <-
                finalizeRepositoryChanges _repository
                  (O.dryRun ? opts)
              debugMessage "About to apply to the working tree."
              unless (O.yes (O.dryRun ? opts)) $
                void $ applyToWorking _repository verbosity (invert torevert)
            putFinished opts "reverting"
  where
    verbosity = O.verbosity ? opts
    diffOpts = diffingOpts opts
    existingPaths repo paths = do
      paths' <- traverse (filterExistingPaths repo verbosity diffOpts) paths
      let paths'' = fmap snd paths'
      when (paths'' == Just []) $ fail "None of the paths you specified exist."
      return paths''

-- | An alias for 'revert -l' i.e. remove every (non-boring) file or change
-- that is not in pristine.
clean :: DarcsCommand
clean = alias
    { commandDescription = desc
    , commandHelp = vsep $ map formatWords
        [ [ "Remove unrecorded changes from the working tree."
          ]
        , [ "This is an alias for `darcs revert -l/--look-for-adds` which"
          , "means it works also on files that have not been added."
          , "You can additionally pass `--boring` to get rid of *every*"
          , "unrecorded file or directory."
          ]
        , [ "See description of `darcs revert` for more details."
          ]
        ]
    , commandOptions = opts
    }
  where
    alias = commandAlias "clean" Nothing revert
    desc = "Alias for `darcs " ++ commandName revert ++ " -l`."
    basicOpts
      = O.interactive -- True
      ^ O.repoDir
      ^ O.diffAlgorithm
      ^ O.maybelookforadds True
    advancedOpts = O.umask
    opts = withStdOpts basicOpts advancedOpts
