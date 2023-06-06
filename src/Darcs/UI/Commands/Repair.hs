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
module Darcs.UI.Commands.Repair ( repair, check ) where

import Darcs.Prelude

import Control.Monad ( when, unless, void )
import Data.Maybe ( isJust )
import System.IO.Error ( catchIOError )
import System.Exit ( exitFailure )
import System.Directory( renameFile )
import System.FilePath ( (<.>) )

import Darcs.UI.Commands
    ( DarcsCommand(..), withStdOpts, nodefaults
    , putInfo, putVerbose, putWarning, amInHashedRepository
    )
import Darcs.UI.Completion ( noArgs )
import Darcs.UI.Flags
    ( DarcsFlag, verbosity, umask, useIndex
    , useCache, compress, diffAlgorithm, quiet
    )
import Darcs.UI.Options ( DarcsOption, oid, (?), (^) )
import qualified Darcs.UI.Options.All as O

import Darcs.Repository.Paths ( indexPath )
import Darcs.Repository.Repair
    ( replayRepository, checkIndex, replayRepositoryInTemp
    , RepositoryConsistency(..)
    )
import Darcs.Repository
    ( withRepository, RepoJob(..)
    , withRepoLock, writePristine
    , finalizeRepositoryChanges
    )
import Darcs.Repository.Hashed ( writeTentativeInventory )
import Darcs.Repository.Pending ( setTentativePending )

import Darcs.Patch ( displayPatch )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..) )

import Darcs.Util.Printer ( Doc, text, ($$) )


repairDescription :: String
repairDescription = "Repair a corrupted repository."

repairHelp :: Doc
repairHelp = text $
  "The `darcs repair` command attempts to fix corruption in the current\n\
  \repository.\n\
  \It works by successively applying all patches in the repository to an\n\
  \empty tree, each time checking that the patch can be cleanly applied\n\
  \to the current pristine tree. If we detect a problem, we try to repair\n\
  \the patch. Finally we compare the existing pristine with the newly\n\
  \reconstructed one and if they differ, replace the existing one.\n\
  \Any problem encountered is reported.\n\
  \The flag `--dry-run` makes this operation read-only and causes it to\n\
  \exit unsuccessfully (with a non-zero exit status) in case any problems\n\
  \are enountered.\n"

commonBasicOpts :: DarcsOption a
                   (Maybe String -> O.DiffAlgorithm -> a)
commonBasicOpts = O.repoDir ^ O.diffAlgorithm

repair :: DarcsCommand
repair = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "repair"
    , commandHelp = repairHelp
    , commandDescription = repairDescription
    , commandExtraArgs = 0
    , commandExtraArgHelp = []
    , commandCommand = withFpsAndArgs repairCmd
    , commandPrereq = amInHashedRepository
    , commandCompleteArgs = noArgs
    , commandArgdefaults = nodefaults
    , ..
    }
  where
    basicOpts = commonBasicOpts ^ O.dryRun
    advancedOpts = O.umask
    allOpts = basicOpts `withStdOpts` advancedOpts
    commandOptions = allOpts

withFpsAndArgs :: (b -> d) -> a -> b -> c -> d
withFpsAndArgs cmd _ opts _ = cmd opts

maybeDo :: Monad m => Maybe t -> (t -> m ()) -> m ()
maybeDo (Just x) f = f x
maybeDo Nothing _ = return ()

repairCmd :: [DarcsFlag] -> IO ()
repairCmd opts
  | O.yes (O.dryRun ? opts) = checkCmd opts
  | otherwise =
    withRepoLock (useCache ? opts) (umask ? opts) $
    RepoJob $ \repo -> do
      bad_replay <- replayRepository
        (diffAlgorithm ? opts)
        repo
        (compress ? opts)
        (verbosity ? opts) $ \RepositoryConsistency {..} -> do
          maybeDo fixedPatches $ \ps -> do
            putInfo opts "Writing out repaired patches..."
            writeTentativeInventory repo (compress ? opts) ps
          maybeDo fixedPristine $ \(tree, Sealed diff) -> do
            putVerbose opts $ "Pristine differences:" $$ displayPatch diff
            putInfo opts "Fixing pristine tree..."
            void $ writePristine repo tree
          maybeDo fixedPending $ \(Sealed pend) -> do
            putInfo opts "Writing out repaired pending..."
            setTentativePending repo pend
          return $ isJust fixedPatches || isJust fixedPristine || isJust fixedPending
      index_ok <- checkIndex repo (quiet opts)
      unless index_ok $ do
        renameFile indexPath (indexPath <.> "bad")
        putInfo opts "Bad index discarded."
      if bad_replay || not index_ok
        then do
          void $
            finalizeRepositoryChanges repo
              (compress ? opts) (O.dryRun ? opts)
        else
          putInfo opts "The repository is already consistent, no changes made."

-- |check is an alias for repair, with implicit DryRun flag.
check :: DarcsCommand
check = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "check"
    , commandHelp = "See `darcs repair` for details."
    , commandExtraArgs = 0
    , commandExtraArgHelp = []
    , commandCommand = withFpsAndArgs checkCmd
    , commandPrereq = amInHashedRepository
    , commandCompleteArgs = noArgs
    , commandArgdefaults = nodefaults
    , ..
    }
  where
    basicOpts = commonBasicOpts
    advancedOpts = oid
    allOpts = basicOpts `withStdOpts` advancedOpts
    commandOptions = allOpts
    commandDescription = "Alias for `darcs " ++ commandName repair ++ " --dry-run'."

checkCmd :: [DarcsFlag] -> IO ()
checkCmd opts = withRepository (useCache ? opts) $ RepoJob $ \repository -> do
  RepositoryConsistency {..} <-
    replayRepositoryInTemp (diffAlgorithm ? opts) repository (compress ? opts) (verbosity ? opts)
  maybeDo fixedPatches $ \_ ->
    putInfo opts "Found broken patches."
  maybeDo fixedPristine $ \(_, Sealed diff) -> do
    putInfo opts "Found broken pristine tree."
    putVerbose opts $ "Differences:" $$ displayPatch diff
  maybeDo fixedPending $ \_ ->
    putInfo opts "Found broken pending."
  bad_index <-
    if useIndex ? opts == O.IgnoreIndex
      then return False
      else
        not <$> do
          checkIndex repository (quiet opts) `catchIOError` \e -> do
            putWarning opts ("Warning, cannot access the index:" $$ text (show e))
            return True
  when bad_index $ putInfo opts "Bad index."
  if isJust fixedPatches || isJust fixedPristine || isJust fixedPending || bad_index
    then exitFailure
    else putInfo opts "The repository is consistent!"
