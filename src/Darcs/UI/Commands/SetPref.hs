--  Copyright (C) 2003 David Roundy
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

module Darcs.UI.Commands.SetPref ( setpref ) where

import Darcs.Prelude

import System.Exit ( exitWith, ExitCode(..) )
import Control.Monad ( when, void )
import Data.Maybe ( fromMaybe )

import Darcs.UI.Commands
    ( DarcsCommand(..)
    , amInHashedRepository
    , nodefaults
    , withStdOpts
    )
import Darcs.UI.Flags ( DarcsFlag, diffingOpts, useCache, umask)
import Darcs.UI.Options ( (?) )
import qualified Darcs.UI.Options.All as O
import Darcs.Repository
    ( RepoJob(..)
    , addToPending
    , finalizeRepositoryChanges
    , withRepoLock
    )
import Darcs.Patch ( changepref )
import Darcs.Patch.Witnesses.Ordered ( FL(..) )
import Darcs.Repository.Prefs ( getPrefval, changePrefval )
import Darcs.Util.English ( orClauses )
import Darcs.Util.Path ( AbsolutePath )
import Darcs.Util.Printer ( Doc, text )

-- | A list of all valid preferences for @_darcs/prefs/prefs@.
validPrefData :: [(String, String)] -- ^ (name, one line description)
validPrefData =
    [("test", "a shell command that runs regression tests"),
     ("predist", "a shell command to run before `darcs dist'"),
     ("boringfile", "the path to a version-controlled boring file"),
     ("binariesfile", "the path to a version-controlled binaries file")]

validPrefs :: [String]
validPrefs = map fst validPrefData

setprefDescription :: String
setprefDescription =
    "Set a preference (" ++ orClauses validPrefs ++ ")."

setprefHelp :: Doc
setprefHelp = text $
 "When working on project with multiple repositories and contributors,\n" ++
 "it is sometimes desirable for a preference to be set consistently\n" ++
 "project-wide.  This is achieved by treating a preference set with\n" ++
 "`darcs setpref` as an unrecorded change, which can then be recorded\n" ++
 "and then treated like any other patch.\n" ++
 "\n" ++
 "Valid preferences are:\n" ++
 "\n" ++
 unlines ["* "++x++" -- "++y | (x,y) <- validPrefData] ++
 "\n" ++
 "For example, a project using GNU autotools, with a `make test` target\n" ++
 "to perform regression tests, might enable Darcs' integrated regression\n" ++
 "testing with the following command:\n" ++
 "\n" ++
 "    darcs setpref test 'autoconf && ./configure && make && make test'\n" ++
 "\n" ++
 "Note that merging is not currently implemented for preferences: if two\n" ++
 "patches attempt to set the same preference, the last patch applied to\n" ++
 "the repository will always take precedence.  This is considered a\n" ++
 "low-priority bug, because preferences are seldom set.\n"

setpref :: DarcsCommand
setpref = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "setpref"
    , commandHelp = setprefHelp
    , commandDescription = setprefDescription
    , commandExtraArgs = 2
    , commandExtraArgHelp = ["<PREF>", "<VALUE>"]
    , commandCommand = setprefCmd
    , commandPrereq = amInHashedRepository
    , commandCompleteArgs = completeArgs
    , commandArgdefaults = nodefaults
    , commandOptions = setprefOpts
    }
  where
    setprefBasicOpts = O.repoDir
    setprefAdvancedOpts = O.umask
    setprefOpts = setprefBasicOpts `withStdOpts` setprefAdvancedOpts
    completeArgs _ _ [] = return validPrefs
    completeArgs _ _ _args = return []

setprefCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
setprefCmd _ opts [pref,val] =
 withRepoLock (useCache ? opts) (umask ? opts) $ RepoJob $ \repository -> do
  when (' ' `elem` pref) $ do
    putStrLn $ "'"++pref++
               "' is not a valid preference name: no spaces allowed!"
    exitWith $ ExitFailure 1
  when (pref `notElem` validPrefs) $ do
    putStrLn $ "'"++pref++"' is not a valid preference name!"
    putStrLn $ "Try one of: " ++ unwords validPrefs
    exitWith $ ExitFailure 1
  oval <- getPrefval pref
  let old = fromMaybe "" oval
  when ('\n' `elem` val) $ do
    putStrLn $ val ++ "is not a valid preference value: newlines forbidden!"
    exitWith $ ExitFailure 1
  changePrefval pref old val
  putStrLn $ "Changing value of "++pref++" from '"++old++"' to '"++val++"'"
  addToPending repository (diffingOpts opts) (changepref pref old val :>: NilFL)
  void $ finalizeRepositoryChanges repository (O.dryRun ? opts)
setprefCmd _ _ _ = error "impossible case"

