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

{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Darcs.UI.Commands.Test
    (
      test
    ) where

import Darcs.Prelude hiding ( init )

import Control.Monad( when )

import System.Process ( system )
import System.Exit ( ExitCode(..), exitWith )

import Darcs.Patch ( description )
import Darcs.Patch.PatchInfoAnd ( hopefully )
import Darcs.Patch.Set ( patchSet2RL )
import Darcs.Patch.Witnesses.Ordered ( mapFL, mapRL_RL )
import Darcs.Patch.Witnesses.Sealed ( Sealed2(..) )
import Darcs.Repository
    ( RepoJob(..)
    , createPristineDirectoryTree
    , readPatches
    , setScriptsExecutable
    , withRepository
    )
import Darcs.Repository.Prefs ( getPrefval )
import Darcs.UI.Commands
    ( DarcsCommand(..)
    , amInHashedRepository
    , nodefaults
    , putInfo
    , withStdOpts
    )
import Darcs.UI.Commands.Test.Impl
    ( StrategyResultRaw(..)
    , PatchSeq(..)
    , exitCodeToTestResult
    , explanatoryTextFor
    , mkTestCmd
    , runTestable
    , patchTreeToFL
    )
import Darcs.UI.Completion ( noArgs )
import Darcs.UI.Flags ( DarcsFlag, useCache )
import Darcs.UI.Options ( (^), (?) )
import qualified Darcs.UI.Options.All as O
import Darcs.Util.Lock ( withPermDir, withTempDir )
import Darcs.Util.Path ( AbsolutePath, toFilePath )
import Darcs.Util.Printer ( Doc, putDocLn, text )


testDescription :: String
testDescription = "Run tests and search for the patch that introduced a bug."

testHelp :: Doc
testHelp = text $
 unlines
 [ "Run test on the current recorded state of the repository.  Given no"
  ,"arguments, it uses the default repository test (see `darcs setpref`)."
  ,"Given one argument, it treats it as a test command."
  ,"Given two arguments, the first is an initialization command and the"
  ,"second is the test (meaning the exit code of the first command is not"
  ,"taken into account to determine success of the test)."
  ,"If given the `--linear` or `--bisect` flags, it tries to find the most"
  ,"recent version in the repository which passes a test."
  ,""
  ,"`--linear` does linear search starting from head, and moving away"
  ,"from head. This strategy is best when the test runs very quickly"
  ,"or the patch you're seeking is near the head."
  ,""
  ,"`--bisect` does binary search.  This strategy is best when the test"
  ,"runs very slowly or the patch you're seeking is likely to be in"
  ,"the repository's distant past."
  ,""
  ,"`--backoff` starts searching from head, skipping further and further"
  ,"into the past until the test succeeds.  It then does a binary search"
  ,"on a subset of those skipped patches.  This strategy works well unless"
  ,"the patch you're seeking is in the repository's distant past."
  ,""
  ,"Under the assumption that failure is monotonous, `--linear` and"
  ,"`--bisect` produce the same result.  (Monotonous means that when moving"
  ,"away from head, the test result changes only once from \"fail\" to"
  ,"\"ok\".)  If failure is not monotonous, any one of the patches that"
  ,"break the test is found at random."
  ,""
  ,"If the test command returns an exit code of 125, the repository"
  ,"state is treated as \"untestable\" - for example you might get it to"
  ,"do this for a build break or other result that isn't the actual"
  ,"problem you want to track down. This can lead to multiple patches"
  ,"being reported as the source of the failure."
  ,""
  ,"For example, if patch 1 introduces a build break, patch 2 breaks a"
  ,"test in an unrelated bit of the code, and patch 3 fixes the build"
  ,"break, then patches 1,2 and 3 would be identified as causing the"
  ,"failure."
  ,""
  ,"The `--shrink-failures` option, on by default, adds a post-processing"
  ,"step to reorder patches to try to narrow down a failure more"
  ,"precisely. In the example above, it's likely that patch 2 could be"
  ,"moved before patch 1 or after patch 3, allowing it to be identified"
  ,"as the sole cause of the failure."
  ,""
  ,"This shrinking can be disabled with `--no-shrink-failures`."
 ]

test :: DarcsCommand
test = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "test"
    , commandHelp = testHelp
    , commandDescription = testDescription
    , commandExtraArgs = -1
    , commandExtraArgHelp = ["[[INITIALIZATION]", "COMMAND]"]
    , commandCommand = testCommand
    , commandPrereq = amInHashedRepository
    , commandCompleteArgs = noArgs
    , commandArgdefaults = nodefaults
    , commandOptions = testOpts
    }
  where
    testBasicOpts = O.testStrategy ^ O.leaveTestDir ^ O.repoDir
    testAdvancedOpts = O.setScriptsExecutable ^ O.shrinkFailure
    testOpts = testBasicOpts `withStdOpts` testAdvancedOpts

testCommand :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
testCommand _ opts args =
 withRepository (useCache ? opts) $ RepoJob $ \repository -> do
  patches <- readPatches repository
  (init :: IO ExitCode,testCmd) <- case args of
    [] ->
      do t <- getTest
         return (return ExitSuccess, mkTestCmd (exitCodeToTestResult <$> t))
    [cmd] ->
      do putStrLn $ "Using test command:\n"++cmd
         return (return ExitSuccess, mkTestCmd (exitCodeToTestResult <$> system cmd))
    [init,cmd] ->
      do putStrLn $ "Using initialization command:\n"++init
         putStrLn $ "Using test command:\n"++cmd
         return (system init, mkTestCmd (exitCodeToTestResult <$> system cmd))
    _ -> fail "Test expects zero to two arguments."
  let wd = case O.leaveTestDir ? opts of
            O.YesLeaveTestDir -> withPermDir
            O.NoLeaveTestDir -> withTempDir
  wd "testing" $ \d -> do
    createPristineDirectoryTree repository (toFilePath d) O.WithWorkingDir
    when (O.yes (O.setScriptsExecutable ? opts)) setScriptsExecutable
    _ <- init
    putInfo opts "Running test...\n"
    result <-
      runTestable
        (O.setScriptsExecutable ? opts)
        testCmd
        (O.testStrategy ? opts)
        (O.shrinkFailure ? opts)
        (mapRL_RL hopefully . patchSet2RL $ patches)
    case result of
      NoPasses -> putStrLn "Noone passed the test!"
      NoFailureOnHead -> putStrLn "Test does not fail on head."
      Blame (Sealed2 ps) -> do
        let extraText = explanatoryTextFor (O.testStrategy ? opts)
        case ps of
          Single p -> do
            putStrLn ("Last recent patch that fails the test" ++ extraText ++ ":")
            putDocLn (description p)
          _ -> do
            putStrLn "These patches jointly trigger the failure:"
            sequence_ $ mapFL (putDocLn . description) (patchTreeToFL ps)
      RunSuccess -> putInfo opts "Test ran successfully.\n"
      RunFailed n -> do
        putInfo opts "Test failed!\n"
        exitWith (ExitFailure n)

 where
    getTest :: IO (IO ExitCode)
    getTest = do
      testline <- getPrefval "test"
      return $
        case testline of
          Nothing -> return ExitSuccess
          Just testcode -> do
            putInfo opts "Running test...\n"
            ec <- system testcode
            if ec == ExitSuccess
              then putInfo opts "Test ran successfully.\n"
              else putInfo opts "Test failed!\n"
            return ec
