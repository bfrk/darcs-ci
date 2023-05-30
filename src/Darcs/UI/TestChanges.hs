{-# LANGUAGE OverloadedStrings #-}
module Darcs.UI.TestChanges ( testTree ) where

import Darcs.Prelude

import System.Exit ( ExitCode(..) )
import System.Process ( system )

import Darcs.UI.Commands ( putInfo )
import Darcs.UI.Options ( Config, (?) )
import qualified Darcs.UI.Options.All as O
import Darcs.Repository.Prefs ( getPrefval )
import Darcs.Repository.Working ( setAllScriptsExecutable )

import Darcs.Util.Lock ( withTempDir, withPermDir )
import Darcs.Util.Path ( toPath )
import Darcs.Util.Progress ( debugMessage )
import Darcs.Util.Tree ( Tree )
import Darcs.Util.Tree.Plain ( writePlainTree )

testTree :: Config -> Tree IO -> IO ExitCode
testTree cfg tree = do
  debugMessage "Considering whether to test..."
  ifRunTest (O.testChanges ? cfg) $ \leaveTestDir -> do
    debugMessage "About to run test if it exists."
    testline <- getPrefval "test"
    case testline of
      Nothing -> return ExitSuccess
      Just testcode -> do
        withDir leaveTestDir "testing" $ \dir -> do
          writePlainTree tree (toPath dir)
          putInfo cfg "Running test..."
          sse (O.setScriptsExecutable ? cfg)
          ec <- system testcode
          putInfo cfg $
            if ec == ExitSuccess
              then "Test ran successfully."
              else "Test failed!"
          return ec
  where
    withDir O.YesLeaveTestDir = withPermDir
    withDir O.NoLeaveTestDir = withTempDir
    sse O.YesSetScriptsExecutable = setAllScriptsExecutable
    sse O.NoSetScriptsExecutable = return ()
    ifRunTest (O.YesTestChanges leaveTestDir) test = test leaveTestDir
    ifRunTest O.NoTestChanges _ = return ExitSuccess
