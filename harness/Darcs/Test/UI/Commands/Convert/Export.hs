module Darcs.Test.UI.Commands.Convert.Export ( testSuite ) where

import Darcs.Prelude
import Darcs.UI.Commands.Convert.Export ( cleanPatchAuthor, cleanPatchAuthorTestCases )

import Test.Tasty.HUnit ( testCase )
import Test.Tasty ( TestTree, testGroup )
import Test.HUnit ( (@?=) )

testSuite :: TestTree
testSuite = testGroup "Darcs.UI.Commands.Convert.Export"
  [ testGroup "cleanPatchAuthor" $ flip map cleanPatchAuthorTestCases $ \(input, expected) ->
      testCase (show input) $ cleanPatchAuthor input @?= expected
  ]
