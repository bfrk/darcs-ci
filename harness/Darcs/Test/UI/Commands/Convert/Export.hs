module Darcs.Test.UI.Commands.Convert.Export ( testSuite ) where

import Darcs.Prelude
import Darcs.UI.Commands.Convert.Export ( cleanPatchAuthor, cleanPatchAuthorTestCases )

import Test.Framework.Providers.HUnit ( testCase )
import Test.Framework ( Test, testGroup )
import Test.HUnit ( (@?=) )

testSuite :: Test
testSuite = testGroup "Darcs.UI.Commands.Convert.Export"
  [ testGroup "cleanPatchAuthor" $ flip map cleanPatchAuthorTestCases $ \(input, expected) ->
      testCase (show input) $ cleanPatchAuthor input @?= expected
  ]
