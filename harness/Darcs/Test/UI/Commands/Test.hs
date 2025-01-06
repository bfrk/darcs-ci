module Darcs.Test.UI.Commands.Test ( testSuite ) where

import qualified Darcs.Test.UI.Commands.Test.Commutable ( testSuite )
import qualified Darcs.Test.UI.Commands.Test.Simple ( testSuite )

import Test.Tasty ( TestTree, testGroup )

testSuite :: TestTree
testSuite =
  testGroup "Darcs.UI.Commands.Test"
    [ Darcs.Test.UI.Commands.Test.Simple.testSuite
    , Darcs.Test.UI.Commands.Test.Commutable.testSuite
    ]
