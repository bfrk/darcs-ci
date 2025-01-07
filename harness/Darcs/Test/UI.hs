module Darcs.Test.UI ( testSuite ) where

import qualified Darcs.Test.UI.Commands.Test ( testSuite )
import qualified Darcs.Test.UI.Commands.Convert.Export ( testSuite )

import Test.Framework ( Test, testGroup )

testSuite :: Test
testSuite =
  testGroup "Darcs.UI"
    [ Darcs.Test.UI.Commands.Test.testSuite
    , Darcs.Test.UI.Commands.Convert.Export.testSuite
    ]
