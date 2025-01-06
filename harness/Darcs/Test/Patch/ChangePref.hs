module Darcs.Test.Patch.ChangePref ( testSuite ) where

import Darcs.Prelude

import qualified Data.ByteString.Char8 as BC
import Test.QuickCheck
import Test.QuickCheck.Instances.ByteString ()
import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.QuickCheck ( testProperty )

import qualified Darcs.Patch.Prim.V1.Core as Prim
import Darcs.Patch.RepoPatch ( Eq2, ReadPatch, FormatPatch )
import qualified Darcs.Patch.V1.Prim as V1
import qualified Darcs.Patch.V2.Prim as V2
import Darcs.Patch.Witnesses.Sealed ( Sealed2(..) )
import Darcs.Patch.Witnesses.Show ( Show2(..) )
import Darcs.Util.ByteString ( decodeLocale )

import Darcs.Test.Patch.Properties.Generic

arbitraryChangePref :: Gen (Prim.Prim wX wY)
arbitraryChangePref = do
  preflen <- choose (3, 10)
  pref <- vectorOf preflen $ chooseEnum ('a', 'z')
  let value = fmap decodeLocale $ arbitrary `suchThat` (\s -> not ('\n' `BC.elem` s))
  old <- value
  new <- value
  return $ Prim.ChangePref pref old new

newtype Setpref p = Setpref (Sealed2 p) deriving Show

instance Arbitrary (Setpref V1.Prim) where
  arbitrary = Setpref . Sealed2 . V1.Prim <$> arbitraryChangePref

instance Arbitrary (Setpref V2.Prim) where
  arbitrary = Setpref . Sealed2 . V2.Prim <$> arbitraryChangePref

testSuite
  :: forall p
   . ( Arbitrary (Setpref p)
     , Eq2 p
     , Show2 p
     , ReadPatch p
     , FormatPatch p
     )
  => TestTree
testSuite = testGroup "ChangePref"
  [ testProperty "readPatch . formatPatch == id" $
      unSetpref (formatRead :: PatchProperty p)
  ]
  where
    unSetpref :: (forall wA wB. p wA wB -> x) -> Setpref p -> x
    unSetpref test (Setpref (Sealed2 p)) = test p
