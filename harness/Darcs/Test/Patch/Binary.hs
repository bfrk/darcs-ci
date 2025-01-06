module Darcs.Test.Patch.Binary ( testSuite ) where

import Darcs.Prelude

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
import Darcs.Util.Path ( AnchoredPath(..), Name, makeName )

import Darcs.Test.Patch.Properties.Generic

arbitraryName :: Gen Name
arbitraryName = arbitrary `suchThatMap` toName
  where
    toName bs =
      case makeName bs of
        Left _ -> Nothing
        Right n -> Just n

arbitraryBinary :: Gen (Prim.Prim wX wY)
arbitraryBinary = do
  path <- fmap AnchoredPath $ listOf arbitraryName `suchThat` (not . null)
  old <- arbitrary
  new <- arbitrary
  return $ Prim.FP path (Prim.Binary old new)

newtype Binary p = Binary (Sealed2 p) deriving Show

instance Arbitrary (Binary V1.Prim) where
  arbitrary = Binary . Sealed2 . V1.Prim <$> arbitraryBinary

instance Arbitrary (Binary V2.Prim) where
  arbitrary = Binary . Sealed2 . V2.Prim <$> arbitraryBinary

testSuite
  :: forall p
   . ( Arbitrary (Binary p)
     , Eq2 p
     , Show2 p
     , ReadPatch p
     , FormatPatch p
     )
  => TestTree
testSuite = testGroup "Binary"
  [ testProperty "readPatch . formatPatch == id" $
      unBinary (formatRead :: PatchProperty p)
  ]
  where
    unBinary :: (forall wA wB. p wA wB -> x) -> Binary p -> x
    unBinary test (Binary (Sealed2 p)) = test p
