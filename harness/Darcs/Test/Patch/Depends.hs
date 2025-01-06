module Darcs.Test.Patch.Depends ( testSuite ) where

import Darcs.Prelude

import Darcs.Patch.Depends
import Darcs.Patch.Set

import Darcs.Patch.Witnesses.Ordered ( FL(..), (:>)(..), RL(..)  )

import Darcs.Patch.V2 ( RepoPatchV2 )
import qualified Darcs.Patch.V2.Prim as V2

import Darcs.Patch.Named ( infopatch )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, piap, info )
import Darcs.Patch.Info ( PatchInfo, rawPatchInfo )
import Darcs.Patch.Prim.V1.Core ( Prim(..), FilePatchType(..) )

import Darcs.Util.Path ( unsafeFloatPath )

import Darcs.Test.TestOnly.Instance ()

import Test.HUnit ( assertFailure )
import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit ( testCase )

type Patch = RepoPatchV2 V2.Prim

testSuite :: TestTree
testSuite = testGroup "Darcs.Patch.Depends" $
  [ test1
  ]

data WA
data WB

test1 :: TestTree
test1 = testCase "findCommonWithThem: \"them\" patch contents should not be inspected" $ do
  let
    mkPatch :: PatchInfo -> FL V2.Prim wA wB -> PatchInfoAnd Patch wA wB
    mkPatch pi ps = piap pi (infopatch pi ps)

    p1info = rawPatchInfo "1999" "p1" "harness" [] False
    p1 = mkPatch p1info (V2.Prim (FP (unsafeFloatPath "foo") AddFile) :>: NilFL)
    set1 :: PatchSet Patch Origin WA
    set1 = PatchSet NilRL (NilRL :<: p1)

    p2info = rawPatchInfo "1999" "p2" "harness" [] False
    p2 = piap p2info (error "patch p2 content should not be read")
    p1' = piap p1info (error "patch p1' content should not be read")
    set2 :: PatchSet Patch Origin WB
    set2 = PatchSet NilRL (NilRL :<: p2 :<: p1')

  case findCommonWithThem set1 set2 of
    PatchSet NilRL (NilRL :<: p1res) :> NilFL
      | info p1res == p1info -> return ()
      | otherwise -> assertFailure $ "findCommonWithThem failed: got info " ++ show (info p1res)
    _ -> assertFailure $ "findCommonWithThem failed: unexpected structure"
