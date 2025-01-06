--  Copyright (C) 2002-2005,2007 David Roundy
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

-- UndecidableInstances was added because GHC 8.6 needed it
-- even though GHC 8.2 didn't
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Darcs.Test.Patch.Properties
  ( unit_V1P1
  , unit_V2P1
  , qc_V1P1
  , qc_V2
  , qc_V3
  , qc_Named_V1
  , qc_Named_V2
  , qc_Named_V3
  , qc_prim
  , qc_prim_v1
  , qc_named_prim
  ) where

import Darcs.Prelude

import Data.Constraint ( Dict(..) )
import Data.Maybe ( fromMaybe )
import Test.QuickCheck ( Arbitrary(..) )
import Test.Tasty ( TestTree, adjustOption )
import Test.Tasty.QuickCheck ( QuickCheckMaxSize(..), testProperty )

import Darcs.Test.Util.TestResult ( TestResult, maybeFailed )
import Darcs.Test.Patch.Utils
    ( PropList
    , TestCheck(..)
    , TestCondition(..)
    , TestGenerator(..)
    , properties
    , testCases
    , testConditional
    )

import Darcs.Patch.Witnesses.Maybe
import Darcs.Patch.Witnesses.Ordered
import Darcs.Patch.Witnesses.Sealed
import Darcs.Patch.Witnesses.Eq ( Eq2 )
import Darcs.Patch.Witnesses.Show
import Darcs.Patch.FromPrim ( PrimOf, FromPrim(..) )
import Darcs.Patch.Prim ( PrimCoalesce, PrimPatch, coalesce )
import qualified Darcs.Patch.V1.Prim as V1 ( Prim )
import qualified Darcs.Patch.V2.Prim as V2 ( Prim )
import Darcs.Patch.Prim.Named ( NamedPrim )
import Darcs.Patch.V1 ( RepoPatchV1 )
import Darcs.Patch.V2.RepoPatch ( isConsistent, isForward, RepoPatchV2 )
import Darcs.Patch.V3 ( RepoPatchV3 )
import Darcs.Patch.Commute ( Commute(..) )
import Darcs.Patch.Invert ( Invert )
import Darcs.Patch.Show ( ShowPatchBasic )
import Darcs.Patch.Apply( ApplyState, ObjectIdOfPatch )
import Darcs.Patch.Merge ( CleanMerge, Merge )
import Darcs.Patch.Named ( Named )
import qualified Darcs.Patch.RepoPatch as RP

import Darcs.Test.Patch.Arbitrary.Generic
import Darcs.Test.Patch.Arbitrary.Named ()
import Darcs.Test.Patch.Arbitrary.PatchTree
import Darcs.Test.Patch.Arbitrary.PrimFileUUID()
import Darcs.Test.Patch.Arbitrary.Mergeable
import Darcs.Test.Patch.Arbitrary.RepoPatchV1 ()
import Darcs.Test.Patch.Arbitrary.RepoPatchV2 ()
import Darcs.Test.Patch.Arbitrary.RepoPatchV3 ()
import Darcs.Test.Patch.Arbitrary.PrimV1 ()
import Darcs.Test.Patch.Merge.Checked ( CheckedMerge )
import Darcs.Test.Patch.RepoModel
import Darcs.Test.Patch.Types.Pair ( Pair(..) )
import Darcs.Test.Patch.WithState
    ( PropagateShrink
    , ShrinkModel
    , WithState(..)
    , ArbitraryWS(..)
    )

import qualified Darcs.Test.Patch.Examples.Set1 as Ex1
import qualified Darcs.Test.Patch.Examples.Set2 as Ex2

import Darcs.Test.Patch.Properties.Check( Check(..) )
import Darcs.Test.Patch.Properties.Generic
    ( MergeProperty
    , PatchProperty
    , SequenceProperty
    , SequencePairProperty
    )
import qualified Darcs.Test.Patch.Properties.Generic as PropG
import qualified Darcs.Test.Patch.Properties.Mergeable as PropM
import qualified Darcs.Test.Patch.Properties.RepoPatchV3 as PropR3
import qualified Darcs.Test.Patch.Properties.V1Set1 as Prop1
import qualified Darcs.Test.Patch.Properties.V1Set2 as Prop2

import Darcs.Test.Patch.Types.Triple (Triple(..))


type Prim2 = V2.Prim

instance PrimPatch prim => RepoApply (NamedPrim prim)
instance PrimPatch prim => RepoApply (RepoPatchV1 prim)
instance PrimPatch prim => RepoApply (RepoPatchV2 prim)
instance PrimPatch prim => RepoApply (RepoPatchV3 prim)


onFL :: (forall wA wB. p wA wB -> TestResult) -> FL p wX wY -> TestResult
onFL prop = mconcat . mapFL prop

onRL :: (forall wA wB. p wA wB -> TestResult) -> RL p wX wY -> TestResult
onRL prop = mconcat . mapRL prop

unit_V1P1:: [TestTree]
unit_V1P1 =
  [ testCases "known commutes" Prop1.checkCommute Ex1.knownCommutes
  , testCases "known non-commutes" Prop1.checkCantCommute Ex1.knownCantCommutes
  , testCases "known merges" Prop1.checkMerge Ex1.knownMerges
  , testCases "known merges (equiv)" Prop1.checkMergeEquiv Ex1.knownMergeEquivs
  , testCases "known canons" Prop1.checkCanon Ex1.knownCanons
  , testCases "merge swaps" Prop1.checkMergeSwap Ex1.mergePairs2
  , testCases "the patch validation works" Prop1.tTestCheck Ex1.validPatches
  , testCases "commute/recommute" (PropG.recommute commute) Ex1.commutePairs
  , testCases "merge properties: merge either way valid" PropG.mergeEitherWayValid Ex1.mergePairs
  , testCases "merge properties: merge swap" PropG.mergeEitherWay Ex1.mergePairs
  , testCases "primitive patch IO functions" (onFL PropG.formatRead) Ex1.primitiveTestPatches
  , testCases "IO functions (test patches)" (onFL PropG.formatRead) Ex1.testPatches
  , testCases "IO functions (named test patches)" PropG.formatRead Ex1.testPatchesNamed
  , testCases "primitive commute/recommute" (PropG.recommute commute) Ex1.primitiveCommutePairs
  ]

unit_V2P1 :: [TestTree]
unit_V2P1 =
  [ testCases "coalesce commute" (PropG.coalesceCommute (fmap maybeToFL . coalesce)) Ex2.primPermutables
  , testCases "prim recommute" (PropG.recommute commute . Pair) Ex2.commutables
  , testCases "square commute law" (PropG.squareCommuteLaw commute . Pair) Ex2.commutables
  , testCases "prim inverses commute" (PropG.commuteInverses commute . Pair) Ex2.commutables
  , testCases "FL prim recommute" (PropG.recommute commute . Pair) Ex2.commutablesFL
  , testCases "FL square commute law" (PropG.squareCommuteLaw commute . Pair) Ex2.commutablesFL
  , testCases "FL prim inverses commute" (PropG.commuteInverses commute . Pair) $ Ex2.commutablesFL
  , sealedCases "read and show work on Prim" PropG.formatRead Ex2.primPatches
  , sealedCases "read and show work on RepoPatchV2" PropG.formatRead Ex2.repov2Patches
  , testCases "example flattenings work" (PropM.propConsistentTreeFlattenings fromPrim2) Ex2.repov2PatchLoopExamples
  , sealedCases "V2 merge input consistent" (PropG.mergeArgumentsConsistent isConsistent) Ex2.repov2Mergeables
  , sealedCases "V2 merge input is forward" (PropG.mergeArgumentsConsistent isForward) Ex2.repov2Mergeables
  , sealedCases "V2 merge output is forward" (PropG.mergeConsistent isForward) Ex2.repov2Mergeables
  , sealedCases "V2 merge output consistent" (PropG.mergeConsistent isConsistent) Ex2.repov2Mergeables
  , sealedCases "V2 merge either way" PropG.mergeEitherWay Ex2.repov2Mergeables
  , sealedCases "V2 merge and commute" PropG.mergeCommute Ex2.repov2Mergeables
  , sealedCases "V2 recommute" (PropG.recommute commute . Pair) Ex2.repov2Commutables
  , sealedCases "V2 inverses commute" (PropG.commuteInverses commute . Pair) Ex2.repov2Commutables
  , sealedCases "V2 permutivity" (PropG.permutivity commute) Ex2.repov2NonduplicateTriples
  ]
  where
    fromPrim2 :: PropM.FromPrimT RepoPatchV2 Prim2
    fromPrim2 = fromAnonymousPrim
    sealedCases :: String -> (forall wX wY. p wX wY -> TestResult) -> [Sealed2 p] -> TestTree
    sealedCases name prop = testCases name (unseal2 prop)

arbitraryThing :: TestGenerator thing (Sealed2 thing)
arbitraryThing = TestGenerator (\f p -> Just (unseal2 f p))

arbitraryWSThing :: TestGenerator thing (Sealed2 (WithState thing))
arbitraryWSThing = TestGenerator (\f wsp -> Just (unseal2 (f . wsPatch) wsp))

type TestablePrim prim =
  ( ApplyState prim ~ RepoState (ModelOf prim)
  , ArbitraryPrim prim
  , CleanMerge prim
  , Commute prim
  , Eq2 prim
  , Invert prim
  , RepoModel (ModelOf prim)
  , RP.Apply prim
  , RP.ReadPatch prim
  , RP.FormatPatch prim
  , Show1 (ModelOf prim)
  , Show2 prim
  , ShowPatchBasic prim
  )

type TestablePrimBase prim =
  ( ApplyState prim ~ RepoState (ModelOf prim)
  , ArbitraryPrim prim
  , PrimPatch prim
  , PropagateShrink prim prim
  , RepoApply prim
  , Show1 (ModelOf prim)
  , ShrinkModel (ModelOf prim) prim
  )

qc_prim :: forall prim.
           ( TestablePrim prim
           , MightHaveDuplicate prim
           , ArbitraryWS prim
           , RepoApply prim
           ) => [TestTree]
qc_prim =
  [testProperty "prim pair coverage" (unseal2 (PropG.propPrimPairCoverage @prim . wsPatch))] ++
  -- The following fails because of setpref patches:
  -- testProperty "prim inverse doesn't commute" (inverseDoesntCommute :: Prim -> Maybe Doc)
  (case runCoalesceTests @prim of
    Just Dict ->
      [ testProperty "prim coalesce effect preserving"
        (unseal2 $ PropG.coalesceEffectPreserving (fmap maybeToFL . coalesce) :: Sealed2 (WithState (Pair prim)) -> TestResult)
      ]
    Nothing -> [])
    ++ concat
  [ pair_properties         @prim      "arbitrary"    arbitraryWSThing
  , pair_properties         @(FL prim) "arbitrary FL" arbitraryWSThing
  , coalesce_properties     @prim      "arbitrary"    arbitraryWSThing
  , prim_commute_properties @prim      "arbitrary"    arbitraryWSThing
  , prim_commute_properties @(FL prim) "arbitrary FL" arbitraryWSThing
  , patch_properties        @prim      "arbitrary"    arbitraryWSThing
  , patch_properties        @(FL prim) "arbitrary FL" arbitraryWSThing
  , patch_repo_properties   @prim      "arbitrary"    arbitraryThing
  , patch_repo_properties   @(FL prim) "arbitrary FL" arbitraryThing
  , pair_repo_properties    @prim      "arbitrary"    arbitraryThing
  , pair_repo_properties    @(FL prim) "arbitrary FL" arbitraryThing
  , triple_properties       @prim      "arbitrary"    arbitraryWSThing
  , triple_properties       @(FL prim) "arbitrary FL" arbitraryWSThing
  , [ testProperty "readPatch . formatPatch == id"
      (unseal2 $ (PropG.formatRead . wsPatch) :: Sealed2 (WithState prim) -> TestResult)
    ]
  ]

qc_named_prim :: forall prim.
                 ( TestablePrim prim
                 , PrimPatch prim
                 ) => [TestTree]
qc_named_prim =
  qc_prim @(NamedPrim prim) ++
  [ testProperty
      "prim inverse doesn't commute"
      (unseal2 $ (PropG.inverseDoesntCommute . wsPatch) :: Sealed2 (WithState (NamedPrim prim)) -> TestResult)
  ]

qc_prim_v1
  :: forall prim
   . ( TestablePrim prim
     , MightHaveDuplicate prim
     , ArbitraryWS prim
     , RepoApply prim
     , Eq (ObjectIdOfPatch prim)
     , RP.IsHunk prim
     , PrimCoalesce prim
     )
  => [TestTree]
qc_prim_v1 =
  qc_prim @prim ++
  [ testProperty
      "prims do not coalesce and commute"
      (unseal2 $ (PropG.notCoalesceAndCommute . wsPatch)
        :: Sealed2 (WithState (Pair prim)) -> TestResult)
  ]

qc_V1P1 :: [TestTree]
qc_V1P1 =
  mergeablePatchProperties @(RepoPatchV1 V1.Prim) ++
  [ testProperty "commuting by patch and its inverse is ok" (Prop2.propCommuteInverse . mapSeal2 (getPair . wsPatch))
  , testProperty "a patch followed by its inverse is identity" (Prop2.propPatchAndInverseIsIdentity . mapSeal2 (getPair . wsPatch))
  , testProperty "'simple smart merge'" Prop2.propSimpleSmartMergeGoodEnough
  , testProperty "commutes are equivalent" (Prop2.propCommuteEquivalency . mapSeal2 (getPair . wsPatch))
  , testProperty "merges are valid" Prop2.propMergeValid
  , testProperty "inverses being valid" (Prop2.propInverseValid . mapSeal2 wsPatch)
  , testProperty "other inverse being valid" (Prop2.propOtherInverseValid . mapSeal2 wsPatch)
  -- The patch generator isn't smart enough to generate correct test cases for
  -- the following: (which will be obsoleted soon, anyhow)
  -- , testProperty "the order dependence of unravel" Prop.propUnravelOrderIndependent
  -- , testProperty "the unravelling of three merges" Prop.propUnravelThreeMerge
  -- , testProperty "the unravelling of a merge of a sequence" Prop.propUnravelSeqMerge
  , testProperty "the order of commutes" (Prop2.propCommuteEitherOrder . mapSeal2 (getTriple . wsPatch))
  , testProperty "commute either way" (Prop2.propCommuteEitherWay . mapSeal2 (getPair . wsPatch))
  , testProperty "the double commute" (Prop2.propCommuteTwice . mapSeal2 (getPair . wsPatch))
  , testProperty "merges commute and are well behaved" Prop2.propMergeIsCommutableAndCorrect
  , testProperty "merges can be swapped" Prop2.propMergeIsSwapable
  ]

qc_V2 :: forall prim . TestablePrimBase prim => [TestTree]
qc_V2 =
  [ testProperty "with quickcheck that patches are consistent"
    (withSingle consistent)
  ]
  ++ mergeablePatchProperties @(RepoPatchV2 prim)
  ++ concat
  [ merge_properties   @(RepoPatchV2 prim) "tree" (TestGenerator mergePairFromTree)
  , merge_properties   @(RepoPatchV2 prim) "twfp" (TestGenerator mergePairFromTWFP)
  , pair_properties    @(RepoPatchV2 prim) "tree" (TestGenerator (\handle -> commutePairFromTree (handle . Pair)))
  , pair_properties    @(RepoPatchV2 prim) "twfp" (TestGenerator (\handle -> commutePairFromTWFP (handle . Pair)))
  , patch_properties   @(RepoPatchV2 prim) "tree" (TestGenerator patchFromTree)
  , triple_properties  @(RepoPatchV2 prim) "tree" (TestGenerator (\handle -> commuteTripleFromTree (handle . Triple)))
  ]
  where
    consistent :: RepoPatchV2 prim wX wY -> TestResult
    consistent = maybeFailed . isConsistent

qc_V3 :: forall prim . TestablePrimBase prim => [TestTree]
qc_V3 =
  [ testProperty "repo invariants"
    (withSequence (PropR3.prop_repoInvariants :: SequenceProperty (RepoPatchV3 prim)))
  ]
  ++ mergeablePatchProperties @(RepoPatchV3 prim)
  ++ difficultPatchProperties @(RepoPatchV3 prim)

instance (ArbitraryPrim prim, ApplyState prim ~ RepoState (ModelOf prim)) =>
         ArbitraryMergeable (Named (RepoPatchV3 prim)) where
  notRepoPatchV1 = Just (NotRepoPatchV1 (\case {}))

instance MightHaveDuplicate p => MightHaveDuplicate (Named p)

qc_Named_V1 :: forall prim . TestablePrimBase prim => [TestTree]
qc_Named_V1 = serializePatchProperties @(Named (RepoPatchV1 prim))

qc_Named_V2 :: forall prim . TestablePrimBase prim => [TestTree]
qc_Named_V2 = serializePatchProperties @(Named (RepoPatchV2 prim))

qc_Named_V3 :: forall prim . TestablePrimBase prim => [TestTree]
qc_Named_V3 =
  mergeablePatchProperties @(Named (RepoPatchV3 prim)) ++
  difficultPatchProperties @(Named (RepoPatchV3 prim))

-- | Similar to 'RepoPatch' but with constraints reduced to what is needed for
-- generation and property testing of mergeable patches, so that we have
-- instances for @'Named' p@ for all 'RepoPatch' types @p@.
type MergeablePatch p =
  ( ApplyState (PrimOf p) ~ ApplyState p
  , CheckedMerge p
  , PrimPatch (PrimOf p)
  , RP.Conflict p
  , RP.ReadPatch p
  , RP.FormatPatch p
  , Show2 p
  , ShowPatchBasic p
  )

mergeablePatchProperties
  :: forall p
   . ( ArbitraryMergeable p
     , MergeablePatch p
     , ShrinkModel (ModelOf p) (PrimOf p)
     , PrimBased p
     , RepoApply p
     , RepoApply (PrimOf p)
     )
  => [TestTree]
mergeablePatchProperties =
  serializePatchProperties @p ++
  [ testProperty "readPatch . showPatch == id(RL)"
      (withSequence ((onRL PropG.formatRead) :: SequenceProperty p))
  , testProperty "resolutions don't conflict"
      (withSequence (PropM.propResolutionsDontConflict :: SequenceProperty p))
  ]

serializePatchProperties
  :: forall p
   . ( MergeablePatch p
     , PrimBased p
     , RepoApply p
     , ShrinkModel (ModelOf p) (PrimOf p)
     , RepoState (ModelOf p) ~ ApplyState p
     , RepoModel (ModelOf p)
     , RepoApply (PrimOf p)
     )
  => [TestTree]
serializePatchProperties =
  [ testProperty "readPatch . formatPatch == id"
      (withSingle (PropG.formatRead :: PatchProperty p))
  ]

-- | These properties regularly fail for RepoPatchV2 with the standard test
-- case generator when we crank up the number of tests (to e.g. 10000).
difficultPatchProperties
  :: forall p
   . ( ArbitraryMergeable p
     , MergeablePatch p
     , ShrinkModel (ModelOf p) (PrimOf p)
     , MightHaveDuplicate p
     , PrimBased p
     , RepoApply p
     , RepoApply (PrimOf p)
     )
  => [TestTree]
difficultPatchProperties =
  [ adjustOption (\(QuickCheckMaxSize n) -> QuickCheckMaxSize (n `div` 3)) $
    testProperty "reorderings are consistent"
      (PropM.propConsistentReorderings @p)
  , testProperty "recommute"
      (withPair (PropG.recommute com))
  , testConditional "permutivity"
      (fromMaybe False . withTriple notDuplicatestriple)
      (withTriple (PropG.permutivity com))
  , testProperty "merge either way"
      (withFork (PropG.mergeEitherWay :: MergeProperty p))
  , testProperty "merge commute"
      (withFork (PropG.mergeCommute :: MergeProperty p))
  , testProperty "resolutions are invariant under reorderings"
      (withSequencePair (PropM.propResolutionsOrderIndependent :: SequencePairProperty p))
  ]
  where
    com :: (p :> p) wA wB -> Maybe ((p :> p) wA wB)
    com = commute

pair_properties :: forall p gen
                 . ( Show gen, Arbitrary gen, MightHaveDuplicate p
                   , Commute p, Invert p, ShowPatchBasic p, Eq2 p
                   )
                => PropList (Pair p) gen
pair_properties genname gen =
  properties gen "commute" genname
  [ ("recommute"              , TestCondition (const True)     , TestCheck (PropG.recommute commute)           )
  , ("inverses commute"       , TestCondition (const True)     , TestCheck (PropG.commuteInverses commute)     )
  , ("inverse composition"    , TestCondition (const True)     , TestCheck PropG.inverseComposition            )
  ]

coalesce_properties :: forall p gen
                     . ( Show gen, Arbitrary gen, TestablePrim p
                       )
                    => PropList (Triple p) gen
coalesce_properties genname gen =
  properties gen "commute" genname
   (case runCoalesceTests @p of
      Just Dict ->
        [ ( "coalesce commutes with commute"
          , TestCondition (const True)
          , TestCheck (PropG.coalesceCommute (fmap maybeToFL . coalesce) . getTriple))
        ]
      Nothing -> [])

-- The following properties do not hold for "RepoPatchV2" patches (conflictors and
-- duplicates, specifically) .
prim_commute_properties :: forall p gen
                            . (Show gen, Arbitrary gen, Commute p, Invert p, ShowPatchBasic p, Eq2 p)
                           => PropList (Pair p) gen
prim_commute_properties genname gen =
  properties gen "commute" genname
  [ ("square commute law", TestCondition (const True)     , TestCheck (PropG.squareCommuteLaw commute))
  ]

patch_properties :: forall p gen .
                    ( Show gen
                    , Arbitrary gen
                    , Invert p
                    , Eq2 p
                    , ShowPatchBasic p
                    )
                 => PropList p gen
patch_properties genname gen =
  properties gen "patch" genname
  [ ("inverse . inverse is id"  , TestCondition (const True)     , TestCheck PropG.invertInvolution)
  ]

patch_repo_properties
  :: forall p gen
   . ( Show gen, Arbitrary gen
     , Invert p, ShowPatchBasic p
     , RepoModel (ModelOf p)
     , RepoState (ModelOf p) ~ ApplyState p
     , RepoApply p
     )
  =>  PropList (WithState p) gen
patch_repo_properties genname gen =
  properties gen "patch/repo" genname
  [ ("invert rollback"          , TestCondition (const True)     , TestCheck PropG.invertRollback)
  ]

merge_properties :: forall p gen .
                    ( Show gen, Arbitrary gen, Commute p
                    , Invert p, Eq2 p, Merge p, ShowPatchBasic p
                    , MightHaveDuplicate p, Check p
                    )
                 => PropList (p :\/: p) gen
merge_properties genname gen =
  properties gen "merge" genname
  [ ("merge either way"           , TestCondition (const True)   , TestCheck PropG.mergeEitherWay      )
  , ("merge either way valid"     , TestCondition (const True)   , TestCheck PropG.mergeEitherWayValid )
  , ("merge commute"              , TestCondition (const True)   , TestCheck PropG.mergeCommute        )
  ]

triple_properties :: forall p gen .
                     ( Show gen, Arbitrary gen, Commute p
                     , Eq2 p, ShowPatchBasic p
                     , MightHaveDuplicate p
                     )
                  => PropList (Triple p) gen
triple_properties genname gen =
  properties gen "triple" genname
  [ ( "permutivity"
    , TestCondition (notDuplicatestriple . getTriple)
    , TestCheck (PropG.permutivity commute . getTriple) )
  ]

pair_repo_properties
  :: forall p gen .
     ( Show gen
     , Arbitrary gen
     , Commute p
     , Eq2 p
     , ShowPatchBasic p
     , RepoModel (ModelOf p)
     , RepoState (ModelOf p) ~ ApplyState p
     , RepoApply p
     )
  => PropList (WithState (Pair p)) gen
pair_repo_properties genname gen =
  properties gen "patch/repo" genname
    [ ( "commute is effect preserving"
      , TestCondition (const True)
      , TestCheck (PropG.effectPreserving commute))
    ]
