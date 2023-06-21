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
module Darcs.Test.Patch ( testSuite ) where

import Darcs.Prelude

import Data.Constraint ( Dict(..) )
import Test.Framework ( Test, testGroup )
import Test.Framework.Providers.QuickCheck2 ( testProperty )
import Test.QuickCheck( Arbitrary(..) )

import Darcs.Test.Util.TestResult ( TestResult, maybeFailed )
import Darcs.Test.Patch.Utils
    ( PropList
    , TestCheck(..)
    , TestCondition(..)
    , TestGenerator(..)
    , properties
    , testCases
    , testConditional
    , fromNothing
    )

import Darcs.Patch.Witnesses.Ordered
import Darcs.Patch.Witnesses.Sealed
import Darcs.Patch.Witnesses.Eq ( Eq2, unsafeCompare )
import Darcs.Patch.Witnesses.Show
import Darcs.Patch.FromPrim ( PrimOf, FromPrim(..) )
import Darcs.Patch.Prim ( PrimPatch, coalesce )
import qualified Darcs.Patch.Prim.FileUUID as FileUUID ( Prim )
import qualified Darcs.Patch.V1.Prim as V1 ( Prim )
import qualified Darcs.Patch.V2.Prim as V2 ( Prim )
import Darcs.Patch.Prim.Named ( NamedPrim )
import Darcs.Patch.V1 ( RepoPatchV1 )
import Darcs.Patch.V2.RepoPatch ( isConsistent, isForward, RepoPatchV2 )
import Darcs.Patch.V3 ( RepoPatchV3 )
import Darcs.Patch.Commute ( Commute(..) )
import Darcs.Patch.Invert ( Invert )
import Darcs.Patch.Show ( ShowPatchBasic )
import Darcs.Patch.Apply( Apply, ApplyState )
import Darcs.Patch.Merge ( Merge )

import Darcs.Test.Patch.Arbitrary.Generic
import Darcs.Test.Patch.Arbitrary.Named ()
import Darcs.Test.Patch.Arbitrary.PatchTree
import Darcs.Test.Patch.Arbitrary.PrimFileUUID()
import Darcs.Test.Patch.Arbitrary.RepoPatch
import Darcs.Test.Patch.Arbitrary.RepoPatchV1 ()
import Darcs.Test.Patch.Arbitrary.RepoPatchV2 ()
import Darcs.Test.Patch.Arbitrary.RepoPatchV3 ()
import Darcs.Test.Patch.Arbitrary.PrimV1 ()
import Darcs.Test.Patch.Arbitrary.Shrink ( Shrinkable )
import Darcs.Test.Patch.Merge.Checked ( CheckedMerge )
import Darcs.Test.Patch.RepoModel
import Darcs.Test.Patch.WithState
    ( ArbitraryState(..)
    , PropagateShrink
    , ShrinkModel
    , WithState(..)
    , WithEndState(..)
    , ArbitraryWS(..)
    , Pair(..)
    , makeWS2Gen
    )

import qualified Darcs.Test.Patch.Info
import qualified Darcs.Test.Patch.Selection

import qualified Darcs.Test.Patch.Examples.Set1 as Ex
import qualified Darcs.Test.Patch.Examples.Set2Unwitnessed as ExU

import Darcs.Test.Patch.Properties.Check( Check(..) )
import Darcs.Test.Patch.Properties.Generic ( PatchProperty, MergeProperty, SequenceProperty )
import qualified Darcs.Test.Patch.Properties.Generic as PropG
import qualified Darcs.Test.Patch.Properties.RepoPatch as PropR
import qualified Darcs.Test.Patch.Properties.RepoPatchV3 as PropR3
import qualified Darcs.Test.Patch.Properties.GenericUnwitnessed as PropU
import qualified Darcs.Test.Patch.Properties.V1Set1 as Prop1
import qualified Darcs.Test.Patch.Properties.V1Set2 as Prop2

import qualified Darcs.Test.Patch.Rebase as Rebase
import qualified Darcs.Test.Patch.Unwind as Unwind

import qualified Darcs.Test.Patch.WSub as WSub


type Prim1 = V1.Prim
type Prim2 = V2.Prim

-- Generic Arbitrary instances

-- We define them here so they don't overlap with those for RepoPatchV1,
-- which use a different generator for V1Model.

type ArbitraryModel p = (RepoModel (ModelOf p), ArbitraryState p)

instance ArbitraryModel p => ArbitraryWS (FL p) where
  arbitraryWS = makeWS2Gen aSmallRepo

newtype Triple p wX wY = Triple { getTriple :: (p :> p :> p) wX wY }
  deriving Show

instance Show2 p => Show2 (Triple p)

type instance ModelOf (Triple p) = ModelOf p

instance ArbitraryState p => ArbitraryState (Triple p) where
  arbitraryState sIn = do
    Sealed (WithEndState p sOut) <- arbitraryState sIn
    return (Sealed (WithEndState (Triple p) sOut))

instance ArbitraryModel p => ArbitraryWS (Triple p) where
  arbitraryWS = makeWS2Gen aSmallRepo

-- End generic instances

unit_V1P1:: [Test]
unit_V1P1 =
  [ testCases "known commutes" Prop1.checkCommute Ex.knownCommutes
  , testCases "known non-commutes" Prop1.checkCantCommute Ex.knownCantCommutes
  , testCases "known merges" Prop1.checkMerge Ex.knownMerges
  , testCases "known merges (equiv)" Prop1.checkMergeEquiv Ex.knownMergeEquivs
  , testCases "known canons" Prop1.checkCanon Ex.knownCanons
  , testCases "merge swaps" Prop1.checkMergeSwap Ex.mergePairs2
  , testCases "the patch validation works" Prop1.tTestCheck Ex.validPatches
  , testCases "commute/recommute" (PropG.recommute commute) Ex.commutePairs
  , testCases "merge properties: merge either way valid" PropG.mergeEitherWayValid Ex.mergePairs
  , testCases "merge properties: merge swap" PropG.mergeEitherWay Ex.mergePairs
  , testCases "primitive patch IO functions" (Prop1.tShowRead eqFLUnsafe) Ex.primitiveTestPatches
  , testCases "IO functions (test patches)" (Prop1.tShowRead eqFLUnsafe) Ex.testPatches
  , testCases "IO functions (named test patches)" (Prop1.tShowRead unsafeCompare) Ex.testPatchesNamed
  , testCases "primitive commute/recommute" (PropG.recommute commute) Ex.primitiveCommutePairs
  ]

unit_V2P1 :: [Test]
unit_V2P1 =
  [ testCases "coalesce commute" (PropU.coalesceCommute WSub.coalesce) ExU.primPermutables
  , testCases "prim recommute" (PropU.recommute WSub.commute) ExU.commutables
  , testCases "square commute law" (PropU.squareCommuteLaw WSub.commute) ExU.commutables
  , testCases "prim inverses commute" (PropU.commuteInverses WSub.commute) ExU.commutables
  , testCases "FL prim recommute" (PropU.recommute WSub.commute) ExU.commutablesFL
  , testCases "FL square commute law" (PropU.squareCommuteLaw WSub.commute) ExU.commutablesFL
  , testCases "FL prim inverses commute" (PropU.commuteInverses WSub.commute) $ ExU.commutablesFL
  , testCases "fails" (PropU.commuteFails WSub.commute) ([] :: [(Prim2 WSub.:> Prim2) wX wY])
  , testCases "read and show work on Prim" PropU.showRead ExU.primPatches
  , testCases "read and show work on RepoPatchV2" PropU.showRead ExU.repov2Patches
  , testCases "example flattenings work" (PropR.propConsistentTreeFlattenings fromPrim2) ExU.repov2PatchLoopExamples
  , testCases "V2 merge input consistent" (PropU.mergeArgumentsConsistent isConsistent) ExU.repov2Mergeables
  , testCases "V2 merge input is forward" (PropU.mergeArgumentsConsistent isForward) ExU.repov2Mergeables
  , testCases "V2 merge output is forward" (PropU.mergeConsistent isForward) ExU.repov2Mergeables
  , testCases "V2 merge output consistent" (PropU.mergeConsistent isConsistent) ExU.repov2Mergeables
  , testCases "V2 merge either way" PropU.mergeEitherWay ExU.repov2Mergeables
  , testCases "V2 merge and commute" PropU.mergeCommute ExU.repov2Mergeables

  , testCases "V2 recommute" (PropU.recommute WSub.commute) ExU.repov2Commutables
  , testCases "V2 inverses commute" (PropU.commuteInverses WSub.commute) ExU.repov2Commutables
  , testCases "V2 permutivity" (PropU.permutivity WSub.commute) ExU.repov2NonduplicateTriples
  ]
  where
    fromPrim2 :: PropR.FromPrimT RepoPatchV2 Prim2
    fromPrim2 = fromAnonymousPrim

arbitraryThing :: TestGenerator thing (Sealed2 thing)
arbitraryThing = TestGenerator (\f p -> Just (unseal2 f p))

arbitraryWSThing :: TestGenerator thing (Sealed2 (WithState thing))
arbitraryWSThing = TestGenerator (\f wsp -> Just (unseal2 (f . wsPatch) wsp))

qc_prim :: forall prim.
           ( TestablePrim prim
           , Show2 prim
           , Show1 (ModelOf prim)
           , MightBeEmptyHunk prim
           , MightHaveDuplicate prim
           , ArbitraryWS prim
           , ArbitraryState prim
           ) => [Test]
qc_prim =
  -- The following fails because of setpref patches:
  -- testProperty "prim inverse doesn't commute" (inverseDoesntCommute :: Prim -> Maybe Doc)
  (case runCoalesceTests @prim of
    Just Dict ->
      [ testProperty "prim coalesce effect preserving"
        (unseal2 $ PropG.coalesceEffectPreserving coalesce :: Sealed2 (WithState (Pair prim)) -> TestResult)
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
  , [ testProperty "readPatch/showPatch"
      (unseal2 $ (PropG.showRead . wsPatch) :: Sealed2 (WithState prim) -> TestResult)
    , testProperty "readPatch/showPatch (FL)"
      (unseal2 $ (PropG.showRead . wsPatch) :: Sealed2 (WithState (FL prim)) -> TestResult)
    ]
  ]

qc_named_prim :: forall prim.
                 ( TestablePrim prim
                 , Show2 prim
                 , Show1 (ModelOf (NamedPrim prim))
                 , MightBeEmptyHunk prim
                 , ArbitraryState prim
                 , RepoModel (ModelOf prim)
                 , ArbitraryState prim
                 ) => [Test]
qc_named_prim =
  qc_prim @(NamedPrim prim) ++
  [ testProperty
      "prim inverse doesn't commute"
      (unseal2 $ (PropG.inverseDoesntCommute . wsPatch) :: Sealed2 (WithState (NamedPrim prim)) -> TestResult)
  ]


qc_V1P1 :: [Test]
qc_V1P1 =
  repoPatchProperties @(RepoPatchV1 V1.Prim) ++
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

qc_V2 :: forall prim wXx wYy.
         ( PrimPatch prim
         , Show1 (ModelOf prim)
         , ShrinkModel prim
         , PropagateShrink prim prim
         , ArbitraryPrim prim
         , Shrinkable prim
         , RepoState (ModelOf prim) ~ ApplyState prim
         )
      => prim wXx wYy -> [Test]
qc_V2 _ =
  [ testProperty "with quickcheck that patches are consistent"
    (withSingle consistent)
  ]
  ++ repoPatchProperties @(RepoPatchV2 prim)
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

qc_V3 :: forall prim wXx wYy.
         ( PrimPatch prim
         , Show1 (ModelOf prim)
         , ShrinkModel prim
         , PropagateShrink prim prim
         , ArbitraryPrim prim
         , Shrinkable prim
         , RepoState (ModelOf prim) ~ ApplyState prim
         )
      => prim wXx wYy
      -> [Test]
qc_V3 _ =
  [ testProperty "repo invariants"
    (withSequence (PropR3.prop_repoInvariants :: SequenceProperty (RepoPatchV3 prim)))
  ]
  ++ repoPatchProperties @(RepoPatchV3 prim)
  ++ difficultRepoPatchProperties @(RepoPatchV3 prim)

repoPatchProperties :: forall p.
                       ( ArbitraryRepoPatch p
                       , Show2 p
                       , Show1 (ModelOf p)
                       , CheckedMerge p
                       , ShrinkModel (PrimOf p)
                       , PrimBased p
                       )
                    => [Test]
repoPatchProperties =
  [ testProperty "readPatch/showPatch"
      (withSingle (PropG.showRead :: PatchProperty p))
  , testProperty "readPatch/showPatch (RL)"
      (withSequence (PropG.showRead :: SequenceProperty p))
{- we no longer support inversion for RepoPatches
  , testProperty "invert involution"
      (withSingle (PropG.invertInvolution :: PatchProperty p))
  , testProperty "inverse composition"
      (withPair (PropG.inverseComposition :: PairProperty p))
-}
  , testProperty "resolutions don't conflict"
      (withSequence (PropR.propResolutionsDontConflict :: SequenceProperty p))
  ]

-- | These properties regularly fail for RepoPatchV2 with the standard test
-- case generator when we crank up the number of tests (to e.g. 10000).
difficultRepoPatchProperties :: forall p.
                       ( ArbitraryRepoPatch p
                       , ShrinkModel (PrimOf p)
                       , Show2 p
                       , CheckedMerge p
                       , MightHaveDuplicate p
                       , Show1 (ModelOf p)
                       , PrimBased p
                       )
                    => [Test]
difficultRepoPatchProperties =
  [ testProperty "reorderings are consistent"
      (PropR.propConsistentReorderings @p)
{- we no longer support inversion for RepoPatches
  , testProperty "inverses commute"
      (withPair (PropG.commuteInverses com))
  , testConditional "nontrivial inverses commute"
      (withPair nontrivialCommute)
      (withPair (PropG.commuteInverses com))
-}
  , testProperty "recommute"
      (withPair (PropG.recommute com))
  , testConditional "nontrivial recommute"
      (fromNothing . withPair nontrivialCommute)
      (withPair (PropG.recommute com))
  , testConditional "permutivity"
      (fromNothing . withTriple notDuplicatestriple)
      (withTriple (PropG.permutivity com))
  , testConditional "nontrivial permutivity"
      (fromNothing . withTriple (\t -> nontrivialTriple t && notDuplicatestriple t))
      (withTriple (PropG.permutivity com))
  , testProperty "merge either way"
      (withFork (PropG.mergeEitherWay :: MergeProperty p))
{- this test relies on inversion and is thereore only valid for prims
  , testProperty "merge either way valid"
      (withFork (PropG.mergeEitherWayValid :: MergeProperty p))
-}
  , testConditional "nontrivial merge either way"
      (fromNothing . withFork nontrivialMerge)
      (withFork (PropG.mergeEitherWay :: MergeProperty p))
  , testProperty "merge commute"
      (withFork (PropG.mergeCommute :: MergeProperty p))
  , testProperty "resolutions are invariant under reorderings"
      (withSequence (PropR.propResolutionsOrderIndependent :: SequenceProperty p))
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
  , ("nontrivial recommute"   , TestCondition nontrivialCommute, TestCheck (PropG.recommute commute)           )
  , ("inverses commute"       , TestCondition (const True)     , TestCheck (PropG.commuteInverses commute)     )
  , ("nontrivial inverses"    , TestCondition nontrivialCommute, TestCheck (PropG.commuteInverses commute)     )
  , ("inverse composition"    , TestCondition (const True)     , TestCheck PropG.inverseComposition            )
  ]

coalesce_properties :: forall p gen
                     . ( Show gen, Arbitrary gen, TestablePrim p
                       , MightBeEmptyHunk p
                       )
                    => PropList (Triple p) gen
coalesce_properties genname gen =
  properties gen "commute" genname
   (case runCoalesceTests @p of
      Just Dict ->
        [ ( "coalesce commutes with commute"
          , TestCondition (const True)
          , TestCheck (PropG.coalesceCommute coalesce . getTriple))
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
  , ("nontrivial square commute law", TestCondition nontrivialCommute, TestCheck (PropG.squareCommuteLaw commute))
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
     , Invert p, Apply p, ShowPatchBasic p
     , RepoModel (ModelOf p)
     , RepoState (ModelOf p) ~ ApplyState p
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
  , ("nontrivial merge either way", TestCondition nontrivialMerge, TestCheck PropG.mergeEitherWay      )
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
  , ( "nontrivial permutivity"
    , TestCondition (\(Triple t) -> nontrivialTriple t && notDuplicatestriple t)
    , TestCheck (PropG.permutivity commute . getTriple) )
  ]

pair_repo_properties
  :: forall p gen .
     ( Show gen
     , Arbitrary gen
     , Commute p
     , Apply p
     , ShowPatchBasic p
     , MightBeEmptyHunk p
     , RepoModel (ModelOf p)
     , RepoState (ModelOf p) ~ ApplyState p
     )
  => PropList (WithState (Pair p)) gen
pair_repo_properties genname gen =
  properties gen "patch/repo" genname
    [ ( "commute is effect preserving"
      , TestCondition (const True)
      , TestCheck (PropG.effectPreserving commute))
    ]

-- tests (either QuickCheck or Unit) that should be run on any type of patch
general_patchTests
  :: forall p
   . ( ArbitraryRepoPatch p, CheckedMerge p
     , PrimBased p, Commute (OnlyPrim p), ArbitraryPrim (OnlyPrim p)
     , ShrinkModel (PrimOf p)
     , Show1 (ModelOf (PrimOf p)), Show2 p
     )
  => [Test]
general_patchTests =
     [ testGroup "Rebase patches" $ Rebase.testSuite @p
     , testGroup "Unwind" $ Unwind.testSuite @p
     ]

-- | This is the big list of tests that will be run using testrunner.
testSuite :: [Test]
testSuite =
    [ primTests
    , repoPatchV1Tests
    , repoPatchV2Tests
    , repoPatchV3Tests
    , Darcs.Test.Patch.Info.testSuite
    , Darcs.Test.Patch.Selection.testSuite
    ]
  where
    primTests = testGroup "Prim patches"
      [ testGroup "V1.Prim wrapper for Prim.V1" $ qc_prim @Prim1
      , testGroup "V2.Prim wrapper for Prim.V1" $ qc_prim @Prim2
      , testGroup "Prim.FileUUID" $ qc_prim @FileUUID.Prim
      , testGroup "NamedPrim over V2.Prim" $ qc_named_prim @Prim2
      , testGroup "NamedPrim over Prim.FileUUID" $ qc_named_prim @FileUUID.Prim
      ]
    repoPatchV1Tests = testGroup "RepoPatchV1"
      [ testGroup "using V1.Prim wrapper for Prim.V1" $
          unit_V1P1 ++ qc_V1P1 ++
          general_patchTests @(RepoPatchV1 Prim1)
      ]
    repoPatchV2Tests = testGroup "RepoPatchV2"
      [ testGroup "using V2.Prim wrapper for Prim.V1" $
          unit_V2P1 ++ qc_V2 (undefined :: Prim2 wX wY) ++
          general_patchTests @(RepoPatchV2 Prim2)
      , testGroup "using Prim.FileUUID" $
          qc_V2 (undefined :: FileUUID.Prim wX wY) ++
          general_patchTests @(RepoPatchV2 FileUUID.Prim)
      ]
    repoPatchV3Tests = testGroup "RepoPatchV3"
      [ testGroup "using V2.Prim wrapper for Prim.V1" $
          qc_V3 (undefined :: Prim2 wX wY) ++
          general_patchTests @(RepoPatchV3 Prim2)
      , testGroup "using Prim.FileUUID" $
          qc_V3 (undefined :: FileUUID.Prim wX wY) ++
          general_patchTests @(RepoPatchV3 FileUUID.Prim)
      ]
