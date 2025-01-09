{-# LANGUAGE UndecidableInstances, ViewPatterns #-}
-- | Test case generator for patch with a Merge instance
module Darcs.Test.Patch.Arbitrary.Mergeable
  ( withSingle
  , withPair
  , withTriple
  , withFork
  , withSequence
  , withSequencePair
  , withAllSequenceItems
  , NotRepoPatchV1(..)
  , ArbitraryMergeable(..)
  ) where

import Darcs.Prelude

import Darcs.Test.Patch.WithState
import Darcs.Test.Patch.RepoModel
import Darcs.Test.Patch.Arbitrary.Generic ( ArbitraryPrim(..), PrimBased )
import Darcs.Test.Patch.Merge.Checked ( CheckedMerge )
import Darcs.Test.Patch.Types.MergeableSequence
  ( MergeableSequence(..)
  , WithSplit(..)
  , mergeableSequenceToRL
  )
import Darcs.Test.Patch.Types.Pair ( Pair(..) )
import Darcs.Patch.Witnesses.Sealed
import Darcs.Patch.Witnesses.Ordered hiding ( Fork )
import Darcs.Patch.Apply ( Apply(..) )
import Darcs.Patch.FromPrim ( PrimOf )
import Darcs.Patch.V1 ( RepoPatchV1 )

import Data.Constraint
import Data.Void

data NotRepoPatchV1 p = NotRepoPatchV1 (forall prim . Dict (p ~ RepoPatchV1 prim) -> Void)

-- | Class to simplify type signatures and superclass constraints.
class
  ( ArbitraryPrim (PrimOf p)
  , RepoModel (ModelOf p)
  , ApplyState p ~ RepoState (ModelOf p)
  ) => ArbitraryMergeable p where

  notRepoPatchV1 :: Maybe (NotRepoPatchV1 p)


withSingle
  :: (CheckedMerge p, PrimBased p)
  => (forall wX wY. p wX wY -> r)
  -> Sealed2 (WithStartState2 (MergeableSequence p)) -> Maybe r
withSingle prop (Sealed2 (WithStartState2 _ ms))
  = case mergeableSequenceToRL ms of
      _ :<: pp -> Just (prop pp)
      _ -> Nothing

withPair
  :: (CheckedMerge p, PrimBased p)
  => (forall wX wY. Pair p wX wY -> r)
  -> Sealed2 (WithStartState2 (MergeableSequence p)) -> Maybe r
withPair prop (Sealed2 (WithStartState2 _ ms))
  = case mergeableSequenceToRL ms of
      _ :<: pp1 :<: pp2 -> Just (prop (Pair (pp1 :> pp2)))
      _ -> Nothing

withTriple
  :: (CheckedMerge p, PrimBased p)
  => (forall wX wY. (p :> p :> p) wX wY -> r)
  -> Sealed2 (WithStartState2 (MergeableSequence p)) -> Maybe r
withTriple prop (Sealed2 (WithStartState2 _ ms))
  = case mergeableSequenceToRL ms of
      _ :<: pp1 :<: pp2 :<: pp3 -> Just (prop (pp1 :> pp2 :> pp3))
      _ -> Nothing

withFork
  :: (CheckedMerge p, PrimBased p)
  => (forall wX wY. (FL p :\/: FL p) wX wY -> r)
  -> Sealed2 (WithStartState2 (MergeableSequence p)) -> Maybe r
-- We can't use (MergeableSequence p:\/: MergeableSequence p) as the input because
-- the witnesses would be wrong, so just use MergeableSequence p and choose the
-- ParMS cases.
withFork prop (Sealed2 (WithStartState2 _ (ParMS ms1 ms2)))
  = Just (prop (reverseRL (mergeableSequenceToRL ms1) :\/: reverseRL (mergeableSequenceToRL ms2)))
withFork _ _ = Nothing

withSequence
  :: (CheckedMerge p, PrimBased p)
  => (forall wX wY. RL p wX wY -> r)
  -> Sealed2 (WithStartState2 (MergeableSequence p)) -> r
withSequence prop (Sealed2 (WithStartState2 _ ms))
  = prop (mergeableSequenceToRL ms)

withSequencePair
  :: (CheckedMerge p, PrimBased p)
  => (forall wX wY. (RL p :> RL p) wX wY -> r)
  -> Sealed2 (WithStartState2 (WithSplit (MergeableSequence p))) -> r
withSequencePair prop (Sealed2 (WithStartState2 _ (WithSplit n ms)))
  = prop (splitAtRL n (mergeableSequenceToRL ms))

withAllSequenceItems
  :: (CheckedMerge p, PrimBased p, Monoid r)
  => (forall wX wY. p wX wY -> r)
  -> Sealed2 (WithStartState2 (MergeableSequence p)) -> r
withAllSequenceItems prop (Sealed2 (WithStartState2 _ ms))
  = mconcat . mapRL prop . mergeableSequenceToRL $ ms
