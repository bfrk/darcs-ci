{-# LANGUAGE UndecidableInstances #-}
module Darcs.Test.Patch.Types.MergeableSequence
  ( MergeableSequence(..)
  , arbitraryMergeableSequence
  , mergeableSequenceToRL
  , WithSplit(..)
  ) where

import Darcs.Prelude

import Control.Applicative ( (<|>) )
import Test.QuickCheck

import Darcs.Test.Patch.Arbitrary.Generic ( PrimBased(..) )
import Darcs.Test.Patch.Arbitrary.Shrink
import Darcs.Test.Patch.Merge.Checked
import Darcs.Test.Patch.Types.Merged ( Merged, typedMerge )
import Darcs.Test.Patch.WithState
import Darcs.Test.Patch.RepoModel
import Darcs.Test.Util.QuickCheck ( bSized )
import Darcs.Patch.Witnesses.Maybe
import Darcs.Patch.Witnesses.Sealed
import Darcs.Patch.Witnesses.Unsafe
import Darcs.Patch.Witnesses.Ordered
import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch.Effect ( Effect(..) )
import Darcs.Patch.Invert ( Invert(..) )
import Darcs.Patch.FromPrim ( PrimPatchBase, PrimOf )
import Darcs.Patch.Prim ( sortCoalesceFL, PrimCoalesce )
import Darcs.Patch.Witnesses.Show

-- | This type provides a concrete, pre-merged representation of a sequence
-- of patches that might have conflicts once merged. The structure also allows
-- for conflict resolutions, e.g. in @SeqMS (ParMS x y) z@, @z@ could be a
-- resolution patch.
-- Working with the pre-merged patches makes it easier to manipulate the test
-- case, e.g. for shrinking.
-- Note that although MergeableSequence is parameterised on a patch type @p@
-- that needs to support merging, it only explicitly contains primitive
-- patches. The merged patches are constructed on-the-fly when the structure
-- is used. It's necessary to fix the structure to a specific mergeable patch
-- type because otherwise the merged patches could vary, invalidating the
-- context of conflict resolution patches like @z@.
data MergeableSequence p wX wY where
  NilMS :: MergeableSequence p wX wX
  SeqMS
    :: MergeableSequence p wX wY
    -> OnlyPrim p wY wZ
    -> MergeableSequence p wX wZ
  ParMS
    :: MergeableSequence p wX wA
    -> MergeableSequence p wX wB
    -> MergeableSequence p wX (Merged wA wB)

instance PrimPatchBase p => PrimPatchBase (MergeableSequence p) where
  type PrimOf (MergeableSequence p) = PrimOf p

instance (CheckedMerge p, PrimBased p) => Effect (MergeableSequence p) where
  effect NilMS = NilFL
  effect (SeqMS ps p) = effect ps +>+ primEffect @p p
  effect (ParMS ms1 ms2) =
    let ps1 = mergeableSequenceToRL ms1
        ps2 = mergeableSequenceToRL ms2
    in case typedMerge (reverseRL ps1 :\/:reverseRL ps2) of
      (ps2', _) -> effect ms1 +>+ effect ps2'


instance
  ( PropagateShrink prim (OnlyPrim p)
  , CheckedMerge p, Effect p, PrimOf p ~ prim
  , Invert prim, PrimCoalesce prim
  , PrimBased p
  )
  => PropagateShrink prim (MergeableSequence p) where
  -- Note that the result of propagateShrink is always either
  -- Just (Just2 _ :> _) or Nothing, so we don't need to worry about
  -- the Just (Nothing2 :> _) case in recursive calls.
  propagateShrink (prim :> NilMS) = Just (Just2 NilMS :> Just2 prim)
  propagateShrink (prim :> SeqMS ps p) = do
    Just2 ps' :> mprim' <- propagateShrink (prim :> ps)
    mp' :> mprim'' <- propagateShrinkMaybe (mprim' :> p)
    let result = case mp' of
          Just2 p' -> SeqMS ps' p'
          Nothing2 -> ps'
    return (Just2 result :> mprim'')
  propagateShrink
    ((prim :: prim wA wB) :>
       ParMS (ms1 :: MergeableSequence p wB wD1) (ms2 :: MergeableSequence p wB wD2)) = do
    Just2 (ms1' :: MergeableSequence p wA wC1) :> (mprim1' :: Maybe2 prim wC1 wD1)
      <- propagateShrink (prim :> ms1)
    Just2 (ms2' :: MergeableSequence p wA wC2) :> (mprim2' :: Maybe2 prim wC2 wD2)
      <- propagateShrink (prim :> ms2)
    let
      ms' :: MergeableSequence p wA (Merged wC1 wC2)
      ms' = parMS ms1' ms2'
      ps1  :: FL p wB wD1
      ps2  :: FL p wB wD2
      mergedps1 :: FL p wD2 (Merged wD1 wD2)
      mergedps2 :: FL p wD1 (Merged wD1 wD2)
      ps1' :: FL p wA wC1
      ps2' :: FL p wA wC2
      mergedps1' :: FL p wC2 (Merged wC1 wC2)
      mergedps2' :: FL p wC1 (Merged wC1 wC2)
      ps1  = reverseRL (mergeableSequenceToRL ms1)
      ps2  = reverseRL (mergeableSequenceToRL ms2)
      ps1' = reverseRL (mergeableSequenceToRL ms1')
      ps2' = reverseRL (mergeableSequenceToRL ms2')
      (mergedps2 , mergedps1 ) = typedMerge (ps1  :\/: ps2 )
      (mergedps2', mergedps1') = typedMerge (ps1' :\/: ps2')
      -- Unless the shrinking prim disappears on both branches of the merge,
      -- we'll need to try to recalculate it for the result of the merge - trying
      -- to use propagateShrink a second time wouldn't guarantee the right
      -- contexts. (This is a bit complicated to see, hence all the type signatures
      -- in this function.)
      recalcShrink
        :: prim wX wY
        -> FL p wY (Merged wD1 wD2)
        -> FL p wX (Merged wC1 wC2)
        -> Maybe (Maybe2 prim (Merged wC1 wC2) (Merged wD1 wD2))
      recalcShrink primIn m1 m2 =
        case sortCoalesceFL (invert (effect m2) +>+ primIn :>: effect m1) of
          NilFL -> Just Nothing2
          prim' :>: NilFL -> Just (Just2 prim')
          -- If we don't get 0 or 1 prims, we can't use this result given the type
          -- of propagateShrink as a whole. If that was changed to return an FL we
          -- could use it, but at the cost of more complexity elsewhere.
          _ -> Nothing
    mprim' :: Maybe2 prim (Merged wC1 wC2) (Merged wD1 wD2)
      <-
      case (mprim1', mprim2') of
        (Nothing2, Nothing2) -> Just Nothing2
        (Just2 prim1', _) | Just prim'' <- recalcShrink prim1' mergedps2 mergedps2' -> Just prim''
        (_, Just2 prim2') | Just prim'' <- recalcShrink prim2' mergedps1 mergedps1' -> Just prim''
        _ -> Nothing
    return (Just2 ms' :> mprim')

instance (Show2 p, PrimBased p) => Show (MergeableSequence p wX wY) where
  showsPrec _d NilMS = showString "NilMS"
  showsPrec d (SeqMS ms p) =
    showParen (d > appPrec) $ showString "SeqMS " . showsPrec2 (appPrec + 1) ms . showString " " . showsPrec2 (appPrec + 1) p
  showsPrec d (ParMS ms1 ms2) =
    showParen (d > appPrec) $ showString "ParMS " . showsPrec2 (appPrec + 1) ms1 . showString " " . showsPrec2 (appPrec + 1) ms2

instance (Show2 p, PrimBased p) => Show1 (MergeableSequence p wX)
instance (Show2 p, PrimBased p) => Show2 (MergeableSequence p)

type instance ModelOf (MergeableSequence p) = ModelOf p

parMS
  :: MergeableSequence p wX wA
  -> MergeableSequence p wX wB
  -> MergeableSequence p wX (Merged wA wB)
parMS NilMS ms = unsafeCoercePEnd ms
parMS ms NilMS = unsafeCoercePEnd ms
parMS ms1 ms2 = ParMS ms1 ms2

instance Shrinkable (OnlyPrim p) => Shrinkable (MergeableSequence p) where
  shrinkInternally NilMS = []
  shrinkInternally (SeqMS ms p) =
    SeqMS ms <$> shrinkInternally p
      <|>
    flip SeqMS p <$> shrinkInternally ms
  shrinkInternally (ParMS ms1 ms2) =
    parMS ms1 <$> shrinkInternally ms2
      <|>
    flip parMS ms2 <$> shrinkInternally ms1

  shrinkAtStart NilMS = []
  shrinkAtStart (SeqMS NilMS p) = mapFlipped (SeqMS NilMS) <$> shrinkAtStart p
  shrinkAtStart (ParMS {}) = []
  shrinkAtStart (SeqMS (ParMS {}) p) = [FlippedSeal (SeqMS NilMS p)]
  shrinkAtStart (SeqMS ms p) = mapFlipped (flip SeqMS p) <$> shrinkAtStart ms

  shrinkAtEnd NilMS = []
  shrinkAtEnd (SeqMS ms p) =
    Sealed ms:map (mapSeal (SeqMS ms)) (shrinkAtEnd p)
  shrinkAtEnd (ParMS ms1 ms2) =
    do
      Sealed ms2' <- shrinkAtEnd ms2
      return $ Sealed $ parMS ms1 ms2'
     <|>
    do
      Sealed ms1' <- shrinkAtEnd ms1
      return $ Sealed $ parMS ms1' ms2

mergeableSequenceToRL
  :: (CheckedMerge p, PrimBased p)
  => MergeableSequence p wX wY
  -> RL p wX wY
mergeableSequenceToRL NilMS = NilRL
mergeableSequenceToRL (SeqMS ms p) = mergeableSequenceToRL ms :<: liftFromPrim p
mergeableSequenceToRL (ParMS ms1 ms2) =
  let
    ps1 = mergeableSequenceToRL ms1
    ps2 = mergeableSequenceToRL ms2
  in
    case typedMerge (reverseRL ps1 :\/: reverseRL ps2) of
      (ps2', _) -> ps1 +<<+ ps2'

-- | Generate an arbitrary sequence of patches, using a generator
-- for the underlying patch type and merging.
-- The sequence uses a given start state and is bounded by a
-- given depth.
arbitraryMergeableSequence
  :: forall model p wX
   . ( RepoModel model
     , CheckedMerge p
     , PrimBased p
     , RepoApply p, ApplyState p ~ RepoState model
     )
  => (forall wA . model wA -> Gen (Sealed (WithEndState model (OnlyPrim p wA))))
  -> model wX
  -> Int
  -> Gen (Sealed (WithEndState model (MergeableSequence p wX)))
arbitraryMergeableSequence arbitrarySingle = go
  where
    go rm depth
      | depth == 0 = return $ Sealed $ WithEndState NilMS rm
      | otherwise =
        frequency
          [ ( 1
            , do Sealed (WithEndState ms rm') <- go rm (depth - 1)
                 Sealed (WithEndState p rm'') <- arbitrarySingle rm'
                 return $ Sealed $ WithEndState (SeqMS ms p) rm'')
          , ( 3
            , do Sealed (WithEndState ms1 _) <- go rm ((depth + 1) `div` 2)
                 Sealed (WithEndState ms2 _) <- go rm (depth `div` 2)
                 let ps1 = mergeableSequenceToRL ms1
                     ps2 = mergeableSequenceToRL ms2
                 case validateMerge @p (typedMerge (reverseRL ps1 :\/:reverseRL ps2)) of
                   Nothing -> go rm depth
                   Just (ps2', _) ->
                     return $
                       Sealed $
                       WithEndState (parMS ms1 ms2) $ unFail $ repoApply rm (ps1 +>>+ ps2')
            )
          ]

instance
  ( RepoModel model
  , RepoApply p, ApplyState p ~ RepoState model
  , model ~ ModelOf (OnlyPrim p)
  , model ~ ModelOf p
  , CheckedMerge p
  , PrimBased p
  )
  => ArbitraryState (MergeableSequence p) where
  arbitraryState rm = bSized 3 0.035 9 $ arbitraryMergeableSequence arbitraryState rm

data WithSplit p wX wY = WithSplit Int (p wX wY)

type instance ModelOf (WithSplit p) = ModelOf p

instance PrimPatchBase p => PrimPatchBase (WithSplit p) where
  type PrimOf (WithSplit p) = PrimOf p

instance Effect p => Effect (WithSplit p) where
  effect (WithSplit _ p) = effect p

instance Shrinkable p => Shrinkable (WithSplit p) where
  shrinkInternally (WithSplit n ms) = map (WithSplit n) (shrinkInternally ms)
  shrinkAtStart (WithSplit n ms) = map (mapFlipped (WithSplit n)) (shrinkAtStart ms)
  shrinkAtEnd (WithSplit n ms) = map (mapSeal (WithSplit n)) (shrinkAtEnd ms)

instance
  ( PropagateShrink prim (OnlyPrim p)
  , CheckedMerge p, Effect p, PrimOf p ~ prim
  , Invert prim, PrimCoalesce prim
  , PrimBased p
  )
  => PropagateShrink prim (WithSplit (MergeableSequence p)) where
  propagateShrink (x :> WithSplit n p) =
    case propagateShrink (x :> p) of
      Nothing -> Nothing
      Just (Just2 p' :> x') -> Just (Just2 (WithSplit n p') :> x')
      Just (Nothing2 :> x') -> Just (Nothing2 :> x')

instance Show2 p => Show2 (WithSplit p)

instance Show2 p => Show (WithSplit p wX wY) where
  showsPrec d (WithSplit n p) =
    showParen (d > appPrec) $
      showString "WithSplit " . shows n . showString " " . showsPrec2 (appPrec + 1) p

instance
  ( RepoModel model
  , RepoApply p, ApplyState p ~ RepoState model
  , model ~ ModelOf (OnlyPrim p)
  , model ~ ModelOf p
  , CheckedMerge p
  , PrimBased p
  )
  => ArbitraryState (WithSplit (MergeableSequence p)) where
  arbitraryState s = do
    Sealed (WithEndState ms s') <- arbitraryState s
    n <- chooseInt (0, lengthMS ms)
    return $ seal $ WithEndState (WithSplit n ms) s'

lengthMS :: MergeableSequence p wX wY -> Int
lengthMS NilMS = 0
lengthMS (ParMS a b) = lengthMS a + lengthMS b
lengthMS (SeqMS a _) = lengthMS a + 1
