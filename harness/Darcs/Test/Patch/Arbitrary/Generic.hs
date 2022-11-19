{-# LANGUAGE UndecidableInstances #-}
module Darcs.Test.Patch.Arbitrary.Generic
  ( ArbitraryPrim(..)
  , ShrinkPrim
  , TestablePrim
  , PrimBased(..)
  , NullPatch(..)
  , RepoModel(..)
  , MightBeEmptyHunk(..)
  , MightHaveDuplicate(..)
  , nontrivialCommute
  , nontrivialTriple
  , nontrivialMerge
  , notDuplicatestriple
  , MergeableSequence(..)
  , arbitraryMergeableSequence
  , mergeableSequenceToRL
  ) where

import Darcs.Prelude

import Control.Applicative ( (<|>) )
import Data.Constraint (Dict(..))
import Test.QuickCheck

import Darcs.Test.Patch.Arbitrary.Shrink
import Darcs.Test.Patch.Merge.Checked
import Darcs.Test.Patch.WithState
import Darcs.Test.Patch.RepoModel
import Darcs.Test.Patch.V1Model
import Darcs.Test.Util.QuickCheck ( bSized )
import Darcs.Patch.Witnesses.Maybe
import Darcs.Patch.Witnesses.Sealed
import Darcs.Patch.Witnesses.Eq
import Darcs.Patch.Witnesses.Unsafe
import Darcs.Patch.Witnesses.Ordered
import Darcs.Patch.Apply ( Apply, ApplyState )
import Darcs.Patch.Effect ( Effect(..) )
import Darcs.Patch.Format ( PatchListFormat )
import Darcs.Patch.Merge ( CleanMerge, Merge(..), mergerFLFL )
import Darcs.Patch.Invert ( Invert(..) )
import Darcs.Patch.Commute ( Commute(..) )
import Darcs.Patch.FromPrim ( PrimPatchBase, PrimOf )
import Darcs.Patch.Prim ( sortCoalesceFL, PrimCoalesce, PrimConstruct )
import Darcs.Patch.Read ( ReadPatch )
import Darcs.Patch.Show ( ShowPatchBasic )
import Darcs.Patch.Witnesses.Show

class NullPatch p where
  nullPatch :: p wX wY -> EqCheck wX wY

class MightBeEmptyHunk p where
  -- |V1 Prims support the value 'Hunk n [] []' that is treated specially in the
  -- commute code and ends up breaking certain tests by behaving anomalously.
  -- In practice they shouldn't appear in real repositories. For later,
  -- as yet unreleased patch types, we should eliminate them completely.
  -- An alternative to using this as a guard might be to avoid generating them.
  isEmptyHunk :: p wX wY -> Bool
  isEmptyHunk _ = False

instance MightBeEmptyHunk (FL p)

class MightHaveDuplicate p where
  -- |"duplicates" in V2 patches (RepoPatchV2) have lots of bugs
  -- that break various commute/merge properties.
  hasDuplicate :: p wX wY -> Bool
  hasDuplicate _ = False

instance MightHaveDuplicate p => MightHaveDuplicate (FL p) where
  hasDuplicate NilFL = False
  hasDuplicate (p :>: ps) = hasDuplicate p || hasDuplicate ps

nontrivialCommute :: (Commute p, Eq2 p) => Pair p wX wY -> Bool
nontrivialCommute (Pair (x :> y)) =
  case commute (x :> y) of
    Just (y' :> x') -> not (y' `unsafeCompare` y) || not (x' `unsafeCompare` x)
    Nothing -> False

nontrivialMerge :: (Eq2 p, Merge p) => (p :\/: p) wX wY -> Bool
nontrivialMerge (x :\/: y) =
  case merge (x :\/: y) of
    y' :/\: x' -> not (y' `unsafeCompare` y) || not (x' `unsafeCompare` x)

nontrivialTriple :: (Eq2 p, Commute p) => (p :> p :> p) wX wY -> Bool
nontrivialTriple (a :> b :> c) =
  case commute (a :> b) of
    Nothing -> False
    Just (b' :> a') ->
      case commute (a' :> c) of
        Nothing -> False
        Just (c'' :> a'') ->
          case commute (b :> c) of
            Nothing -> False
            Just (c' :> b'') ->
              (not (a `unsafeCompare` a') || not (b `unsafeCompare` b')) &&
              (not (c' `unsafeCompare` c) || not (b'' `unsafeCompare` b)) &&
              (not (c'' `unsafeCompare` c) || not (a'' `unsafeCompare` a'))

notDuplicatestriple :: MightHaveDuplicate p => (p :> p :> p) wX wY -> Bool
notDuplicatestriple (a :> b :> c) =
  not (hasDuplicate a || hasDuplicate b || hasDuplicate c)

class ( ArbitraryState prim
      , NullPatch prim
      , RepoModel (ModelOf prim)
      , Shrinkable prim
      )
      => ArbitraryPrim prim
    where
        -- hooks to disable certain kinds of tests for certain kinds of patches

        -- These tests depend on the PrimCoalesce class, which may not be
        -- implemented. By passing the implementation in explicitly only where
        -- it is available, we can avoid having to have dummy instances that
        -- won't be used.
        runCoalesceTests :: Maybe (Dict (PrimCoalesce prim))
        default runCoalesceTests :: PrimCoalesce prim => Maybe (Dict (PrimCoalesce prim))
        runCoalesceTests = Just Dict

        -- TODO in practice both hasPrimConstruct and usesV1Model will only work for V1 prims
        -- and their newtypes. Consider merging into one method.

        hasPrimConstruct :: Maybe (Dict (PrimConstruct prim))
        default hasPrimConstruct :: PrimConstruct prim => Maybe (Dict (PrimConstruct prim))
        hasPrimConstruct = Just Dict

        usesV1Model :: Maybe (Dict (ModelOf prim ~ V1Model))
        default usesV1Model :: ModelOf prim ~ V1Model => Maybe (Dict (ModelOf prim ~ V1Model))
        usesV1Model = Just Dict

type ShrinkPrim prim =
  ( ShrinkModel prim
  , PropagateShrink prim prim
  )

type TestablePrim prim =
  ( Apply prim, CleanMerge prim, Commute prim, Invert prim, Eq2 prim, Show2 prim
  , PatchListFormat prim, ShowPatchBasic prim, ReadPatch prim
  , RepoModel (ModelOf prim), ApplyState prim ~ RepoState (ModelOf prim)
  , ArbitraryPrim prim
  )

{- | A witness type that makes the result witness of merging explicit:

>  wA -----> wY = Merged wA wB
>   ^        ^
>   |        |
>   |        |
>   |        |
>  wX -----> wB

It's quite ad hoc. Even if we add @wX@ as a third parameter, as in

> typedMerge :: p wX wA -> p wX wB -> (p wA (Merged wA wX wB), p wB (Merged wA wX wB))

this breaks down as soon as we try to exploit symmetries. For instance, the
symmetry of merge requires that

> Merged wA wX wB ~ Merged wB wX wA

and, given something like

> typedCommute :: p wX wA -> p wA wY -> (p wX wB, p wB wY)

the merge-commute law would require

> wB ~ Merged wY wA wX ~ Merged (Merged wB wX wA) wA wX

etc. In fact, we want equalities corresponding to all 8 symmetries of a
square (the group D4 with 4 rotations and 4 reflections).

The problem here seems to be that we use '*' (aka 'Type') for witnesses.
With a dedicated witness kind we could define our own equality rules and
hope that we can convince the type checker to make use of them. -}

data Merged wA wB

-- | A wrapper around 'merge' for FL that checks each individual merge,
-- and also returns a more strongly typed witness than the usual existential.
typedMerge
  :: CheckedMerge p
  => (FL p :\/: FL p) wA wB
  -> (FL p wA (Merged wA wB), FL p wB (Merged wA wB))
typedMerge (p :\/: q) =
  case mergerFLFL (checkedMerger merge) (p :\/: q) of
    (q' :/\: p') -> (unsafeCoercePEnd q', unsafeCoercePEnd p')

-- |Given a patch type that contains mergeable patches, such as
-- @RepoPatchV1 prim@ or @Named (RepoPatchV1 prim)@, construct the
-- equivalent conflict-free types, e.g. @prim@ / @Named prim@ respectively.
class ( Effect p, Show2 (OnlyPrim p), ArbitraryState (OnlyPrim p)
      , Shrinkable (OnlyPrim p), PropagateShrink (PrimOf p) (OnlyPrim p)
      , ModelOf p ~ ModelOf (OnlyPrim p)
      )
    => PrimBased p where
  type OnlyPrim p :: * -> * -> *
  primEffect :: OnlyPrim p wX wY -> FL (PrimOf p) wX wY
  liftFromPrim :: OnlyPrim p wX wY -> p wX wY

instance (Commute (OnlyPrim p), PrimBased p) => PrimBased (FL p) where
  type OnlyPrim (FL p) = FL (OnlyPrim p)
  primEffect = concatFL . mapFL_FL (primEffect @p)
  liftFromPrim = mapFL_FL liftFromPrim

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
  propagateShrink (prim :> NilMS) = return (Just2 NilMS :> prim :>: NilFL)
  propagateShrink (prim :> SeqMS ps p) = do
    Just2 ps' :> prims' <- propagateShrink (prim :> ps)
    mp' :> prims'' <- propagateShrinks (prims' :> p)
    let result = case mp' of
          Just2 p' -> SeqMS ps' p'
          Nothing2 -> ps'
    return (Just2 result :> prims'')
{-
  As usual, a picture makes it crystal clear what's going on.

  Inputs: prim, ParMS ms1 ms2
  Intermediate results from recursive calls: ms1', prims1', ms2', prims2'
  Outputs: parMS ms1' ms2' :> prims'

     Merged C1 C2 ---prims'---> Merged D1 D2
            / \                  / \
   mergedps2' mergedps1' mergedps2  mergedps1
          /     \              /     \
         /      C2 --prims2'--/-----> D2
        /       /            /       /
      C1 --------prims1'--> D1      /
        \     /              \     /
       ms1'  ms2'           ms1   ms2
          \ /                  \ /
           A -------prim------> B

-}
  propagateShrink
    ((prim :: prim wA wB) :>
       ParMS (ms1 :: MergeableSequence p wB wD1) (ms2 :: MergeableSequence p wB wD2)) = do
    Just2 (ms1' :: MergeableSequence p wA wC1) :> (prims1' :: FL prim wC1 wD1)
      <- propagateShrink (prim :> ms1)
    Just2 (ms2' :: MergeableSequence p wA wC2) :> (_prims2' :: FL prim wC2 wD2)
      <- propagateShrink (prim :> ms2)
    let
      ms' :: MergeableSequence p wA (Merged wC1 wC2)
      ms' = parMS ms1' ms2'
      ps1  :: FL p wB wD1
      ps2  :: FL p wB wD2
      _mergedps1 :: FL p wD2 (Merged wD1 wD2)
      mergedps2 :: FL p wD1 (Merged wD1 wD2)
      ps1' :: FL p wA wC1
      ps2' :: FL p wA wC2
      _mergedps1' :: FL p wC2 (Merged wC1 wC2)
      mergedps2' :: FL p wC1 (Merged wC1 wC2)
      ps1  = reverseRL (mergeableSequenceToRL ms1)
      ps2  = reverseRL (mergeableSequenceToRL ms2)
      ps1' = reverseRL (mergeableSequenceToRL ms1')
      ps2' = reverseRL (mergeableSequenceToRL ms2')
      (mergedps2 , _mergedps1 ) = typedMerge (ps1  :\/: ps2 )
      (mergedps2', _mergedps1') = typedMerge (ps1' :\/: ps2')
      prims' :: FL prim (Merged wC1 wC2) (Merged wD1 wD2)
      -- Arbitrarily choose one of two ways of constructing the result;
      -- we could as well use
      -- > prims' = recalcShrink prims2' mergedps1 mergedps1'
      -- In fact, coalescing of effects should make them equal.
      -- Also note that if both prims1' and prims2' are NilFL, then
      -- the result should be NilFL as well, since in that case
      -- mergedps2 and mergedps2' are parallel.
      prims' = recalcShrink prims1' mergedps2 mergedps2'
    return (Just2 ms' :> prims')
    where
      recalcShrink
        :: FL prim wX wY -> FL p wY wD -> FL p wX wC -> FL prim wC wD
      recalcShrink prims ps qs =
        sortCoalesceFL $ invert (effect qs) +>+ prims +>+ effect ps

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
     , ApplyState p ~ RepoState model
     , RepoApply p
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
  , Apply p, ApplyState p ~ RepoState model
  , model ~ ModelOf (OnlyPrim p)
  , model ~ ModelOf p
  , CheckedMerge p
  , PrimBased p
  , RepoApply p
  )
  => ArbitraryState (MergeableSequence p) where
  arbitraryState rm = bSized 3 0.035 9 $ arbitraryMergeableSequence arbitraryState rm
