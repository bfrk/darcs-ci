{-# LANGUAGE UndecidableInstances #-}
module Darcs.Test.Patch.Arbitrary.Generic
  ( ArbitraryPrim(..)
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
  ) where

import Darcs.Prelude

import Data.Constraint (Dict(..))

import Darcs.Test.Patch.Arbitrary.Shrink
import Darcs.Test.Patch.Types.Pair ( Pair(..) )
import Darcs.Test.Patch.WithState
import Darcs.Test.Patch.RepoModel
import Darcs.Test.Patch.V1Model
import Darcs.Patch.Witnesses.Eq
import Darcs.Patch.Witnesses.Ordered
import Darcs.Patch.Apply ( Apply, ApplyState )
import Darcs.Patch.Effect ( Effect(..) )
import Darcs.Patch.Format ( PatchListFormat )
import Darcs.Patch.Merge ( CleanMerge, Merge(..) )
import Darcs.Patch.Invert ( Invert(..) )
import Darcs.Patch.Commute ( Commute(..) )
import Darcs.Patch.FromPrim ( PrimOf )
import Darcs.Patch.Prim ( PrimCoalesce, PrimConstruct )
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

type TestablePrim prim =
  ( Apply prim, CleanMerge prim, Commute prim, Invert prim, Eq2 prim, Show2 prim
  , PatchListFormat prim, ShowPatchBasic prim, ReadPatch prim
  , RepoModel (ModelOf prim), ApplyState prim ~ RepoState (ModelOf prim)
  , ArbitraryPrim prim
  )

-- |Given a patch type that contains mergeable patches, such as
-- @RepoPatchV1 prim@ or @Named (RepoPatchV1 prim)@, construct the
-- equivalent conflict-free types, e.g. @prim@ / @Named prim@ respectively.
class ( Effect p, Show2 (OnlyPrim p), ArbitraryState (OnlyPrim p)
      , Shrinkable (OnlyPrim p), PropagateShrink (PrimOf p) (OnlyPrim p)
      , ModelOf p ~ ModelOf (OnlyPrim p)
      )
    => PrimBased p where
  type OnlyPrim p :: Type -> Type -> Type
  primEffect :: OnlyPrim p wX wY -> FL (PrimOf p) wX wY
  liftFromPrim :: OnlyPrim p wX wY -> p wX wY

instance (Commute (OnlyPrim p), PrimBased p) => PrimBased (FL p) where
  type OnlyPrim (FL p) = FL (OnlyPrim p)
  primEffect = concatFL . mapFL_FL (primEffect @p)
  liftFromPrim = mapFL_FL liftFromPrim
