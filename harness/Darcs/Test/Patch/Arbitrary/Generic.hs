{-# LANGUAGE UndecidableInstances #-}
module Darcs.Test.Patch.Arbitrary.Generic
  ( ArbitraryPrim(..)
  , PrimBased(..)
  , NullPatch(..)
  , RepoModel(..)
  , MightHaveDuplicate(..)
  , notDuplicatestriple
  ) where

import Darcs.Prelude

import Data.Constraint (Dict(..))

import Darcs.Patch.Commute ( Commute(..) )
import Darcs.Patch.Effect ( Effect(..) )
import Darcs.Patch.FromPrim ( PrimOf )
import Darcs.Patch.Prim ( PrimCoalesce, PrimConstruct )
import Darcs.Patch.Witnesses.Eq
import Darcs.Patch.Witnesses.Ordered
import Darcs.Patch.Witnesses.Show
import Darcs.Test.Patch.Arbitrary.Shrink
import Darcs.Test.Patch.RepoModel
import Darcs.Test.Patch.V1Model
import Darcs.Test.Patch.WithState

class NullPatch p where
  nullPatch :: p wX wY -> EqCheck wX wY

class MightHaveDuplicate p where
  -- |"duplicates" in V2 patches (RepoPatchV2) have lots of bugs
  -- that break various commute/merge properties.
  hasDuplicate :: p wX wY -> Bool
  hasDuplicate _ = False

instance MightHaveDuplicate p => MightHaveDuplicate (FL p) where
  hasDuplicate NilFL = False
  hasDuplicate (p :>: ps) = hasDuplicate p || hasDuplicate ps

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
