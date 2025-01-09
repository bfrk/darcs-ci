module Darcs.Test.Patch.RepoModel where

import Darcs.Prelude

import Control.Exception ( SomeException )

import Darcs.Patch.Apply ( Apply, ApplyState )
import Darcs.Patch.Info ( PatchInfo )
import Darcs.Patch.Witnesses.Ordered ( FL, RL, mapFL, mapRL )
import Darcs.Patch.Witnesses.Show ( Show1 )

import Test.QuickCheck ( Gen )

type Fail = Either SomeException

unFail :: Fail t -> t
unFail = either (error.show) id

maybeFail :: Fail a -> Maybe a
maybeFail = either (const Nothing) Just

-- | Class of patch types that can be applied to a model
class Apply p => RepoApply p where
  -- | This method exists so that we can keep track of the names of patches
  -- that have been applied to the model. This allows us to generate 'Named'
  -- patches with meaningful explicit dependencies.
  patchNames :: p wX wY -> [PatchInfo]
  patchNames _ = []

instance RepoApply p => RepoApply (FL p) where
  patchNames = concat . mapFL patchNames

instance RepoApply p => RepoApply (RL p) where
  patchNames = concat . mapRL patchNames

class Show1 model => RepoModel model where
  type RepoState model :: (Type -> Type) -> Type
  showModel :: model x -> String
  eqModel :: model x -> model x -> Bool
  aSmallRepo :: Gen (model x)
  repoApply :: (RepoApply p, ApplyState p ~ RepoState model) => model x -> p x y -> Fail (model y)

type family ModelOf (p :: Type -> Type -> Type) :: Type -> Type

type instance ModelOf (FL p) = ModelOf p
type instance ModelOf (RL p) = ModelOf p
