module Darcs.Test.Patch.RepoModel where

import Darcs.Prelude

import Control.Exception ( SomeException )

import Darcs.Patch.Apply ( Apply, ApplyState )
import Darcs.Patch.Witnesses.Ordered ( FL, RL )

import Test.QuickCheck ( Gen )

type Fail = Either SomeException

unFail :: Fail t -> t
unFail = either (error.show) id

maybeFail :: Fail a -> Maybe a
maybeFail = either (const Nothing) Just

class RepoModel model where
  type RepoState model :: (* -> *) -> *
  showModel :: model x -> String
  eqModel :: model x -> model x -> Bool
  aSmallRepo :: Gen (model x)
  repoApply :: (Apply p, ApplyState p ~ RepoState model) => model x -> p x y -> Fail (model y)

type family ModelOf (p :: * -> * -> *) :: * -> *

type instance ModelOf (FL p) = ModelOf p
type instance ModelOf (RL p) = ModelOf p
