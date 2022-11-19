{-# LANGUAGE UndecidableInstances #-}
module Darcs.Test.Patch.Arbitrary.Named
  (
  ) where

import Darcs.Prelude

import Darcs.Test.Patch.Info ()
import Darcs.Test.Patch.Arbitrary.Generic
import Darcs.Test.Patch.Arbitrary.Shrink
import Darcs.Test.Patch.RepoModel
import Darcs.Test.Patch.WithState

import Darcs.Patch.Apply
import Darcs.Patch.Commute
import Darcs.Patch.Named
import Darcs.Patch.Witnesses.Maybe
import Darcs.Patch.Witnesses.Ordered
import Darcs.Patch.Witnesses.Sealed

import Control.Applicative ( (<|>) )
import Test.QuickCheck

type instance ModelOf (Named p) = ModelOf p

instance (ArbitraryState p, RepoModel (ModelOf p)) => ArbitraryState (Named p) where
  arbitraryState rm = do
    info <- arbitrary
    deps <- sublistOf (appliedPatchNames rm)
    Sealed (WithEndState prims rm') <- arbitraryState rm
    return $ Sealed $ WithEndState (NamedP info deps prims) rm'

instance (Commute p, Shrinkable p) => Shrinkable (Named p) where
  shrinkInternally (NamedP pi deps ps) =
    -- TODO this isn't quite right because other patches might
    -- explicitly depend on this one
    (\pi' -> NamedP pi' deps ps) <$> shrink pi
    <|>
    NamedP pi deps <$> shrinkInternally ps

  shrinkAtStart (NamedP pi deps ps) = mapFlipped (NamedP pi deps) <$> shrinkAtStart ps
  shrinkAtEnd (NamedP pi deps ps) = mapSeal (NamedP pi deps) <$> shrinkAtEnd ps

instance PropagateShrink prim p => PropagateShrink prim (Named p) where
  propagateShrink (prim :> NamedP pi deps ps) = do
    mps' :> mprim' <- propagateShrink (prim :> ps)
    return (mapMB_MB (NamedP pi deps) mps' :> mprim')

instance (Commute (OnlyPrim p), PrimBased p, RepoModel (ModelOf (OnlyPrim p))) => PrimBased (Named p) where
  type OnlyPrim (Named p) = Named (OnlyPrim p)

  primEffect (NamedP _ _ ps) = primEffect @(FL p) ps
  liftFromPrim (NamedP pi deps ps) = NamedP pi deps (liftFromPrim ps)

instance Apply p => RepoApply (Named p) where
  patchNames p = [patch2patchinfo p]
