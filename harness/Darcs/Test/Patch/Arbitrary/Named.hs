{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module Darcs.Test.Patch.Arbitrary.Named
  ( WithNames(..)
  ) where

import Darcs.Prelude

import Darcs.Test.Patch.Arbitrary.Generic
import Darcs.Test.Patch.Arbitrary.Shrink
import Darcs.Test.Patch.RepoModel
import Darcs.Test.Patch.WithState
import Darcs.Test.TestOnly.Instance ()

import Darcs.Patch.Apply
import Darcs.Patch.Commute
import Darcs.Patch.Info ( PatchInfo, rawPatchInfo )
import Darcs.Patch.Named
import Darcs.Patch.Witnesses.Maybe
import Darcs.Patch.Witnesses.Ordered
import Darcs.Patch.Witnesses.Sealed
import Darcs.Patch.Witnesses.Show

import Data.List ( sort )
import Test.QuickCheck

data WithNames m wX = WithNames
  { primModel :: m wX
  , appliedPatchNames :: [PatchInfo]
  }

instance Show1 m => Show1 (WithNames m)

instance Show1 m => Show (WithNames m wX) where
  showsPrec d (WithNames m ns) =
    showParen (d > appPrec)
      $ showString "WithNames "
      . showsPrec1 (appPrec + 1) m
      . showString " "
      . showsPrec (appPrec + 1) ns

instance RepoModel m => RepoModel (WithNames m) where
  type RepoState (WithNames m) = RepoState m
  aSmallRepo = WithNames <$> aSmallRepo <*> pure []
  repoApply m p =
    WithNames <$> repoApply (primModel m) p <*> pure (appliedPatchNames m ++ patchNames p)
  eqModel m1 m2 =
    eqModel (primModel m1) (primModel m2)
    && sort (appliedPatchNames m1) == sort (appliedPatchNames m2)
  showModel = show

type instance ModelOf (Named p) = WithNames (ModelOf p)

instance (ArbitraryState p, RepoModel (ModelOf p)) => ArbitraryState (Named p) where
  arbitraryState rm = do
    info <- arbitrarySimplePatchInfo
    deps <- sublistOf (appliedPatchNames rm)
    Sealed (WithEndState prims rm') <- arbitraryState (primModel rm)
    return $ Sealed $
      WithEndState (NamedP info deps prims) (WithNames rm' (info : appliedPatchNames rm))
    where
      -- generate only minimal, human readable (and always valid) 'PatchInfo's
      -- with no need for shrinking
      arbitrarySimplePatchInfo = do
        -- this is hopefully random enough to avoid collisions in practice
        name <- vectorOf 20 $ elements ['a'..'z']
        let date = "20240606010532"
            author = "tester"
            log = []
            inverted = False
        return $ rawPatchInfo date name author log inverted

instance (Commute p, Shrinkable p) => Shrinkable (Named p) where
  shrinkInternally (NamedP pi deps ps) =
    NamedP pi deps <$> shrinkInternally ps

  shrinkAtStart (NamedP pi deps ps) = mapFlipped (NamedP pi deps) <$> shrinkAtStart ps
  shrinkAtEnd (NamedP pi deps ps) = mapSeal (NamedP pi deps) <$> shrinkAtEnd ps

instance ShrinkModel model p => ShrinkModel (WithNames model) p where
  shrinkModelPatch (WithNames m _) = shrinkModelPatch m

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
