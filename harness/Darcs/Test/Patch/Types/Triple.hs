{-# LANGUAGE UndecidableInstances #-}
module Darcs.Test.Patch.Types.Triple
  ( Triple(..)
  ) where

import Darcs.Prelude

import Darcs.Patch.Witnesses.Ordered
import Darcs.Patch.Witnesses.Sealed
import Darcs.Patch.Witnesses.Show
import Darcs.Test.Patch.RepoModel
import Darcs.Test.Patch.WithState
    ( ArbitraryState(..)
    , WithEndState(..)
    , ArbitraryWS(..)
    , makeWS2Gen
    )

newtype Triple p wX wY = Triple { getTriple :: (p :> p :> p) wX wY }
  deriving Show

instance Show2 p => Show2 (Triple p)

type instance ModelOf (Triple p) = ModelOf p

instance ArbitraryState p => ArbitraryState (Triple p) where
  arbitraryState sIn = do
    Sealed (WithEndState p sOut) <- arbitraryState sIn
    return (Sealed (WithEndState (Triple p) sOut))

instance (RepoModel (ModelOf p), ArbitraryState p) => ArbitraryWS (Triple p) where
  arbitraryWS = makeWS2Gen aSmallRepo
