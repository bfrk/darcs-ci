{-# OPTIONS_GHC -Wno-orphans #-}
module Darcs.Test.Patch.Arbitrary.Sealed
  ( ArbitraryS2(..)
  ) where

import Darcs.Patch.Witnesses.Sealed
  ( Sealed2(..)
  )

import Test.QuickCheck

class ArbitraryS2 p where
  arbitraryS2 :: Gen (Sealed2 p)
  shrinkS2 :: Sealed2 p -> [Sealed2 p]
  shrinkS2 _ = []

instance ArbitraryS2 p => Arbitrary (Sealed2 p) where
  arbitrary = arbitraryS2
  shrink = shrinkS2
