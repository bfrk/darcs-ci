-- Copyright (C) 2002-2003,2007 David Roundy
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; see the file COPYING.  If not, write to
-- the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
-- Boston, MA 02110-1301, USA.

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module Darcs.Test.Patch.Arbitrary.RepoPatchV1 (Patch) where

import Prelude ()
import Darcs.Prelude

import Control.Exception ( try, evaluate, SomeException )
import System.IO.Unsafe

import Darcs.Patch
import Darcs.Patch.V1 ()
import Darcs.Patch.V1.Core ( RepoPatchV1(..) )
import qualified Darcs.Patch.V1.Prim as V1 ( Prim )
import Darcs.Patch.Witnesses.Ordered
import Darcs.Patch.Witnesses.Sealed ( Sealed(Sealed) )


import Darcs.Test.Patch.Arbitrary.Generic ( MightHaveDuplicate, ArbitraryPrim, PrimBased(..) )
import Darcs.Test.Patch.Arbitrary.Mergeable
import Darcs.Test.Patch.Merge.Checked ( CheckedMerge(..) )
import Darcs.Test.Patch.RepoModel ( RepoState, ModelOf )
import Darcs.Test.Patch.Types.Pair ( Pair(..) )
import Darcs.Test.Patch.WithState
  ( PropagateShrink(..)
  , ArbitraryState(..), WithEndState(..)
  )

type Patch = RepoPatchV1 V1.Prim

instance (ArbitraryPrim prim, ApplyState prim ~ RepoState (ModelOf prim)) =>
         ArbitraryMergeable (RepoPatchV1 prim) where
  notRepoPatchV1 = Nothing

instance PrimPatch prim => CheckedMerge (RepoPatchV1 prim) where
  validateMerge v =
    case unsafePerformIO (try (evaluate v)) of
      Left (_ :: SomeException) -> Nothing
      Right x -> Just x

instance MightHaveDuplicate (RepoPatchV1 prim)

type instance ModelOf (RepoPatchV1 prim) = ModelOf prim

instance (PrimPatch prim, ArbitraryPrim prim, PropagateShrink prim prim) => PrimBased (RepoPatchV1 prim) where
  type OnlyPrim (RepoPatchV1 prim) = prim
  primEffect prim = prim :>: NilFL
  liftFromPrim = PP

-- TODO: this instance only exists because of the history of the V1 QuickCheck tests
-- (qc_V1P1 in D.T.Patch). The QuickCheck tests for V1, V2, V3 etc should be aligned
-- and this instance removed.
instance ArbitraryState prim => ArbitraryState (RepoPatchV1 prim) where
  arbitraryState repo = do
    Sealed (WithEndState prim repo') <- arbitraryState repo
    return (Sealed (WithEndState (PP prim) repo'))
  arbitraryStatePair repo = do
    Sealed (WithEndState (Pair (prim1 :> prim2)) repo') <- arbitraryStatePair repo
    return (Sealed (WithEndState (Pair (PP prim1 :> PP prim2)) repo'))
