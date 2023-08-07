--  Copyright (C) 2002-2005,2007 David Roundy
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2, or (at your option)
--  any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; see the file COPYING.  If not, write to
--  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
--  Boston, MA 02110-1301, USA.

module Darcs.Test.Patch ( testSuite ) where

import Darcs.Prelude

import Test.Framework ( Test, testGroup )

import Darcs.Patch.Witnesses.Show
import Darcs.Patch.FromPrim ( PrimOf )
import qualified Darcs.Patch.Prim.FileUUID as FileUUID ( Prim )
import qualified Darcs.Patch.V1.Prim as V1 ( Prim )
import qualified Darcs.Patch.V2.Prim as V2 ( Prim )
import Darcs.Patch.V1 ( RepoPatchV1 )
import Darcs.Patch.V2.RepoPatch ( RepoPatchV2 )
import Darcs.Patch.V3 ( RepoPatchV3 )
import Darcs.Patch.Commute ( Commute(..) )

import Darcs.Test.Patch.Arbitrary.Generic
import Darcs.Test.Patch.Arbitrary.Named ()
import Darcs.Test.Patch.Arbitrary.PrimFileUUID()
import Darcs.Test.Patch.Arbitrary.RepoPatch
import Darcs.Test.Patch.Arbitrary.RepoPatchV1 ()
import Darcs.Test.Patch.Arbitrary.RepoPatchV2 ()
import Darcs.Test.Patch.Arbitrary.RepoPatchV3 ()
import Darcs.Test.Patch.Arbitrary.PrimV1 ()
import Darcs.Test.Patch.Merge.Checked ( CheckedMerge )
import Darcs.Test.Patch.RepoModel
import Darcs.Test.Patch.WithState ( ShrinkModel )

import qualified Darcs.Test.Patch.Depends
import qualified Darcs.Test.Patch.Info
import qualified Darcs.Test.Patch.Selection

import Darcs.Test.Patch.Properties

import qualified Darcs.Test.Patch.Rebase as Rebase
import qualified Darcs.Test.Patch.Unwind as Unwind

type Prim1 = V1.Prim
type Prim2 = V2.Prim

-- tests (either QuickCheck or Unit) that should be run on any type of patch
general_patchTests
  :: forall p
   . ( ArbitraryRepoPatch p, CheckedMerge p
     , PrimBased p, Commute (OnlyPrim p), ArbitraryPrim (OnlyPrim p)
     , ShrinkModel (PrimOf p)
     , Show1 (ModelOf (PrimOf p)), Show2 p
     )
  => [Test]
general_patchTests =
     [ testGroup "Rebase patches" $ Rebase.testSuite @p
     , testGroup "Unwind" $ Unwind.testSuite @p
     ]

-- | This is the big list of tests that will be run using testrunner.
testSuite :: [Test]
testSuite =
    [ primTests
    , repoPatchV1Tests
    , repoPatchV2Tests
    , repoPatchV3Tests
    , Darcs.Test.Patch.Depends.testSuite
    , Darcs.Test.Patch.Info.testSuite
    , Darcs.Test.Patch.Selection.testSuite
    ]
  where
    primTests = testGroup "Prim patches"
      [ testGroup "V1.Prim wrapper for Prim.V1" $ qc_prim @Prim1
      , testGroup "V2.Prim wrapper for Prim.V1" $ qc_prim @Prim2
      , testGroup "Prim.FileUUID" $ qc_prim @FileUUID.Prim
      , testGroup "NamedPrim over V2.Prim" $ qc_named_prim @Prim2
      , testGroup "NamedPrim over Prim.FileUUID" $ qc_named_prim @FileUUID.Prim
      ]
    repoPatchV1Tests = testGroup "RepoPatchV1"
      [ testGroup "using V1.Prim wrapper for Prim.V1" $
          unit_V1P1 ++ qc_V1P1 ++
          general_patchTests @(RepoPatchV1 Prim1)
      ]
    repoPatchV2Tests = testGroup "RepoPatchV2"
      [ testGroup "using V2.Prim wrapper for Prim.V1" $
          unit_V2P1 ++ qc_V2 (undefined :: Prim2 wX wY) ++
          general_patchTests @(RepoPatchV2 Prim2)
      , testGroup "using Prim.FileUUID" $
          qc_V2 (undefined :: FileUUID.Prim wX wY) ++
          general_patchTests @(RepoPatchV2 FileUUID.Prim)
      ]
    repoPatchV3Tests = testGroup "RepoPatchV3"
      [ testGroup "using V2.Prim wrapper for Prim.V1" $
          qc_V3 (undefined :: Prim2 wX wY) ++
          general_patchTests @(RepoPatchV3 Prim2)
      , testGroup "using Prim.FileUUID" $
          qc_V3 (undefined :: FileUUID.Prim wX wY) ++
          general_patchTests @(RepoPatchV3 FileUUID.Prim)
      ]
