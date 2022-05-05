-- Copyright (C) 2003 David Roundy
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

module Darcs.Patch.Set
    ( PatchSet(..)
    , Tagged(..)
    , SealedPatchSet
    , Origin
    , progressPatchSet
    , patchSetInventoryHashes
    , patchSetTags
    , emptyPatchSet
    , appendPSFL
    , patchSet2RL
    , patchSet2FL
    , inOrderTags
    , patchSetSnoc
    , patchSetSplit
    , patchSetDrop
    ) where

import Darcs.Prelude
import Data.Maybe ( catMaybes )

import Darcs.Patch.Info ( PatchInfo, piTag )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, info )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..) )
import Darcs.Patch.Witnesses.Ordered
    ( FL, RL(..), (+<+), (+<<+), (:>)(..), reverseRL,
    mapRL_RL, concatRL, mapRL )
import Darcs.Patch.Witnesses.Show ( Show1, Show2 )

import Darcs.Util.Progress ( progress )
import Darcs.Util.ValidHash ( InventoryHash )

-- |'Origin' is a type used to represent the initial context of a repo.
data Origin

type SealedPatchSet p wStart = Sealed ((PatchSet p) wStart)

-- |The patches in a repository are stored in chunks broken up at \"clean\"
-- tags. A tag is clean if the only patches before it in the current
-- repository ordering are ones that the tag depends on (either directly
-- or indirectly). Each chunk is stored in a separate inventory file on disk.
--
-- A 'PatchSet' represents a repo's history as the list of patches since the
-- last clean tag, and then a list of patch lists each delimited by clean tags.
--
-- Because the invariants about clean tags can only be maintained if a
-- 'PatchSet' contains the whole history, the first witness is always forced
-- to be 'Origin'. The type still has two witnesses so it can easily be used
-- with combinators like ':>' and 'Fork'.
--
-- The history is lazily loaded from disk so does not normally need to be all
-- kept in memory.
data PatchSet p wStart wY where
    PatchSet :: RL (Tagged p) Origin wX -> RL (PatchInfoAnd p) wX wY
             -> PatchSet p Origin wY

deriving instance Show2 p => Show (PatchSet p wStart wY)

instance Show2 p => Show1 (PatchSet p wStart)

instance Show2 p => Show2 (PatchSet p)


emptyPatchSet :: PatchSet p Origin Origin
emptyPatchSet = PatchSet NilRL NilRL

-- |A 'Tagged' is a single chunk of a 'PatchSet'.
-- It has a 'PatchInfo' representing a clean tag,
-- the hash of the previous inventory (if it exists),
-- and the list of patches since that previous inventory.
data Tagged p wX wZ where
    Tagged :: RL (PatchInfoAnd p) wX wY -> PatchInfoAnd p wY wZ -> Maybe InventoryHash
           -> Tagged p wX wZ

deriving instance Show2 p => Show (Tagged p wX wZ)

instance Show2 p => Show1 (Tagged p wX)

instance Show2 p => Show2 (Tagged p)


-- |'patchSet2RL' takes a 'PatchSet' and returns an equivalent, linear 'RL' of
-- patches.
patchSet2RL :: PatchSet p wStart wX -> RL (PatchInfoAnd p) wStart wX
patchSet2RL (PatchSet ts ps) = concatRL (mapRL_RL ts2rl ts) +<+ ps
  where
    ts2rl :: Tagged p wY wZ -> RL (PatchInfoAnd p) wY wZ
    ts2rl (Tagged ps2 t _) = ps2 :<: t

-- |'patchSet2FL' takes a 'PatchSet' and returns an equivalent, linear 'FL' of
-- patches.
patchSet2FL :: PatchSet p wStart wX -> FL (PatchInfoAnd p) wStart wX
patchSet2FL = reverseRL . patchSet2RL

-- |'appendPSFL' takes a 'PatchSet' and a 'FL' of patches that "follow" the
-- PatchSet, and concatenates the patches into the PatchSet.
appendPSFL :: PatchSet p wStart wX -> FL (PatchInfoAnd p) wX wY
           -> PatchSet p wStart wY
appendPSFL (PatchSet ts ps) newps = PatchSet ts (ps +<<+ newps)

-- |Runs a progress action for each tag and patch in a given PatchSet, using
-- the passed progress message. Does not alter the PatchSet.
progressPatchSet :: String -> PatchSet p wStart wX -> PatchSet p wStart wX
progressPatchSet k (PatchSet ts ps) =
    PatchSet (mapRL_RL progressTagged ts) (mapRL_RL prog ps)
  where
    prog = progress k

    progressTagged :: Tagged p wY wZ -> Tagged p wY wZ
    progressTagged (Tagged tps t h) = Tagged (mapRL_RL prog tps) (prog t) h

patchSetInventoryHashes :: PatchSet p wX wY -> [Maybe InventoryHash]
patchSetInventoryHashes (PatchSet ts _) = mapRL (\(Tagged _ _ mh) -> mh) ts

-- | The tag names of /all/ tags of a given 'PatchSet'.
patchSetTags :: PatchSet p wX wY -> [String]
patchSetTags = catMaybes . mapRL (piTag . info) . patchSet2RL

inOrderTags :: PatchSet p wS wX -> [PatchInfo]
inOrderTags (PatchSet ts _) = go ts
  where go :: RL(Tagged t1) wT wY -> [PatchInfo]
        go (ts' :<: Tagged _ t _) = info t : go ts'
        go NilRL = []

patchSetSnoc :: PatchSet p wX wY -> PatchInfoAnd p wY wZ -> PatchSet p wX wZ
patchSetSnoc (PatchSet ts ps) p = PatchSet ts (ps :<: p)

-- | Split a 'PatchSet' /before/ the latest known clean tag. The left part
-- is what comes before the tag, the right part is the tag and its
-- non-dependencies.
patchSetSplit :: PatchSet p wX wY
              -> (PatchSet p :> RL (PatchInfoAnd p)) wX wY
patchSetSplit (PatchSet (ts :<: Tagged ps' t _) ps) =
  PatchSet ts ps' :> ((NilRL :<: t) +<+ ps)
patchSetSplit (PatchSet NilRL ps) = PatchSet NilRL NilRL :> ps

-- | Drop the last @n@ patches from the given 'PatchSet'.
patchSetDrop :: Int
             -> PatchSet p wStart wX
             -> SealedPatchSet p wStart
patchSetDrop n ps | n <= 0 = Sealed ps
patchSetDrop n (PatchSet (ts :<: Tagged ps t _) NilRL) =
  patchSetDrop n $ PatchSet ts (ps :<: t)
patchSetDrop _ (PatchSet NilRL NilRL) = Sealed $ PatchSet NilRL NilRL
patchSetDrop n (PatchSet ts (ps :<: _)) = patchSetDrop (n - 1) $ PatchSet ts ps
