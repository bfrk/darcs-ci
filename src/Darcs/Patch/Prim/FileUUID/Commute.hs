{-# OPTIONS_GHC -Wno-orphans #-}
module Darcs.Patch.Prim.FileUUID.Commute () where

import Darcs.Prelude

import qualified Data.ByteString as B (length)

import Darcs.Patch.Witnesses.Ordered ( (:>)(..) )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP )
import Darcs.Patch.Prim.FileUUID.Core ( Prim(..), Hunk(..) )
import Darcs.Patch.Commute ( Commute(..) )
import Darcs.Patch.Merge ( CleanMerge(..) )
import Darcs.Patch.Permutations () -- for Invert instance of FL
import Darcs.Patch.Prim.Class ( primCleanMerge )

-- For FileUUID it is easier to list the cases that do /not/ commute
depends :: (Prim :> Prim) wX wY -> Bool
depends (Manifest _i1 l1 :> Demanifest _i2 l2)
  -- after the first patch, i1 is in location l1, so the second patch can
  -- only be valid if i1 == i2 => l1 == l2; so this case is redundant:
  -- | i1 == i2 = True
  -- cannot commute add with remove of any two things at the same location;
  -- note that this implies i1 == i2
  = l1 == l2
depends (Demanifest i1 l1 :> Manifest i2 l2)
  -- this is effectively a move/rename and does not commute
  | i1 == i2 = True
  -- cannot commute remove with add of any two things at the same location
  -- (the add depends on the location being empty)
  | l1 == l2 = True
depends (_ :> _) = False

{-
-- alternative view:
conflicts :: (Prim :\/: Prim) wX wY -> Bool
conflicts (Manifest i1 l1 :\/: Manifest i2 l2)
  -- cannot add the same object in two different or equal locations
  -- (the former is invalid, the latter a duplicate); also cannot
  -- add different objects in the same location
  = i1 == i2 || l1 == l2
conflicts (Demanifest i1 l1 :> Demanifest i2 l2)
  -- the same object cannot be in two different locations, so we have
  -- i1 == i2 => l1 == l2 (for prims valid in the same context).
  -- | i1 == i1
  -- this case implies i1 == i2 and thus is a duplicate
  = l1 == l2
-}

instance Commute Prim where
  commute pair
    | depends pair = Nothing
  commute (Hunk f1 h1 :> Hunk f2 h2)
    | f1 == f2 =
        case commuteHunk (h1 :> h2) of
          Just (h2' :> h1') -> Just (Hunk f2 h2' :> Hunk f1 h1')
          Nothing -> Nothing
  commute (a :> b) =
    Just (unsafeCoerceP b :> unsafeCoerceP a)

commuteHunk :: (Hunk :> Hunk) wX wY -> Maybe ((Hunk :> Hunk) wX wY)
commuteHunk (H off1 old1 new1 :> H off2 old2 new2)
  | off1 + len_new1 < off2  = yes (off2 - len_new1 + len_old1, off1)
  | off2 + len_old2 < off1  = yes (off2, off1 + len_new2 - len_old2)
  | len_old2 /= 0
  , len_old1 /= 0
  , len_new2 /= 0
  , len_new1 /= 0
  , off1 + len_new1 == off2 = yes (off2 - len_new1 + len_old1, off1)
  | len_old2 /= 0
  , len_old1 /= 0
  , len_new2 /= 0
  , len_new1 /= 0
  , off2 + len_old2 == off1 = yes (off2, off1 + len_new2 - len_old2)
  | otherwise               = no
  where
    len_old1 = B.length old1
    len_new1 = B.length new1
    len_old2 = B.length old2
    len_new2 = B.length new2
    yes (off2', off1') = Just (H off2' old2 new2 :> H off1' old1 new1)
    no = Nothing

instance CleanMerge Prim where
  cleanMerge = primCleanMerge
