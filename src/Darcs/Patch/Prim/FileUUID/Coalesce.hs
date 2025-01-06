{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE MultiWayIf #-}
module Darcs.Patch.Prim.FileUUID.Coalesce () where

import Darcs.Prelude

import qualified Data.ByteString as B

import Darcs.Patch.Prim.Class ( PrimCoalesce(..), PrimSift(..) )
import Darcs.Patch.Prim.Coalesce ( sortCoalesceFL2, withAnyToMaybe )
import Darcs.Patch.Prim.FileUUID.Commute ()
import Darcs.Patch.Prim.FileUUID.Core
import Darcs.Patch.Witnesses.Eq ( EqCheck(..) )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP )

instance PrimCoalesce Prim where
  tryToShrink = withAnyToMaybe . sortCoalesceFL2
  sortCoalesceFL = snd . sortCoalesceFL2
  primCoalesce = coalescePair
  isIdentity Identity = IsEq
  isIdentity (Hunk _ (H _ old new))
    | old == new = unsafeCoerceP IsEq
  isIdentity _ = NotEq
  comparePrim p1 p2 = compare p1 (unsafeCoerceP p2)

instance PrimSift Prim where
  primIsSiftable Hunk{} = True
  primIsSiftable _ = False

coalescePair :: Prim wX wY -> Prim wY wZ -> Maybe (Prim wX wZ)
coalescePair Identity p = Just p
coalescePair p Identity = Just p
coalescePair (Hunk i1 (H l1 o1 n1)) (Hunk i2 (H l2 o2 n2))
  | i1 == i2 = Hunk i1 <$> coalesceHunk l1 o1 n1 l2 o2 n2
coalescePair _ _ = Nothing

-- a 1:1 copy of that for V1, only with different types
coalesceHunk
  :: Int -> FileContent -> FileContent
  -> Int -> FileContent -> FileContent
  -> Maybe (Hunk wX wY)
coalesceHunk offset1 old1 new1 offset2 old2 new2
  | offset2 == offset1 =
    Just $
    case compare lengthold2 lengthnew1 of
      LT -> H offset2 old1 (new2 <> B.drop lengthold2 new1)
      GT -> H offset2 (old1 <> B.drop lengthnew1 old2) new2
      EQ -> H offset2 old1 new2
  | offset2 < offset1 && lengthold2 >= offset1 - offset2 =
    case B.take (offset1 - offset2) old2 of
      extra ->
        coalesceHunk offset2 (extra <> old1) (extra <> new1) offset2 old2 new2
  | offset2 > offset1 && lengthnew1 >= offset2 - offset1 =
    case B.take (offset2 - offset1) new1 of
      extra ->
        coalesceHunk offset1 old1 new1 offset1 (extra <> old2) (extra <> new2)
  | otherwise = Nothing
  where
    lengthold2 = B.length old2
    lengthnew1 = B.length new1
