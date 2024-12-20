{-# LANGUAGE ViewPatterns #-}
module Darcs.Patch.Prim.Canonize ( canonizeFL ) where

import Darcs.Prelude

import qualified Data.ByteString as B ( empty )

import Darcs.Patch.FileHunk ( FileHunk(..), IsHunk(..) )
import Darcs.Patch.Prim.Class ( PrimCoalesce(sortCoalesceFL) )
import Darcs.Patch.Witnesses.Ordered ( FL(..), mapFL_FL, concatFL )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoercePEnd )
import Darcs.Util.Diff ( DiffAlgorithm, getChanges )

canonizeHunk :: DiffAlgorithm -> FileHunk xd oid wX wY -> FL (FileHunk xd oid) wX wY
canonizeHunk da h@(FileHunk xd f line old new)
  | null old || null new || old == [B.empty] || new == [B.empty] = h :>: NilFL
  | otherwise =
      buildFL (\(l, o, n) -> FileHunk xd f (l + line) o n) $ getChanges da old new

buildFL
  :: (forall wA wB . a -> FileHunk xd oid wA wB) -> [a] -> FL (FileHunk xd oid) wX wY
buildFL _ [] = unsafeCoercePEnd NilFL
buildFL f (x:xs) = f x :>: buildFL f xs

-- | It can sometimes be handy to have a canonical representation of a given
-- patch.  We achieve this by defining a canonical form for each patch type,
-- and a function 'canonize' which takes a patch and puts it into
-- canonical form.  This routine is used by the diff function to create an
-- optimal patch (based on an LCS algorithm) from a simple hunk describing the
-- old and new version of a file.
canonize :: IsHunk prim
         => DiffAlgorithm -> prim wX wY -> FL prim wX wY
canonize da p | Just fh <- isHunk p =
  mapFL_FL fromHunk $ canonizeHunk da fh
canonize _ p = p :>: NilFL

-- | Put a sequence of primitive patches into canonical form.
--
-- Even if the patches are just hunk patches,
-- this is not necessarily the same set of results as you would get
-- if you applied the sequence to a specific tree and recalculated
-- a diff.
--
-- XXX Why not? How does it differ? The implementation for Prim.V1 does
-- sortCoalesceFL and then invokes the diff algorithm for each hunk. How can
-- that be any different to applying the sequence and then taking the diff?
-- Is this merely because diff does not sort by file path?
canonizeFL :: (IsHunk prim, PrimCoalesce prim)
           => DiffAlgorithm -> FL prim wX wY -> FL prim wX wY
-- Note: it is important to first coalesce and then canonize, since
-- coalescing can produce non-canonical hunks (while hunks resulting
-- from canonizing a single hunk cannot be coalesced). See issue525,
-- in particular msg20270 for details.
canonizeFL da = concatFL . mapFL_FL (canonize da) . sortCoalesceFL
