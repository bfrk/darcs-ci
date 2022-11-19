{-# LANGUAGE ViewPatterns #-}
module Darcs.Patch.Prim.Canonize ( canonizeFL ) where

import Darcs.Prelude

import qualified Data.ByteString as B (ByteString, empty)

import Darcs.Patch.FileHunk ( FileHunk(..), IsHunk(..) )
import Darcs.Patch.Prim.Class
    ( PrimConstruct(primFromHunk)
    , PrimCoalesce(sortCoalesceFL)
    )
import Darcs.Patch.Witnesses.Ordered ( FL(..), mapFL_FL, concatFL )
import Darcs.Patch.Witnesses.Sealed ( unseal, Gap(..), unFreeLeft )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoercePEnd )
import Darcs.Util.Diff ( DiffAlgorithm, getChanges )
import Darcs.Util.Path ( AnchoredPath )

canonizeHunk :: Gap w => DiffAlgorithm -> FileHunk wX wY -> w (FL FileHunk)
canonizeHunk _ (FileHunk f line old new)
  | null old || null new || old == [B.empty] || new == [B.empty] =
      freeGap (FileHunk f line old new :>: NilFL)
canonizeHunk da (FileHunk f line old new) =
  makeHoley f line $ getChanges da old new

makeHoley :: Gap w
          => AnchoredPath
          -> Int
          -> [(Int, [B.ByteString], [B.ByteString])]
          -> w (FL FileHunk)
makeHoley f line =
  foldr
    (joinGap (:>:) . (\(l, o, n) -> freeGap (FileHunk f (l + line) o n)))
    (emptyGap NilFL)

-- | It can sometimes be handy to have a canonical representation of a given
-- patch.  We achieve this by defining a canonical form for each patch type,
-- and a function 'canonize' which takes a patch and puts it into
-- canonical form.  This routine is used by the diff function to create an
-- optimal patch (based on an LCS algorithm) from a simple hunk describing the
-- old and new version of a file.
canonize :: (IsHunk prim, PrimConstruct prim)
         => DiffAlgorithm -> prim wX wY -> FL prim wX wY
canonize da p | Just fh <- isHunk p =
  mapFL_FL primFromHunk $ unseal unsafeCoercePEnd $ unFreeLeft $ canonizeHunk da fh
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
--
-- Besides, diff and apply /must/ be inverses in the sense that for any two
-- states {start, end}, we have
--
-- prop> diff start (apply (diff start end)) == end
canonizeFL :: (IsHunk prim, PrimCoalesce prim, PrimConstruct prim)
           => DiffAlgorithm -> FL prim wX wY -> FL prim wX wY
-- Note: it is important to first coalesce and then canonize, since
-- coalescing can produce non-canonical hunks (while hunks resulting
-- from canonizing a single hunk cannot be coalesced). See issue525,
-- in particular msg20270 for details.
canonizeFL da = concatFL . mapFL_FL (canonize da) . sortCoalesceFL
