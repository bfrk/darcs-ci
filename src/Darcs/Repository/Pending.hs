-- Copyright (C) 2002-2004,2007-2008 David Roundy
-- Copyright (C) 2005 Juliusz Chroboczek
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

module Darcs.Repository.Pending
    ( readPending
    , readTentativePending
    , writeTentativePending
    , siftForPending
    , tentativelyRemoveFromPending
    , revertPending
    , finalizePending
    , setTentativePending
    ) where

import Darcs.Prelude

import Control.Exception ( throwIO )
import System.Directory ( copyFile, renameFile )
import System.IO.Error ( isDoesNotExistError, tryIOError ) 

import Darcs.Patch ( PrimOf, PrimPatch, RepoPatch, commuteFL )
import Darcs.Patch.Commute ( Commute(..) )
import Darcs.Patch.Format ( FormatPatch(..) )
import Darcs.Patch.Invert ( invertFL )
import Darcs.Patch.Permutations ( partitionFL )
import Darcs.Patch.Prim
    ( PrimCoalesce(tryToShrink)
    , PrimSift(primIsSiftable)
    , coalesce
    )
import Darcs.Patch.Progress ( progressFL )
import Darcs.Patch.Read ( ReadPatch(..), legacyReadPatchFL' )
import Darcs.Patch.Witnesses.Maybe ( Maybe2(..) )
import Darcs.Patch.Witnesses.Ordered
    ( FL(..)
    , RL(..)
    , mapFL
    , (+>+)
    , (:>)(..)
    )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..), mapSeal, unseal )

import Darcs.Repository.InternalTypes
    ( AccessType(..)
    , Repository
    , SAccessType(..)
    , repoAccessType
    )
import Darcs.Repository.Paths ( pendingPath, tentativePendingPath )

import Darcs.Util.ByteString ( gzReadFilePS )
import Darcs.Util.Exception ( catchDoesNotExistError )
import Darcs.Util.Format ( Format, ascii, newline, vcat, ($$) )
import Darcs.Util.Lock ( writeFormatBinFile )
import Darcs.Util.Parser ( Parser, parseAll )


-- | Read the contents of  pending (either tentative or regular, depending on
-- the repo's transaction parameter).
readPending
  :: forall rt p wR wU. RepoPatch p => Repository rt p wU wR -> IO (Sealed (FL (PrimOf p) wR))
readPending repo = do
  let filepath :: FilePath =
        case repoAccessType repo of
          SRO -> pendingPath
          SRW -> tentativePendingPath
  -- note: there are (very) old darcs versions that compress pending,
  -- see tests/oldfashioned.sh
  tryIOError (gzReadFilePS filepath) >>= \case
    Left e
      | isDoesNotExistError e -> return (Sealed NilFL)
      | otherwise -> throwIO e
    Right raw ->
      case parseAll (readPendingPatch @(PrimOf p)) raw of
        Right p -> return p
        Left e -> fail $ unlines ["Corrupt pending patch: " ++ show filepath, e]

-- | Read the contents of tentative pending.
readTentativePending
  :: RepoPatch p => Repository 'RW p wU wR -> IO (Sealed (FL (PrimOf p) wR))
readTentativePending = readPending

readPendingPatch :: ReadPatch prim => Parser (Sealed (FL prim wR))
readPendingPatch = legacyReadPatchFL'

-- |Write the contents of tentative pending.
writeTentativePending :: RepoPatch p => Repository 'RW p wU wR
                      -> FL (PrimOf p) wR wP -> IO ()
writeTentativePending _ ps =
    unseal (writePatch tentativePendingPath) (siftForPending ps)

writePatch :: FormatPatch p => FilePath -> FL p wX wY -> IO ()
writePatch f ps = writeFormatBinFile f $ formatPendingPatch ps <> newline

formatPendingPatch :: FormatPatch p => FL p wA wB -> Format
formatPendingPatch (p :>: NilFL) = formatPatch p
formatPendingPatch ps = ascii "{" $$ vcat (mapFL formatPatch ps) $$ ascii "}"

-- | Remove as much as possible of the given list of prim patches from the
-- pending patch. Used by record and amend to update pending.
--
-- This is a highly non-trivial operation, since --look-for-* options cause
-- changes that are normally done explicitly by the user (such as add, move,
-- and replace) to be inferred from the the diff between pristine and working.
-- Furthermore, all these changes are coalesced before we present them to the
-- user to select for recording. Finally, the user can record modified hunks
-- due to hunk splitting. We have to infer from the recorded changes and the
-- old pending which parts of pending is "contained" in the recorded changes
-- and which is not. See 'updatePending' for the details of how to do that.
tentativelyRemoveFromPending
  :: RepoPatch p
  => Repository 'RW p wU wR
  -> FL (PrimOf p) wO wR -- ^ added repo changes
  -> FL (PrimOf p) wO wP -- ^ O = old recorded state, P = (old) pending state
  -> IO ()
tentativelyRemoveFromPending r changes pending = do
  let inverted_changes = invertFL (progressFL "Removing from pending:" changes)
  unseal (writeTentativePending r) (updatePendingRL inverted_changes pending)

-- | Iterate 'updatePending' for all recorded changes.
updatePendingRL :: PrimPatch p => RL p wR wO -> FL p wO wP -> Sealed (FL p wR)
updatePendingRL NilRL ys = Sealed ys
updatePendingRL (xs :<: x) ys = unseal (updatePendingRL xs) (updatePending x ys)

{- | Given an (inverted) single recorded change @x@ and the old pending
@ys@, for each prim @y@ in pending either cancel @x@ against @y@, or
coalesce them. If they coalesce, either commute the result past pending, or
continue with the rest of pending. If coalescing fails, commute @x@ forward
and try again with the next prim from pending. Repeat until we reach the end
of pending or @x@ becomes stuck, in which case we keep it there.

The idea of this algorithm is best explained in terms of an analogy with
arithmetic, where coalescing is addition. Let's say we start out with @a@ in
pending and @b@ in working and record the coalesced @a+b@. We now want to
remove (only) the @a@ from pending. To do that we coalesce @-(a+b)+a@ and
the result (if successful) is @-b@. If this can be commuted past pending, we
are done: the part that came from pending (@a@) is removed and the other
part cancels against what remains in working.

However, we should also guard against the possibility that we recorded a
change that was coalesced from more than one prim in pending. For instance,
suppose we recorded @a+b+c@, where @a@ and @b@ are both from pending and @c@
is form working; after coalescing with @a@ we would be left with
@-(a+b+c)+a=-(b+c)@ which would then be stuck against the remaining @b@.
This is why we continue coalescing, giving us @-(b+c)+b=-c@ which we again
try to commute out etc.

Finally, note that a change can legitimately be stuck in pending i.e. it can
neither be coalesced nor commuted further. For instance, if we have a hunk
in pending and some other prim that depends on it, such as a replace, and
the user records (only) a split-off version of the hunk but not the replace.
This will coalesce with the remaining hunk but then be stuck at the replace.
This is how it should be and thus keeping it there is the correct behavior.
-}

updatePending :: PrimCoalesce p => p wR wO -> FL p wO wP -> Sealed (FL p wR)
updatePending _ NilFL = Sealed NilFL
updatePending x (y :>: ys) =
  case coalesce (x :> y) of
    Just Nothing2 -> Sealed ys -- cancelled out
    Just (Just2 y') ->
      case commuteFL (y' :> ys) of
        Just (ys' :> _) -> Sealed ys' -- drop result if we can commute it past
        Nothing -> updatePending y' ys -- continue coalescing with with y'
    Nothing ->
      case commute (x :> y) of
        Just (y' :> x') -> mapSeal (y' :>:) (updatePending x' ys)
        Nothing -> Sealed (x :>: y :>: ys) -- x is stuck, keep it there

-- | Replace the pending patch with the tentative pending
finalizePending :: Repository 'RW p wU wR -> IO ()
finalizePending _ = renameFile tentativePendingPath pendingPath

-- | Copy the pending patch to the tentative pending, or write a new empty
-- tentative pending if regular pending does not exist.
revertPending :: forall p wU wR. RepoPatch p => Repository 'RO p wU wR -> IO ()
revertPending _ =
  copyFile pendingPath tentativePendingPath `catchDoesNotExistError`
    writePatch tentativePendingPath emptyPending
  where
    emptyPending = NilFL :: FL (PrimOf p) wR wR

-- | Overwrites the pending patch with a new one, starting at the tentative state.
setTentativePending
  :: RepoPatch p => Repository 'RW p wU wR -> FL (PrimOf p) wR wP -> IO ()
setTentativePending repo ps = writeTentativePending repo ps

-- | Simplify the candidate pending patch through a combination of looking
-- for self-cancellations (sequences of patches followed by their inverses),
-- coalescing, and getting rid of any hunk or binary patches we can commute
-- out the back.
--
-- More abstractly, for an argument @p@, pristine state @R@, and working
-- state @U@, define
--
-- > unrecorded p = p +>+ diff (pureApply p R) U
--
-- Then the resulting sequence @p'@ must maintain that equality, i.e.
--
-- > unrecorded p = unrecorded (siftForPending p)
--
-- while trying to "minimize" @p@.
siftForPending
  :: (PrimCoalesce prim, PrimSift prim) => FL prim wX wY -> Sealed (FL prim wX)
siftForPending ps =
  -- Alternately 'sift' and 'tryToShrink' until shrinking no longer reduces
  -- the length of the sequence. Here, 'sift' means to commute siftable
  -- patches to the end of the sequence and then drop them.
  case sift ps of
    Sealed sifted ->
      case tryToShrink sifted of
        Nothing -> Sealed sifted
        Just shrunk -> siftForPending shrunk
  where
    sift xs =
      case partitionFL (not . primIsSiftable) xs of
        (not_siftable :> deps :> _) -> Sealed (not_siftable +>+ deps)
