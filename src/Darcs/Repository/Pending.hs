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
    , tentativelyRemoveFromPW
    , revertPending
    , finalizePending
    , setTentativePending
    ) where

import Darcs.Prelude

import Control.Applicative
import System.Directory ( renameFile )

import Darcs.Patch ( PrimOf, PrimPatch, RepoPatch, commuteFL, readPatch )
import Darcs.Patch.Commute ( Commute(..) )
import Darcs.Patch.Invert ( invertFL )
import Darcs.Patch.Permutations ( partitionFL )
import Darcs.Patch.Prim
    ( PrimCoalesce(tryToShrink)
    , PrimSift(primIsSiftable)
    , coalesce
    )
import Darcs.Patch.Progress ( progressFL )
import Darcs.Patch.Read ( ReadPatch(..), bracketedFL )
import Darcs.Patch.Show ( ShowPatchBasic(..), ShowPatchFor(ForStorage) )
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
    , unsafeStartTransaction
    , withRepoDir
    )
import Darcs.Repository.Paths ( pendingPath, tentativePendingPath )

import Darcs.Util.ByteString ( gzReadFilePS )
import Darcs.Util.Exception ( catchDoesNotExistError, ifDoesNotExistError )
import Darcs.Util.Lock ( writeDocBinFile )
import Darcs.Util.Parser ( Parser )
import Darcs.Util.Printer ( Doc, text, vcat, ($$) )
import Darcs.Util.Workaround ( copyFile )


tentativeSuffix :: String
tentativeSuffix = ".tentative"

-- | Read the contents of pending.
readPending :: RepoPatch p => Repository rt p wU wR
            -> IO (Sealed (FL (PrimOf p) wR))
readPending repo =
  case repoAccessType repo of
    SRO -> readPendingFile "" repo
    SRW -> readPendingFile tentativeSuffix repo

-- |Read the contents of tentative pending.
readTentativePending :: RepoPatch p => Repository 'RW p wU wR
                     -> IO (Sealed (FL (PrimOf p) wR))
readTentativePending = readPendingFile tentativeSuffix

-- |Read the pending file with the given suffix. CWD should be the repository
-- directory. Unsafe!
readPendingFile :: ReadPatch prim => String -> Repository rt p wU wR
                -> IO (Sealed (FL prim wX))
readPendingFile suffix _ =
  ifDoesNotExistError (Sealed NilFL) $ do
    let filepath = pendingPath ++ suffix
    raw <- gzReadFilePS filepath
    case readPatch raw of
      Right p -> return (mapSeal unFLM p)
      Left e -> fail $ unlines ["Corrupt pending patch: " ++ show filepath, e]

-- Wrapper around FL where printed format uses { } except around singletons.
-- Now that the Show behaviour of FL p can be customised (using
-- showFLBehavior (*)), we could instead change the general behaviour of FL Prim;
-- but since the pending code can be kept nicely compartmentalised, it's nicer
-- to do it this way.
-- (*) bf: This function does not exist.
newtype FLM p wX wY = FLM { unFLM :: FL p wX wY }

instance ReadPatch p => ReadPatch (FLM p) where
    readPatch' = mapSeal FLM <$> readMaybeBracketedFL readPatch' '{' '}'

instance ShowPatchBasic p => ShowPatchBasic (FLM p) where
    showPatch f = showMaybeBracketedFL (showPatch f) '{' '}' . unFLM

readMaybeBracketedFL :: (forall wY . Parser (Sealed (p wY))) -> Char -> Char
                     -> Parser (Sealed (FL p wX))
readMaybeBracketedFL parser pre post =
    bracketedFL parser pre post <|> (mapSeal (:>:NilFL) <$> parser)

showMaybeBracketedFL :: (forall wX wY . p wX wY -> Doc) -> Char -> Char
                     -> FL p wA wB -> Doc
showMaybeBracketedFL _ pre post NilFL = text [pre] $$ text [post]
showMaybeBracketedFL printer _ _ (p :>: NilFL) = printer p
showMaybeBracketedFL printer pre post ps = text [pre] $$
                                           vcat (mapFL printer ps) $$
                                           text [post]

-- |Write the contents of tentative pending.
writeTentativePending :: RepoPatch p => Repository 'RW p wU wR
                      -> FL (PrimOf p) wR wP -> IO ()
writeTentativePending _ ps =
    unseal (writePatch name . FLM) (siftForPending ps)
  where
    name = pendingPath ++ tentativeSuffix

writePatch :: ShowPatchBasic p => FilePath -> p wX wY -> IO ()
writePatch f p = writeDocBinFile f $ showPatch ForStorage p <> text "\n"

-- | Remove as much as possible of the given list of prim patches from the
-- pending patch. It is used by record and amend to update pending.
--
-- The "as much as possible" is due to --look-for-* options which cause changes
-- that normally must be explicitly done by the user (such as add, move, and
-- replace) to be inferred from the the diff between pristine and working.
-- Also, before we present prims to the user to select for recording, we
-- coalesce prims from pending and working, which is reason we have to use
-- decoalescing.
tentativelyRemoveFromPW :: forall p wR wO wP wU. RepoPatch p
                        => Repository 'RW p wU wR
                        -> FL (PrimOf p) wO wR -- added repo changes
                        -> FL (PrimOf p) wO wP -- O = old recorded state
                        -> FL (PrimOf p) wP wU -- P = (old) pending state
                        -> IO ()
tentativelyRemoveFromPW r changes pending _working = do
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
revertPending :: RepoPatch p => Repository 'RO p wU wR -> IO ()
revertPending r =
  copyFile pendingPath tentativePendingPath `catchDoesNotExistError`
    (readPending r >>= unseal (writeTentativePending (unsafeStartTransaction r)))

-- | Overwrites the pending patch with a new one, starting at the tentative state.
setTentativePending :: forall p wU wR wP. RepoPatch p
                    => Repository 'RW p wU wR
                    -> FL (PrimOf p) wR wP
                    -> IO ()
setTentativePending repo ps = do
    withRepoDir repo $ writeTentativePending repo ps

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
