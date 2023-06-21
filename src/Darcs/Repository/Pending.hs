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
    , tentativelyRemoveFromPW
    , revertPending
    , finalizePending
    , tentativelyAddToPending
    , setTentativePending
    ) where

import Darcs.Prelude

import Control.Applicative
import System.Directory ( copyFile, renameFile )

import Darcs.Patch
    ( PrimOf
    , RepoPatch
    , PrimPatch
    , readPatch
    )
import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch.Commute ( Commute(..) )
import Darcs.Patch.Invert ( Invert(..) )
import Darcs.Patch.Permutations
    ( removeFL
    , commuteWhatWeCanFL
    , commuteWhatWeCanRL
    )
import Darcs.Patch.Prim
    ( PrimSift(siftForPending)
    , PrimCanonize(primDecoalesce)
    )
import Darcs.Patch.Progress (progressFL)
import Darcs.Util.Parser ( Parser )
import Darcs.Patch.Read ( ReadPatch(..), bracketedFL )
import Darcs.Patch.Show ( ShowPatchBasic(..), ShowPatchFor(ForStorage) )
import Darcs.Patch.Show ( displayPatch )
import Darcs.Patch.Witnesses.Eq ( Eq2(..) )
import Darcs.Patch.Witnesses.Ordered
    ( RL(..), FL(..), (+>+), (+>>+), (:>)(..), mapFL, reverseFL )
import Darcs.Patch.Witnesses.Sealed ( Sealed(Sealed), mapSeal, unseal )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoercePStart )

import Darcs.Repository.Flags ( UpdatePending(..) )
import Darcs.Repository.InternalTypes
    ( AccessType(..)
    , SAccessType(..)
    , Repository
    , unsafeCoerceR
    , repoAccessType
    , withRepoDir
    , unsafeStartTransaction
    )
import Darcs.Repository.Paths ( pendingPath, tentativePendingPath )

import Darcs.Util.ByteString ( gzReadFilePS )
import Darcs.Util.Exception ( ifDoesNotExistError, catchDoesNotExistError )
import Darcs.Util.Lock  ( writeDocBinFile )
import Darcs.Util.Printer ( Doc, ($$), text, vcat, renderString )
import Darcs.Util.Tree ( Tree )


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
-- pending patch. The "as much as possible" is due to --look-for-* options
-- which cause changes that normally must be explicitly done by the user (such
-- as add, move, and replace) to be inferred from the the diff between
-- pristine and working. These changes cannot be removed from pending because
-- they have never been part of it.
--
-- This function is used by Darcs whenever it adds a patch to the repository
-- (eg. with apply or record). Think of it as one part of transferring patches
-- from pending to somewhere else.
tentativelyRemoveFromPending :: forall p wU wR wO. RepoPatch p
                             => Repository 'RW p wU wR
                             -> FL (PrimOf p) wO wR
                             -> IO ()
tentativelyRemoveFromPending r ps = do
    Sealed pend <- readTentativePending (unsafeCoerceR r :: Repository 'RW p wU wO)
    Sealed newpend <-
        return $ updatePending (progressFL "Removing from pending:" ps) pend NilFL
    writeTentativePending r newpend

-- | Similar to 'tentativelyRemoveFromPending', but also takes the (old)
-- difference between pending and working into account. It is used by amend and
-- record commands to adjust the pending patch. See the docs for
-- 'updatePending' below for details.
tentativelyRemoveFromPW :: forall p wR wO wP wU. RepoPatch p
                        => Repository 'RW p wU wR
                        -> FL (PrimOf p) wO wR -- added repo changes
                        -> FL (PrimOf p) wO wP -- O = old recorded state
                        -> FL (PrimOf p) wP wU -- P = (old) pending state
                        -> IO ()
tentativelyRemoveFromPW r changes pending working = do
    Sealed pending' <- return $
        updatePending (progressFL "Removing from pending:" changes) pending working
    writeTentativePending r pending'

{- |
@'updatePending' changes pending working@ updates @pending@ by removing the
@changes@ we added to the repository. If primitive patches were atomic, we
could assume that @changes@ is a subset of @pending +>+ working@, but alas,
they are not: before we select changes we coalesce them; and during
selection we can again arbitrarily split them (though currently this is
limited to hunks).

The algorithm is as follows. For each @x@ in @changes@ we first try to
remove it from @pending@ as is. If this fails, we commute it past @pending@,
pushing any (reverse) dependencies with it, and check if we can remove the
result from @working@.

If prim patches were atomic this check would always succeed and we would be
done now. But due to coalescing and splitting of prims it can fail, so we
must try harder: we now try to decoalesce the commuted changes from
@working@. If that fails, too, then we know that our @x@ originated from
@pending@. So we backtrack and decoalesce @x@ from @pending@. This final
step must not fail. If it does, then we have a bug because it means we
recorded a change that cannot be removed from the net effect of @pending@
and @working@.
-}
updatePending :: (PrimPatch p)
              => FL p wA wB -> FL p wA wC -> FL p wC wD -> Sealed (FL p wB)
-- no changes to the repo => cancel patches in pending whose inverse are in working
updatePending NilFL ys zs = removeRLFL (reverseFL ys) zs
-- pending is empty => keep it that way
updatePending _ NilFL _ = Sealed NilFL
-- no working changes =>
--  just prepend inverted repo changes and rely on sifting to clean up pending
updatePending xs ys NilFL = Sealed (invert xs +>+ ys)
-- x can be removed from pending => continue with the rest
updatePending (x:>:xs) ys zs | Just ys' <- removeFL x ys = updatePending xs ys' zs
-- x and its reverse dependencies can be commuted through pending
-- *and* the result can be removed or decoalesced from working
updatePending (x:>:xs) ys zs
  | ys' :> ix' :> deps <- commuteWhatWeCanFL (invert x :> ys)
  , Just zs' <- removeFromWorking (invert (ix':>:deps)) zs = updatePending xs ys' zs'
  where
    removeFromWorking as bs = removeAllFL as bs <|> decoalesceAllFL bs as
-- decoalesce x from ys and continue with the rest
updatePending (x:>:xs) ys zs =
  case decoalesceFL ys x of
    Just ys' -> updatePending xs ys' zs
    Nothing ->
      error $ renderString
        $ text "cannot eliminate repo change:"
        $$ displayPatch x
        $$ text "from pending:"
        $$ vcat (mapFL displayPatch ys)
        $$ text "or working:"
        $$ vcat (mapFL displayPatch zs)

-- | Remove as many patches as possible of an 'RL' from an adjacent 'FL'.
removeRLFL :: (Commute p, Invert p, Eq2 p)
           => RL p wA wB -> FL p wB wC -> Sealed (FL p wA)
removeRLFL (ys:<:y) zs
  | Just zs' <- removeFL (invert y) zs = removeRLFL ys zs'
  | otherwise = case commuteWhatWeCanRL (ys :> y) of
      deps :> y' :> ys' -> mapSeal ((deps:<:y') +>>+) $ removeRLFL ys' zs
removeRLFL NilRL _ = Sealed NilFL

-- | Remove all patches of the first 'FL' from the second 'FL' or fail.
removeAllFL :: (Commute p, Invert p, Eq2 p)
            => FL p wA wB -> FL p wA wC -> Maybe (FL p wB wC)
removeAllFL (y:>:ys) zs
  | Just zs' <- removeFL y zs = removeAllFL ys zs'
  | otherwise = Nothing
removeAllFL NilFL zs = Just zs

-- | Decoalesce all patches in the second 'FL' from the first 'FL' or fail.
decoalesceAllFL :: (Commute p, Invert p, PrimCanonize p)
                => FL p wA wC -> FL p wA wB -> Maybe (FL p wB wC)
decoalesceAllFL zs (y:>:ys)
  | Just zs' <- decoalesceFL zs y = decoalesceAllFL zs' ys
  | otherwise = Nothing
decoalesceAllFL zs NilFL = Just zs

-- | Decoalesce (subtract) a single patch from an 'FL' by trying to
-- decoalesce it with every element until it succeeds or we cannot
-- commute it any further.
decoalesceFL :: (Commute p, Invert p, {- Eq2 p,  -}PrimCanonize p)
             => FL p wA wC -> p wA wB -> Maybe (FL p wB wC)
decoalesceFL NilFL y = Just (invert y :>: NilFL)
decoalesceFL (z :>: zs) y
  | Just z' <- primDecoalesce z y = Just (z' :>: zs)
  | otherwise = do
      z' :> iy' <- commute (invert y :> z)
      zs' <- decoalesceFL zs (invert iy')
      return (z' :>: zs')

-- | Replace the pending patch with the tentative pending, unless
-- we get @NoUpdatePending@.
finalizePending :: (RepoPatch p, ApplyState p ~ Tree)
                => Repository 'RW p wU wR
                -> UpdatePending
                -> IO ()
finalizePending _ NoUpdatePending = return ()
finalizePending _ YesUpdatePending =
  renameFile tentativePendingPath pendingPath

revertPending :: RepoPatch p
              => Repository 'RO p wU wR
              -> UpdatePending
              -> IO ()
revertPending _ NoUpdatePending = return ()
revertPending r YesUpdatePending =
  copyFile pendingPath tentativePendingPath `catchDoesNotExistError`
    (readPending r >>= unseal (writeTentativePending (unsafeStartTransaction r)))

-- | @tentativelyAddToPending repo ps@ appends @ps@ to the pending patch.
--
--   This fuction is unsafe because it accepts a patch that works on the
--   tentative pending and we don't currently track the state of the
--   tentative pending.
tentativelyAddToPending :: forall p wU wR wX wY. RepoPatch p
                        => Repository 'RW p wU wR
                        -> FL (PrimOf p) wX wY
                        -> IO ()
tentativelyAddToPending repo patch =
    withRepoDir repo $ do
        Sealed pend <- readTentativePending repo
        writeTentativePending repo (pend +>+ unsafeCoercePStart patch)

-- | Overwrites the pending patch with a new one, starting at the tentative state.
setTentativePending :: forall p wU wR wP. RepoPatch p
                    => Repository 'RW p wU wR
                    -> FL (PrimOf p) wR wP
                    -> IO ()
setTentativePending repo ps = do
    withRepoDir repo $ writeTentativePending repo ps
