--  Copyright (C) 2002-2003 David Roundy
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

-- | 'Named' patches group a set of changes with meta data ('PatchInfo') and
-- explicit dependencies (created using `darcs tag` or using --ask-deps).
--
-- While the data constructor 'NamedP' is exported for technical reasons, code
-- outside this modules should (and generally does) treat it as an abstract
-- data type. The only exception is the rebase implementation i.e. the modules
-- under "Darcs.Patch.Rebase".

{-# LANGUAGE UndecidableInstances #-}
module Darcs.Patch.Named
    ( Named(..)
    -- treated as abstract data type except by Darcs.Patch.Rebase
    , infopatch
    , adddeps
    , setinfo
    , anonymous
    , HasDeps(..)
    , patch2patchinfo
    , patchname
    , patchcontents
    , fmapNamed
    , fmapFL_Named
    , mergerIdNamed
    , ShowDepsFormat(..)
    , ShowWhichDeps(..)
    , showDependencies
    ) where

import Darcs.Prelude

import Data.List.Ordered ( nubSort )
import qualified Data.Set as S

import Darcs.Patch.CommuteFn ( MergeFn, commuterIdFL, mergerIdFL )
import Darcs.Patch.Conflict ( Conflict(..), findConflicting )
import Darcs.Patch.Debug ( PatchDebug(..) )
import Darcs.Patch.Effect ( Effect(effect) )
import Darcs.Patch.FileHunk ( IsHunk(..) )
import Darcs.Patch.Format ( PatchListFormat )
import Darcs.Patch.Info ( PatchInfo, readPatchInfo, showPatchInfo, patchinfo,
                          piName, displayPatchInfo, makePatchname )
import Darcs.Patch.Merge ( CleanMerge(..), Merge(..) )
import Darcs.Patch.Object ( ObjectId )
import Darcs.Patch.Apply ( Apply(..), ObjectIdOfPatch )
import Darcs.Patch.Commute ( Commute(..) )
import Darcs.Patch.Ident ( Ident(..), PatchId )
import Darcs.Patch.Inspect ( PatchInspect(..) )
import Darcs.Patch.Permutations ( genCommuteWhatWeCanRL )
import Darcs.Patch.Read ( ReadPatch(..) )
import Darcs.Patch.FromPrim ( PrimPatchBase(..), FromPrim(..) )
import Darcs.Util.Parser ( Parser, option, lexChar,
                                choice, skipWhile, anyChar )
import Darcs.Patch.Repair ( mapMaybeSnd, Repair(..), RepairToFL, Check(..) )
import Darcs.Patch.Show
    ( ShowContextPatch(..)
    , ShowPatch(..)
    , ShowPatchBasic(..)
    , ShowPatchFor(..)
    , displayPatch
    )
import Darcs.Patch.Summary
    ( Summary(..)
    , plainSummaryFL
    )
import Darcs.Patch.Unwind ( Unwind(..), squashUnwound )
import Darcs.Patch.Viewing () -- for ShowPatch FL instances

import Darcs.Patch.Witnesses.Eq ( Eq2(..) )
import Darcs.Patch.Witnesses.Ordered
    ( (:>)(..), (:\/:)(..), (:/\:)(..)
    , FL(..), RL(..), mapFL, mapRL, mapFL_FL, mapRL_RL
    , (+<+), (+>+), concatRLFL, reverseFL
    , (+<<+), (+>>+), concatFL
    )
import Darcs.Patch.Witnesses.Sealed ( Sealed, mapSeal )
import Darcs.Patch.Witnesses.Show ( Show1, Show2 )

import Darcs.Util.IsoDate ( showIsoDateTime, theBeginning )
import Darcs.Util.Printer
    ( Doc, ($$), (<+>), text, vcat, cyanText, blueText, redText )

-- | The @Named@ type adds a patch info about a patch, that is a name.
data Named p wX wY where
    NamedP :: !PatchInfo
           -> ![PatchInfo]
           -> !(FL p wX wY)
           -> Named p wX wY
   deriving Show
-- ^ @NamedP info deps p@ represents patch @p@ with name
-- @info@. @deps@ is a list of dependencies added at the named patch
-- level, compared with the unnamed level (ie, dependencies added with
-- @darcs record --ask-deps@).

instance PrimPatchBase p => PrimPatchBase (Named p) where
    type PrimOf (Named p) = PrimOf p

instance Effect p => Effect (Named p) where
    effect (NamedP _ _ p) = effect p

type instance PatchId (Named p) = PatchInfo

instance Ident (Named p) where
    ident = patch2patchinfo

instance IsHunk (Named p) where
    isHunk _ = Nothing

instance PatchListFormat (Named p)

instance (ReadPatch p, PatchListFormat p) => ReadPatch (Named p) where
 readPatch' = readNamed

readNamed :: (ReadPatch p, PatchListFormat p) => Parser (Sealed (Named p wX))
readNamed = do n <- readPatchInfo
               d <- readDepends
               p <- readPatch'
               return $ (NamedP n d) `mapSeal` p

readDepends :: Parser [PatchInfo]
readDepends =
  option [] $ do lexChar '<'
                 readPis

readPis :: Parser [PatchInfo]
readPis = choice [ do pi <- readPatchInfo
                      pis <- readPis
                      return (pi:pis)
                 , do skipWhile (/= '>')
                      _ <- anyChar
                      return [] ]

instance Apply p => Apply (Named p) where
    type ApplyState (Named p) = ApplyState p
    apply (NamedP _ _ p) = apply p
    unapply (NamedP _ _ p) = unapply p

instance RepairToFL p => Repair (Named p) where
    applyAndTryToFix (NamedP n d p) = mapMaybeSnd (NamedP n d) `fmap` applyAndTryToFix p

anonymous :: FromPrim p => FL (PrimOf p) wX wY -> IO (Named p wX wY)
anonymous ps = do
  info <- patchinfo (showIsoDateTime theBeginning) "anonymous" "unknown" ["anonymous"]
  return $ infopatch info ps

infopatch :: forall p wX wY. FromPrim p => PatchInfo -> FL (PrimOf p) wX wY -> Named p wX wY
infopatch pi ps = NamedP pi [] (fromPrims pi ps) where

adddeps :: Named p wX wY -> [PatchInfo] -> Named p wX wY
adddeps (NamedP pi _ p) ds = NamedP pi ds p

setinfo :: PatchInfo -> Named p wX wY -> Named p wX wY
setinfo i (NamedP _ ds ps) = NamedP i ds ps

-- | This slightly ad-hoc class is here so we can call 'getdeps' with patch
-- types that wrap a 'Named', such as 'RebaseChange'.
class HasDeps p where
  getdeps :: p wX wY -> [PatchInfo]

instance HasDeps (Named p) where
  getdeps (NamedP _ ds _) = ds

patch2patchinfo :: Named p wX wY -> PatchInfo
patch2patchinfo (NamedP i _ _) = i

patchname :: Named p wX wY -> String
patchname (NamedP i _ _) = show $ makePatchname i

patchcontents :: Named p wX wY -> FL p wX wY
patchcontents (NamedP _ _ p) = p

patchcontentsRL :: RL (Named p) wX wY -> RL p wX wY
patchcontentsRL = concatRLFL . mapRL_RL patchcontents

fmapNamed :: (forall wA wB . p wA wB -> q wA wB) -> Named p wX wY -> Named q wX wY
fmapNamed f (NamedP i deps p) = NamedP i deps (mapFL_FL f p)

fmapFL_Named :: (FL p wA wB -> FL q wC wD) -> Named p wA wB -> Named q wC wD
fmapFL_Named f (NamedP i deps p) = NamedP i deps (f p)

instance (Commute p, Eq2 p) => Eq2 (Named p) where
    unsafeCompare (NamedP n1 ds1 ps1) (NamedP n2 ds2 ps2) =
        n1 == n2 && ds1 == ds2 && unsafeCompare ps1 ps2

instance Commute p => Commute (Named p) where
    commute (NamedP n1 d1 p1 :> NamedP n2 d2 p2) =
        if n2 `elem` d1 || n1 `elem` d2
        then Nothing
        else do (p2' :> p1') <- commute (p1 :> p2)
                return (NamedP n2 d2 p2' :> NamedP n1 d1 p1')

instance CleanMerge p => CleanMerge (Named p) where
    cleanMerge (NamedP n1 d1 p1 :\/: NamedP n2 d2 p2)
      | n1 == n2 = error "cannot cleanMerge identical Named patches"
      | otherwise = do
          p2' :/\: p1' <- cleanMerge (p1 :\/: p2)
          return $ NamedP n2 d2 p2' :/\: NamedP n1 d1 p1'

instance Merge p => Merge (Named p) where
    merge (NamedP n1 d1 p1 :\/: NamedP n2 d2 p2)
      | n1 == n2 = error "cannot merge identical Named patches"
      | otherwise =
          case merge (p1 :\/: p2) of
            (p2' :/\: p1') -> NamedP n2 d2 p2' :/\: NamedP n1 d1 p1'

-- Merge an unnamed patch with a named patch.
-- This operation is safe even if the first patch is named, as names can
-- never conflict with each other.
-- This is in contrast with commuterIdNamed which is not safe and hence
-- is defined closer to the code that uses it.
mergerIdNamed :: MergeFn p1 p2 -> MergeFn p1 (Named p2)
mergerIdNamed merger (p1 :\/: NamedP n2 d2 p2) =
   case mergerIdFL merger (p1 :\/: p2) of
     p2' :/\: p1' -> NamedP n2 d2 p2' :/\: p1'

{- | This instance takes care of handling the interaction between conflict
resolution and explicit dependencies. A conflict involves a set of two or
more patches and the general rule is that the conflict is considered
resolved if there is another (later) patch that (transitively) depends on
each of the (mutually) conflicting patches.

This principle extends to explicit dependencies between 'Named' patches. In
particular, recording a tag has the effect of resolving any as yet
unresolved conflicts in a repo.

In general a 'Named' patch contains multiple changes ( a "changeset").
Consider the named patches

@
  Named A [] a
  Named B [] (b1;b2)
  Named C [] c
  Named D [A,B] _
@

where, at the RepoPatch level, @a@ conflicts with @b1@, and @c@ with @b2@.
@D@ depends explicitly on both @A@ and @B@, so it fully covers the conflict
between @a@ and @b1@ and thus we would be justified to consider that
particular conflict as resolved. Unfortunately we cannot detect this at the
Named patch level because RepoPatchV1 and V2 have no notion of patch
identities. Thus, at the Named level the two underlying conflicts appear as
a single large conflict between the three named patches @A@, @B@, and @C@,
and this means that patch @D@ does /not/ count as a (partial) resolution
(even though it arguably should).

When we decide that a set of conflicting Named patches is resolved, we move
the RepoPatches contained in them to the context of the resolution. For all
other named patches, we must commute as much of their contents as possible
past the ones marked as resolved, using commutation at the RepoPatch level
(i.e. ignoring explicit dependencies). -}

instance ( Commute p
         , Conflict p
         , Summary p
         , PrimPatchBase p
         , PatchListFormat p
         , ShowPatch p
         ) =>
         Conflict (Named p) where
  isConflicted (NamedP _ _ ps) = or (mapFL isConflicted ps)
  resolveConflicts context patches =
    case separate S.empty [] context patches NilFL NilFL of
      resolved :> unresolved ->
        resolveConflicts (patchcontentsRL context +<<+ resolved) (reverseFL unresolved)
    where
      -- Separate the patch contents of an 'RL' of 'Named' patches into those
      -- we regard as resolved due to explicit dependencies and any others.
      -- Implicit dependencies are kept with the resolved patches. The first
      -- parameter accumulates the PatchInfo of patches which we consider
      -- resolved; the second one accumulates direct and indirect explicit
      -- dependencies for the patches we have traversed. The third parameter
      -- is the context, which is only needed as input to 'findConflicting'.
      separate
        :: S.Set PatchInfo    -- names of resolved Named patches so far
        -> [S.Set PatchInfo]  -- transitive explicit dependencies so far
        -> RL (Named p) w0 w1 -- context for Named patches
        -> RL (Named p) w1 w2 -- Named patches under consideration
        -> FL p w2 w3         -- result: resolved at RepoPatch layer so far
        -> FL p w3 w4         -- result: unresolved at RepoPatch layer so far
        -> (FL p :> FL p) w1 w4
      separate acc_res acc_deps ctx (ps :<: p@(NamedP name deps contents)) resolved unresolved
        | name `S.member` acc_res || isConflicted p
        , _ :> _ :> conflicting <- findConflicting (ctx +<+ ps) p
        , let conflict_ids = S.fromList $ name : mapRL ident conflicting
        , any (conflict_ids `S.isSubsetOf`) acc_deps =
          -- Either we already determined that p is considered resolved,
          -- or p is conflicted and all patches involved in the conflict are
          -- transitively explicitly depended upon by a single patch.
          -- The action is to regard everything in 'contents' as resolved.
          separate (acc_res `S.union` conflict_ids) (extend name deps acc_deps)
            ctx ps (contents +>+ resolved) unresolved
        | otherwise =
          -- Commute as much as we can of our patch 'contents' past 'resolved',
          -- without dragging dependencies along.
          -- To use existing tools for commutation means we have to
          -- commuteWhatWeCan 'resolved' backwards through the 'contents',
          -- now /with/ dragging dependencies along.
          case genCommuteWhatWeCanRL (commuterIdFL commute)
                (reverseFL contents :> resolved) of
            dragged :> resolved' :> more_unresolved ->
              separate acc_res (extend name deps acc_deps) ctx ps
                (dragged +>>+ resolved') (more_unresolved +>>+ unresolved)
      separate _ _ _ NilRL resolved unresolved = resolved :> unresolved

      -- Extend a list of sets of dependencies by adding the new list of
      -- dependencies to each set that contains the given 'name'. If 'name'
      -- does not occur in any of the sets, we add the dependencies as a new
      -- set to the list.
      -- Since we have to track whether 'name' was found in any of the input
      -- sets, this is not a straight-forward fold, so we use explicit
      -- recursion.
      extend :: Ord a => a -> [a] -> [S.Set a] -> [S.Set a]
      extend _ [] acc_deps = acc_deps
      extend name deps acc_deps = go False (S.fromList deps) acc_deps where
        go False new [] = [new]
        go True _ [] = []
        go found new (ds:dss)
          | name `S.member` ds = ds `S.union` new : go True new dss
          | otherwise = ds : go found new dss

instance (PrimPatchBase p, Unwind p) => Unwind (Named p) where
  fullUnwind (NamedP _ _ ps) = squashUnwound (mapFL_FL fullUnwind ps)

instance PatchInspect p => PatchInspect (Named p) where
    listTouchedFiles (NamedP _ _ p) = listTouchedFiles p
    hunkMatches f (NamedP _ _ p) = hunkMatches f p

instance Summary p => Summary (Named p) where
    conflictedEffect = conflictedEffect . patchcontents

instance Check p => Check (Named p) where
    isInconsistent (NamedP _ _ p) = isInconsistent p

-- ForStorage: note the difference between use of <> when there are
-- no explicit dependencies vs. <+> when there are
showNamedPrefix :: ShowPatchFor -> PatchInfo -> [PatchInfo] -> Doc -> Doc
showNamedPrefix f@ForStorage n [] p =
    showPatchInfo f n <> p
showNamedPrefix f@ForStorage n d p =
    showPatchInfo f n
    $$ blueText "<"
    $$ vcat (map (showPatchInfo f) d)
    $$ blueText ">"
    <+> p
showNamedPrefix f@ForDisplay n [] p =
    showPatchInfo f n
    $$ p
showNamedPrefix f@ForDisplay n d p =
    showPatchInfo f n
    $$ showDependencies ShowNormalDeps ShowDepsVerbose d
    $$ p

instance (PatchListFormat p, ShowPatchBasic p) => ShowPatchBasic (Named p) where
    showPatch f (NamedP n d p) = showNamedPrefix f n d $ showPatch f p

instance ( Apply p
         , IsHunk p
         , PatchListFormat p
         , ObjectId (ObjectIdOfPatch p)
         , ShowContextPatch p
         ) =>
         ShowContextPatch (Named p) where
    showContextPatch f (NamedP n d p) =
        showNamedPrefix f n d <$> showContextPatch f p

data ShowDepsFormat = ShowDepsVerbose | ShowDepsSummary deriving (Eq)

-- | Support for rebase
data ShowWhichDeps = ShowNormalDeps | ShowDroppedDeps deriving (Eq)

showDependencies :: ShowWhichDeps -> ShowDepsFormat -> [PatchInfo] -> Doc
showDependencies which format deps = vcat (map showDependency deps)
  where
    showDependency d =
      case format of
        ShowDepsVerbose ->
          mark which format <+> cyanText (show (makePatchname d)) $$
          text "  *" <+> text (piName d)
        ShowDepsSummary ->
          mark which format <+>
          cyanText (take 8 (show (makePatchname d))) <+> text (piName d)
    mark ShowNormalDeps ShowDepsVerbose = blueText "depend"
    mark ShowDroppedDeps ShowDepsVerbose = redText "dropped"
    mark ShowNormalDeps ShowDepsSummary = text "D"
    mark ShowDroppedDeps ShowDepsSummary = text "D!"

instance (Summary p, PatchListFormat p,
          PrimPatchBase p, ShowPatch p) => ShowPatch (Named p) where
    description (NamedP n _ _) = displayPatchInfo n
    summary (NamedP _ ds ps) =
        showDependencies ShowNormalDeps ShowDepsSummary ds $$ plainSummaryFL ps
    summaryFL nps =
        showDependencies ShowNormalDeps ShowDepsSummary ds $$ plainSummaryFL ps
      where
        ds = nubSort $ concat $ mapFL getdeps nps
        ps = concatFL $ mapFL_FL patchcontents nps
    content (NamedP _ ds ps) =
        showDependencies ShowNormalDeps ShowDepsVerbose ds $$ displayPatch ps

instance Show2 p => Show1 (Named p wX)

instance Show2 p => Show2 (Named p)

instance PatchDebug p => PatchDebug (Named p)

