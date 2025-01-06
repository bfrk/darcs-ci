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
import Darcs.Patch.Conflict ( Conflict(..), findConflicting, isConflicted )
import Darcs.Patch.Debug ( PatchDebug(..) )
import Darcs.Patch.Effect ( Effect(effect) )
import Darcs.Patch.FileHunk ( IsHunk(..) )
import Darcs.Patch.Format ( FormatPatch(..) )
import Darcs.Patch.Info
    ( PatchInfo
    , makePatchname
    , patchinfo
    , piName
    , readPatchInfo
    , showPatchInfo
    , formatPatchInfo
    )
import Darcs.Patch.Merge ( CleanMerge(..), Merge(..) )
import Darcs.Patch.Object ( ObjectId )
import Darcs.Patch.Apply ( Apply(..), ObjectIdOfPatch )
import Darcs.Patch.Commute ( Commute(..) )
import Darcs.Patch.Ident ( Ident(..), PatchId )
import Darcs.Patch.Inspect ( PatchInspect(..) )
import Darcs.Patch.Permutations ( genCommuteWhatWeCanRL )
import Darcs.Patch.Read ( ReadPatch(..), ReadPatches(..) )
import Darcs.Patch.FromPrim ( PrimPatchBase(..), FromPrim(..) )
import Darcs.Util.Parser ( Parser, option, lexChar,
                                choice, skipWhile, anyChar )
import Darcs.Patch.Repair ( mapMaybeSnd, Repair(..), RepairToFL, Check(..) )
import Darcs.Patch.Show
    ( ShowContextPatch(..)
    , ShowPatch(..)
    , ShowPatchBasic(..)
    , showPatch
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
    , FL(..), RL(..), mapFL, mapFL_FL, mapRL_RL
    , (+<+), (+>+), concatRLFL, reverseFL, reverseRL
    , (+<<+), (+>>+), concatFL
    )
import Darcs.Patch.Witnesses.Sealed ( Sealed, mapSeal )
import Darcs.Patch.Witnesses.Show ( Show1, Show2 )

import qualified Darcs.Util.Format as F
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

instance ReadPatches p => ReadPatch (Named p) where
    readPatch' = readNamed

-- this instance is only needed for reading patch bundles
instance ReadPatches p => ReadPatches (Named p)

readNamed :: ReadPatches p => Parser (Sealed (Named p wX))
readNamed = do n <- readPatchInfo
               d <- readDepends
               p <- readPatchFL'
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

To implement this here in a generic way without touching existing instances
for the underlying RepoPatch type @p@, we use the following trick: we move
the contents of patches which we regard as resolved at the Named patch layer
from the "interesting" trailing sequence to the "uninteresting" context,
before passing both on to the lower level 'resolveConflicts'. The challenge
here is to define and then compute the patches "resolved at this (Named)
layer" in such a way that it does not depend on patch order.

The algorithm is roughly as follows:

In a first pass (function 'prepare') we accumulate (direct) conflicts
between Named patches. This is done for all conflicted patches in the
interesting trailing sequence, as well as for any patch that conflicts with
one of them. In the same pass we calculate transitive explicit dependency
sets. This terminates when we have exhausted both the trailing patch
sequence plus any additional patches we add along the way and which we pass
along in the first argument (@todo@).

In a second pass we actually move (the contents of) patches from the
trailing to the context sequence.

Implementation note: I think it would be possible to fuse the two passes
into one, incrementally extending both transitive dependencies and
conflicts. I fear, however, that this will make it much harder to understand
what's going on. And the efficiency gain is probably minimal and in any case
at most an improvement by a constant factor. -}

instance ( Commute p
         , Conflict p
         , Summary p
         , PrimPatchBase p
         , ShowPatch p
         ) =>
         Conflict (Named p) where
  numConflicts (NamedP _ _ ps) = sum (mapFL numConflicts ps)
  resolveConflicts context patches =
    case separate patches NilFL NilFL of
      resolved :> unresolved ->
        resolveConflicts (patchcontentsRL context +<<+ resolved) (reverseFL unresolved)
    where
      -- This partitions the patch contents into 'resolved' (by explicit
      -- dependencies) and 'unresolved'. The 'resolved' part contains the
      -- contents of all patches for which all direct conflicts it is involved
      -- in are transitively covered via explicit dependencies by a single
      -- patch. For all other patches we commute as much as we can out to the
      -- 'unresolved' part.
      separate
        :: RL (Named p) w1 w2 -- Named patches under consideration
        -> FL p w2 w3         -- result: resolved at RepoPatch layer so far
        -> FL p w3 w4         -- result: unresolved at RepoPatch layer so far
        -> (FL p :> FL p) w1 w4
      separate (ps :<: NamedP name _ contents) resolved unresolved
        | -- any direct conflict that we are part of
          css <- S.filter (name `S.member`) final_conflicts
          -- ... needs to be fully covered (transitively) by a single patch
        , all (\cs -> any (cs `S.isSubsetOf`) final_depends) css =
          separate ps (contents +>+ resolved) unresolved
        | otherwise =
          -- Commute as much as we can of our patch 'contents' past 'resolved',
          -- without dragging dependencies along.
          -- To use existing tools for commutation means we have to
          -- commuteWhatWeCan 'resolved' backwards through the 'contents',
          -- now /with/ dragging dependencies along.
          case genCommuteWhatWeCanRL (commuterIdFL commute)
                (reverseFL contents :> resolved) of
            dragged :> resolved' :> more_unresolved ->
              separate ps
                (dragged +>>+ resolved') (more_unresolved +>>+ unresolved)
      separate NilRL resolved unresolved = resolved :> unresolved

      (final_conflicts, final_depends) = prepare S.empty S.empty [] context patches

      -- Calculate direct conflicts and transitive explicit dependencies. This
      -- needs to (potentially) look at the complete history, but as we do for
      -- RepoPatchV3 resolution we terminate early when the set of interesting
      -- patches ('todo') becomes exhausted.
      -- Accumulating parameters:
      -- * todo: patch names to consider, namely all participants in conflicts
      --   we encounter on the way.
      --   invariant: never contains name of a patch we already traversed
      -- * conflicts: set of direct conflicts constructed so far; note that
      --   each element is a pair i.e. two-element set
      -- * depends: list of transitive explicit dependency sets so far
      prepare
        :: S.Set PatchInfo        -- todo
        -> S.Set(S.Set PatchInfo) -- direct conflicts so far
        -> [S.Set PatchInfo]      -- transitive explicit dependencies so far
        -> RL (Named p) wA wB     -- context
        -> RL (Named p) wB wC     -- patches under consideration
        -> (S.Set (S.Set PatchInfo), [S.Set PatchInfo])
      prepare todo conflicts depends ctx (ps :<: p)
        | isConflicted p || ident p `S.member` todo =
            prepare (updTodo p cs todo) (updConflicts p cs conflicts)
              (updDepends p depends) ctx ps
        | otherwise = -- not part of any conflict
            prepare (updTodo p cs todo) conflicts (updDepends p depends) ctx ps
        where cs = conflictingNames (ctx +<+ ps) p
      prepare todo conflicts depends _ NilRL
        | S.null todo = (conflicts, depends)
      prepare todo conflicts depends (ctx :<: p) NilRL
        | ident p `S.member` todo || any (`S.member` todo) cs =
            prepare (updTodo p cs todo) (updConflicts p cs conflicts)
              (updDepends p depends) ctx NilRL
        | otherwise =
            -- may be part of a conflict but not with any interesting
            -- patch, so we can and should ignore its conflicts
            prepare (updTodo p S.empty todo) conflicts
              (updDepends p depends) ctx NilRL
        where cs = conflictingNames ctx p
      prepare _ _ _ NilRL NilRL = error "autsch, hit the bottom"

      updTodo (NamedP name _ _) cs todo = cs <> (name `S.delete` todo)
      updConflicts (NamedP name _ _) our_cs all_cs =
        S.map (`S.insert` S.singleton name) our_cs <> all_cs
      updDepends (NamedP n ds _) = extendDeps n ds

      -- Extend a list of sets of dependencies by adding the new list of
      -- dependencies to each set that contains the given 'name'. If 'name'
      -- does not occur in any of the sets, we add the dependencies as a new
      -- set to the list.
      -- Since we have to track whether 'name' was found in any of the input
      -- sets, this is not a straight-forward fold, so we use explicit
      -- recursion.
      extendDeps :: Ord a => a -> [a] -> [S.Set a] -> [S.Set a]
      extendDeps _ [] = id
      extendDeps name new_deps = go False (S.fromList new_deps) where
        go False new [] = [new]
        go True _ [] = []
        go found new (ds:dss)
          | name `S.member` ds = ds `S.union` new : go True new dss
          | otherwise = ds : go found new dss

      -- The set of (direct) conflicts we can read off a patch (in context).
      -- This is slightly more involved than just calling 'findConflicting' due
      -- to the fact that the latter also commutes out any patch that
      -- explicitly depends on the ones we actually conflict with.
      conflictingNames ctx p =
        case findConflicting ctx p of
          _ :> p' :> ps -> onlyRealConflicts p' (reverseRL ps) S.empty

      -- This filters out patches that 'findConflicting' finds
      -- that are /only/ there because they explicitly depend on
      -- patches that are actually in conflict.
      onlyRealConflicts
        :: Named p wB wC
        -> FL (Named p) wC wD
        -> S.Set (PatchInfo)
        -> S.Set (PatchInfo)
      onlyRealConflicts _ NilFL r = r
      onlyRealConflicts p (q :>: qs) r =
        case commute (p :> q) of
          Just (_ :> p')
            | numConflicts p /= numConflicts p' ->
                onlyRealConflicts p' qs (patch2patchinfo q `S.insert` r)
            | otherwise -> onlyRealConflicts p' qs r
          Nothing ->
            -- This should be 'error "impossible"' but due to commutation
            -- bugs in V1 and V2 we would run into those errors quite a lot.
            -- So we act as if the rest (qs) are real conflicts. Which is
            -- wrong but better than crashing darcs for those legacy formats.
            S.fromList (mapFL patch2patchinfo qs) `S.union` r

instance (PrimPatchBase p, Unwind p) => Unwind (Named p) where
  fullUnwind (NamedP _ _ ps) = squashUnwound (mapFL_FL fullUnwind ps)

instance PatchInspect p => PatchInspect (Named p) where
    listTouchedFiles (NamedP _ _ p) = listTouchedFiles p
    hunkMatches f (NamedP _ _ p) = hunkMatches f p

instance Summary p => Summary (Named p) where
    conflictedEffect = conflictedEffect . patchcontents

instance Check p => Check (Named p) where
    isInconsistent (NamedP _ _ p) = isInconsistent p

showNamedPrefix :: PatchInfo -> [PatchInfo] -> Doc -> Doc
showNamedPrefix n d p =
  showPatchInfo n $$ showDependencies ShowNormalDeps ShowDepsVerbose d $$ p

instance FormatPatch p => FormatPatch (Named p) where
    -- note the difference between use of <> when there are
    -- no explicit dependencies vs. <+> when there are
    formatPatch (NamedP n d ps) = storeNamedPrefix d $ formatPatchFL ps
      where
        storeNamedPrefix [] p = formatPatchInfo n <> p
        storeNamedPrefix ds p =
          F.vcat
            [ formatPatchInfo n
            , F.ascii "<"
            , F.vcat (map formatPatchInfo ds)
            , F.ascii ">" F.<+> p
            ]

instance ShowPatchBasic p => ShowPatchBasic (Named p) where
    showPatch (NamedP n d p) = showNamedPrefix n d $ showPatch p

instance ( Apply p
         , IsHunk p
         , ObjectId (ObjectIdOfPatch p)
         , ShowContextPatch p
         ) =>
         ShowContextPatch (Named p) where
    showPatchWithContextAndApply (NamedP n d p) =
        showNamedPrefix n d <$> showPatchWithContextAndApply p

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

instance (Summary p, PrimPatchBase p, ShowPatch p) => ShowPatch (Named p) where
    description (NamedP n _ _) = showPatchInfo n
    summary (NamedP _ ds ps) =
        showDependencies ShowNormalDeps ShowDepsSummary ds $$ plainSummaryFL ps
    summaryFL nps =
        showDependencies ShowNormalDeps ShowDepsSummary ds $$ plainSummaryFL ps
      where
        ds = nubSort $ concat $ mapFL getdeps nps
        ps = concatFL $ mapFL_FL patchcontents nps
    content (NamedP _ ds ps) =
        showDependencies ShowNormalDeps ShowDepsVerbose ds $$ showPatch ps

instance Show2 p => Show1 (Named p wX)

instance Show2 p => Show2 (Named p)

instance PatchDebug p => PatchDebug (Named p)
