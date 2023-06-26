-- Copyright (C) 2003-2004 David Roundy
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

{- | Definitions used in this module:

[Explicit dependencies]: The set of patches that a (named) patch depends on
  "by name", i.e. irrespective of (non-)commutation (non commuting patches are
  implicit dependencies). The most important example are tags, but non-tag
  patches can also have explicit dependencies by recording them with
  --ask-deps.

[Covered]: A patch @p@ is covered by a tag @t@ if @t@ explicitly depends on
  @p@ or a tag covered by @t@ explicitly depends on @p@. In other words, the
  transitive closure of the relation "is depended on", restricted to
  situations where the right hand side is a tag. Note that it does /not/ take
  explicit dependencies of non-tag patches into account at all.

[Clean]: A tag @t@ in a repository is clean if all patches prior to the tag are
  covered by @t@. Tags normally start out as clean tags (the exception is
  if --ask-deps is used). It typically becomes unclean when it is merged into
  another repo (here the exceptions are if --reorder-patches is used, or if
  the target repo is actually a subset of the source repo).
-}

module Darcs.Patch.Depends
    ( getUncovered
    , areUnrelatedRepos
    , findCommon
    , patchSetMerge
    , countUsThem
    , removeFromPatchSet
    , slightlyOptimizePatchset
    , fullyOptimizePatchSet
    , splitOnTag
    , patchSetUnion
    , patchSetIntersection
    , cleanLatestTag
    , contextPatches
    ) where

import Darcs.Prelude

import Control.Applicative ( (<|>) )
import Data.List ( delete, foldl1', intersect, (\\) )

import Darcs.Patch.Named ( getdeps )
import Darcs.Patch.Commute ( Commute )
import Darcs.Patch.Ident ( fastRemoveSubsequenceRL, findCommonRL )
import Darcs.Patch.Info ( PatchInfo, isTag )
import Darcs.Patch.Merge ( Merge(..) )
import Darcs.Patch.Permutations ( partitionRL )
import Darcs.Patch.PatchInfoAnd( PatchInfoAnd, hopefully, info )
import Darcs.Patch.Set
    ( Origin
    , PatchSet(..)
    , SealedPatchSet
    , Tagged(..)
    , appendPSFL
    , emptyPatchSet
    , patchSet2FL
    , patchSet2RL
    , patchSetSplit
    )
import Darcs.Patch.Progress ( progressRL )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoercePStart )
import Darcs.Patch.Witnesses.Eq ( Eq2(..) )
import Darcs.Patch.Witnesses.Ordered
    ( (:\/:)(..), (:/\:)(..), (:>)(..), Fork(..),
    mapFL, RL(..), FL(..), isShorterThanRL, breakRL,
    (+<+), reverseFL, reverseRL, mapRL )
import Darcs.Patch.Witnesses.Sealed
    ( Sealed(..), seal )

{-|
Find clean tags that are common to both argument 'PatchSet's and return a
'Fork' with the common clean tags and whatever remains of the 'PatchSet's.
The two "uncommon" sequences may still have patches in common, even clean
tags, since we look only at the "known clean" tags of the second argument,
i.e. those that are the head of a 'Tagged' section.

This is a pretty efficient function, because it makes use of the
already-broken-up nature of 'PatchSet's.

Note that the first argument should be the repository that is more cheaply
accessed (i.e. local), as 'taggedIntersection' does its best to reduce the
number of inventories that are accessed from its second argument.
-}
taggedIntersection :: forall p wX wY . Commute p
                   => PatchSet p Origin wX -> PatchSet p Origin wY ->
                      Fork (RL (Tagged p))
                           (RL (PatchInfoAnd p))
                           (RL (PatchInfoAnd p)) Origin wX wY
taggedIntersection (PatchSet NilRL ps1) s2 = Fork NilRL ps1 (patchSet2RL s2)
taggedIntersection s1 (PatchSet NilRL ps2) = Fork NilRL (patchSet2RL s1) ps2
taggedIntersection s1 (PatchSet (ts2 :<: Tagged t2ps t2 _) ps2) =
  -- First try to find t2 in the heads of Tagged sections of s1;
  -- if that fails, try to reorder patches in s1 so that it does;
  -- otherwise t2 does not occur in s1, so recurse with the current
  -- Tagged section of s2 unwrapped.
  case maybeSplitSetOnTag (info t2) s1 <|> splitOnTag (info t2) s1 of
    Just (PatchSet ts1 ps1) -> Fork ts1 ps1 (unsafeCoercePStart ps2)
    Nothing -> taggedIntersection s1 (PatchSet ts2 (t2ps :<: t2 +<+ ps2))

-- |'maybeSplitSetOnTag' takes a tag's 'PatchInfo', @t0@, and a 'PatchSet' and
-- attempts to find @t0@ in one of the 'Tagged's in the PatchSet. If the tag is
-- found, the 'PatchSet' is split up, on that tag, such that all later patches
-- are in the "since last tag" patch list. If the tag is not found, 'Nothing'
-- is returned.
-- This is a simpler version of 'splitOnTag' that only looks at the heads
-- of 'Tagged' sections and does not commute any patches.
maybeSplitSetOnTag :: PatchInfo -> PatchSet p wStart wX
                   -> Maybe (PatchSet p wStart wX)
maybeSplitSetOnTag t0 origSet@(PatchSet (ts :<: Tagged pst t _) ps)
    | t0 == info t = Just origSet
    | otherwise = do
        PatchSet ts' ps' <- maybeSplitSetOnTag t0 (PatchSet ts (pst :<: t))
        Just $ PatchSet ts' (ps' +<+ ps)
maybeSplitSetOnTag _ _ = Nothing

-- | Take a tag's 'PatchInfo', and a 'PatchSet', and attempt to find the tag in
-- the 'PatchSet'. If found, return a new 'PatchSet', in which the tag is now
-- clean (and the last of the 'Tagged' list), while all patches that are not
-- covered by the tag are in the trailing list of patches.
-- If the tag is not in the 'PatchSet', we return 'Nothing'.
splitOnTag :: Commute p => PatchInfo -> PatchSet p wStart wX
           -> Maybe (PatchSet p wStart wX)
-- If the tag we are looking for is the first Tagged tag of the patchset, we
-- are done.
splitOnTag t s@(PatchSet (_ :<: Tagged _ hp _) _) | info hp == t = Just s
-- If the tag is the most recent patch in the set, we check if the patch is the
-- only non-depended-on patch in the set (i.e. it is a clean tag); creating a
-- new Tagged out of the patches and tag, and adding it to the patchset, if
-- this is the case. Otherwise, we try to make the tag clean.
splitOnTag t patchset@(PatchSet ts hps@(ps :<: hp)) | info hp == t =
    if getUncovered patchset == [t]
        then
          -- If t is the only patch not covered by any tag, then it is clean
          Just $ PatchSet (ts :<: Tagged ps hp Nothing) NilRL
        else
          -- Make it clean by commuting out patches not explicitly depended on
          -- by @t@; since we do this with just the trailing sequence @hps@ i.e.
          -- we don't include the tag of the next Tagged, we have to make an
          -- extra check to see if this tag is covered, too, and otherwise
          -- recurse with the next Tagged section unwrapped. Note that we cannot
          -- simply check if @t@ depends on this tag because it may depend
          -- indirectly via unclean tags contained in @hps@.
          case partitionRL ((`notElem` (t : getdeps (hopefully hp))) . info) hps of
            tagAndDeps@(ds' :<: hp') :> nonDeps ->
                -- check if t is now fully clean
                if getUncovered (PatchSet ts tagAndDeps) == [t]
                    then let tagged = Tagged ds' hp' Nothing in
                         return $ PatchSet (ts :<: tagged) nonDeps
                    else do
                        unfolded <- unwrapOneTagged $ PatchSet ts tagAndDeps
                        PatchSet ts' ps' <- splitOnTag t unfolded
                        return $ PatchSet ts' (ps' +<+ nonDeps)
            _ -> error "impossible case"
-- We drop the leading patch, to try and find a non-Tagged tag.
splitOnTag t (PatchSet ts (ps :<: p)) = do
    PatchSet ns xs <- splitOnTag t (PatchSet ts ps)
    return $ PatchSet ns (xs :<: p)
-- If there are no patches left, we "unfold" the next Tagged, and try again.
splitOnTag t0 patchset@(PatchSet (_ :<: Tagged _ _ _) NilRL) =
    unwrapOneTagged patchset >>= splitOnTag t0
-- If we've checked all the patches, but haven't found the tag, return Nothing.
splitOnTag _ (PatchSet NilRL NilRL) = Nothing

-- | Reorder a 'PatchSet' such that the latest tag becomes clean.
cleanLatestTag :: Commute p
               => PatchSet p wStart wX
               -> PatchSet p wStart wX
cleanLatestTag inp@(PatchSet ts ps) =
  case breakRL (isTag . info) ps of
    NilRL :> _ -> inp -- no tag among the ps -> we are done
    (left@(_ :<: t) :> right) ->
      case splitOnTag (info t) (PatchSet ts left) of
        Just (PatchSet ts' ps') -> PatchSet ts' (ps' +<+ right)
        _ -> error "impossible case" -- because t is in left

-- | Create a 'Tagged' section for every clean tag. For unclean tags we try to
-- make them clean, but only if that doesn't make an earlier clean tag dirty.
-- This means that the operation is idempotent and in particular monotonic,
-- which justifies the "optimize" in the name.
fullyOptimizePatchSet
  :: forall p wZ . Commute p => PatchSet p Origin wZ -> PatchSet p Origin wZ
fullyOptimizePatchSet = go emptyPatchSet . patchSet2FL
  where
    go :: PatchSet p Origin wY -> FL (PatchInfoAnd p) wY wZ -> PatchSet p Origin wZ
    go s NilFL = s
    go s@(PatchSet ts ps) (q:>:qs)
      | isTag qi, getUncovered s' == [qi] =
          -- tag is clean
          go (PatchSet (ts :<: Tagged ps q Nothing) NilRL) qs
      | isTag qi, Just s'' <- makeClean s q = go s'' qs
      | otherwise = go s' qs
      where
        qi = info q
        s' = PatchSet ts (ps:<:q)

-- | Take a 'PatchSet' and an adjacent tag and try to make the tag clean
-- by commuting out trailing patches that are not covered by the tag.
makeClean
  :: Commute p
  => PatchSet p Origin wY
  -> PatchInfoAnd p wY wZ
  -> Maybe (PatchSet p Origin wZ)
makeClean (PatchSet ts ps) t =
  let ti = info t in
  case partitionRL ((`notElem` (ti : getdeps (hopefully t))) . info) (ps :<: t) of
    tagAndDeps@(ds :<: t') :> nonDeps ->
      -- check if tag really became clean
      if getUncovered (PatchSet ts tagAndDeps) == [ti]
        then Just $ PatchSet (ts :<: Tagged ds t' Nothing) nonDeps
        else Nothing
    _ -> error "imposible"

-- |'unwrapOneTagged' unfolds a single Tagged object in a PatchSet, adding the
-- tag and patches to the PatchSet's patch list.
unwrapOneTagged :: PatchSet p wX wY -> Maybe (PatchSet p wX wY)
unwrapOneTagged (PatchSet (ts :<: Tagged tps t _) ps) =
    Just $ PatchSet ts (tps :<: t +<+ ps)
unwrapOneTagged _ = Nothing

-- | Return the 'PatchInfo' for all the patches in a 'PatchSet' that are not
-- *explicitly* depended on by any tag (in the given 'PatchSet').
--
-- This is exactly the set of patches that a new tag recorded on top
-- of the 'PatchSet' would explicitly depend on.
--
-- Note that the result is not minimal with respect to dependencies, not even
-- explicit dependencies: explicit dependencies of regular (non-tag) patches
-- are completely ignored.
getUncovered :: PatchSet p wStart wX -> [PatchInfo]
getUncovered (PatchSet tagged patches) =
  findUncovered $
    case tagged of
      NilRL -> mapRL infoAndExplicitDeps patches
      _ :<: Tagged _ t _ -> mapRL infoAndExplicitDeps patches ++ [(info t, [])]
  where
    -- Both findUncovered and dropDepsIn are basically graph algorithms. We
    -- present the (directed, acyclic) graph as a topologically sorted list of
    -- vertices together with the targets of their outgoing edges. The problem
    -- findUncovered solves is to find all vertices with no incoming edges.
    -- This is done by removing all vertices reachable from any vertex in the
    -- graph.
    findUncovered :: Eq a => [(a, [a])] -> [a]
    findUncovered [] = []
    findUncovered ((pi, deps) : rest) =
        pi : findUncovered (dropDepsIn deps rest)

    -- Remove the given list of vertices from the graph, as well as all
    -- vertices reachable from them.
    dropDepsIn :: Eq a => [a] -> [(a, [a])] -> [(a, [a])]
    dropDepsIn [] ps = ps
    dropDepsIn _  [] = []
    dropDepsIn ds (hp@(hpi,hpds) : ps)
        | hpi `elem` ds = dropDepsIn (delete hpi ds ++ hpds) ps
        | otherwise = hp : dropDepsIn ds ps

    -- The patch info together with the list of explicit dependencies in case
    -- it is a tag. This constructs one element of the graph representation.
    -- It cannot be used for the tag of a Tagged section as that may not be
    -- available in a lazy repo. That's okay because we already know it is
    -- clean, so no patches preceding it it can be uncovered.
    infoAndExplicitDeps :: PatchInfoAnd p wX wY -> (PatchInfo, [PatchInfo])
    infoAndExplicitDeps p
        | isTag (info p) = (info p, getdeps $ hopefully p)
        | otherwise = (info p, [])

-- | Create a new 'Tagged' section for the most recent clean tag found in the
-- tail of un-'Tagged' patches without re-ordering patches. Note that earlier
-- tags may remain un-'Tagged' even if they are actually clean.
slightlyOptimizePatchset :: PatchSet p wStart wX -> PatchSet p wStart wX
slightlyOptimizePatchset (PatchSet ts0 ps0) =
    go $ PatchSet ts0 (progressRL "Optimizing inventory" ps0)
  where
    go :: PatchSet p wStart wY -> PatchSet p wStart wY
    go (PatchSet ts NilRL) = PatchSet ts NilRL
    go s@(PatchSet ts (ps :<: hp))
        | isTag (info hp)
        , [info hp] == getUncovered s =
            PatchSet (ts :<: Tagged ps hp Nothing) NilRL
        | otherwise = appendPSFL (go (PatchSet ts ps)) (hp :>: NilFL)

removeFromPatchSet
  :: (Commute p, Eq2 p)
  => FL (PatchInfoAnd p) wX wY
  -> PatchSet p wStart wY
  -> Maybe (PatchSet p wStart wX)
removeFromPatchSet bad s@(PatchSet ts ps)
  | all (`elem` mapRL info ps) (mapFL info bad) = do
    ps' <- fastRemoveSubsequenceRL (reverseFL bad) ps
    return (PatchSet ts ps')
  | otherwise = removeFromPatchSet bad =<< unwrapOneTagged s

-- | The symmetric difference between two 'PatchSet's, expressed as a 'Fork'
-- consisting of the intersection 'PatchSet' and the trailing lists of
-- left-only and right-only patches.
--
-- From a purely functional point of view this is a symmetric function.
-- However, laziness effects make it asymmetric: the LHS is more likely to be
-- evaluated fully, while the RHS is evaluated as sparingly as possible. For
-- efficiency, the LHS should come from the local repo and the RHS from the
-- remote one. This asymmetry can also have a semantic effect, namely if
-- 'PatchSet's have *unavailable* patches or inventories, for instance when we
-- deal with a lazy clone of a repo that is no longer accessible. In this case
-- the order of arguments may determine whether the command fails or succeeds.
findCommon
  :: Commute p
  => PatchSet p Origin wX
  -> PatchSet p Origin wY
  -> Fork (PatchSet p) (FL (PatchInfoAnd p)) (FL (PatchInfoAnd p)) Origin wX wY
findCommon us them =
  case taggedIntersection us them of
    Fork common us' them' ->
      case findCommonRL us' them' of
        Fork more_common us'' them'' ->
          Fork (PatchSet common more_common) (reverseRL us'') (reverseRL them'')

countUsThem :: Commute p
            => PatchSet p Origin wX
            -> PatchSet p Origin wY
            -> (Int, Int)
countUsThem us them =
    case taggedIntersection us them of
        Fork _ us' them' -> let uu = mapRL info us'
                                tt = mapRL info them' in
                            (length $ uu \\ tt, length $ tt \\ uu)

patchSetMerge
  :: (Commute p, Merge p)
  => PatchSet p Origin wX
  -> PatchSet p Origin wY
  -> (FL (PatchInfoAnd p) :/\: FL (PatchInfoAnd p)) wX wY
-- The first two special cases are semantically redundant but important
-- for optimization; patchSetUnion below relies on that.
patchSetMerge us (PatchSet NilRL NilRL) = NilFL :/\: patchSet2FL us
patchSetMerge (PatchSet NilRL NilRL) them = patchSet2FL them :/\: NilFL
patchSetMerge us them =
  case findCommon us them of
    Fork _ us' them' -> merge (us' :\/: them')

-- | A 'PatchSet' consisting of the patches common to all input 'PatchSet's.
-- This is *undefined* for the empty list since intersection of 'PatchSet's
-- has no unit.
patchSetIntersection
  :: Commute p => [SealedPatchSet p Origin] -> SealedPatchSet p Origin
patchSetIntersection = foldr1 go
  where
    go (Sealed ps) (Sealed acc) =
      case findCommon ps acc of
        Fork common _ _ -> seal common

-- | A 'PatchSet' consisting of the patches contained in any of the input
-- 'PatchSet's. The input 'PatchSet's are merged in left to right order, left
-- patches first.
patchSetUnion
  :: (Commute p, Merge p) => [SealedPatchSet p Origin] -> SealedPatchSet p Origin
-- You may consider simplifying this to a plain foldr'. However, this is
-- extremely inefficient because we have to build everything up from an empty
-- PatchSet. In principle this could be avoided by merging right patches first,
-- but then we get a failure in the conflict-chain-resolution test for darcs-1.
patchSetUnion [] = seal emptyPatchSet
patchSetUnion [x] = x
patchSetUnion xs = foldl1' go xs
  where
    go (Sealed acc) (Sealed ps) =
      case patchSetMerge acc ps of
        ps_only :/\: _ -> seal $ appendPSFL acc ps_only

-- | Two 'PatchSet's are considered unrelated unless they share a common
-- inventory, or either 'PatchSet' has less than 5 patches, or they have at
-- least one patch in common.
areUnrelatedRepos
  :: Commute p => PatchSet p Origin wX -> PatchSet p Origin wY -> Bool
areUnrelatedRepos us them =
  case taggedIntersection us them of
    Fork NilRL u t
      | t `isShorterThanRL` 5 -> False
      | u `isShorterThanRL` 5 -> False
      | otherwise -> null $ intersect (mapRL info u) (mapRL info t)
    _ -> False

-- | Split a 'PatchSet' at the latest clean tag. The left part is what comes
-- before the tag, the right part is the tag and its non-dependencies.
contextPatches :: PatchSet p wX wY
               -> (PatchSet p :> RL (PatchInfoAnd p)) wX wY
contextPatches = patchSetSplit . slightlyOptimizePatchset
