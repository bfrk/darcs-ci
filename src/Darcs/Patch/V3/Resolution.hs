{- | Conflict resolution for 'RepoPatchV3' -}
{-# OPTIONS_GHC -Wno-orphans #-}
module Darcs.Patch.V3.Resolution () where

import qualified Data.Set as S

import Darcs.Prelude
import Data.List ( partition, sort )

import Darcs.Patch.Commute ( commuteFL )
import Darcs.Patch.Conflict ( Conflict(..), isConflicted, mangleOrFail )
import Darcs.Patch.Ident ( Ident(..), SignedId(..), StorableId(..) )
import Darcs.Patch.Prim ( PrimPatch )
import Darcs.Patch.Prim.WithName ( PrimWithName, wnPatch )
import Darcs.Patch.V3.Contexted ( Contexted, ctxDepends, ctxId, ctxToFL )
import Darcs.Patch.V3.Core ( RepoPatchV3(..), (+|), (-|) )
import Darcs.Patch.Witnesses.Ordered ( FL(..), RL(..), mapFL_FL, (:>)(..) )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..), mapSeal )


-- * Conflict Resolution

{- This gives an overview of the algorithm for marking conflicts.

The goal is to calculate the markup for a trailing RL of patches, usually
the ones we are going to add to our repo. But since in V3 we store only the
direct conflicts, not the transitive set, we also require the full context
of all previous patches.

The markup presents each /transitive/ unresolved conflict in the form of a
set of alternative changes that all apply at the end of the repo. These
alternatives form the vertices of an undirected graph, where an edge exists
between two vertices iff they conflict. We represent this graph as a list of
connected 'Component's; thus each 'Component' represents one transitive
conflict.

The graph is constructed by commuting any patch that is part of a conflict
to the head. If that succeeds, the resulting conflictor gives us all
participents of the (direct) conflict in the form of contexted patches that
apply to the end of the repo. We check if there is an overlap between this
set and any already constructed components. If this is the case, we join
them into a larger component, otherwise we add a new component. If commuting
to the head fails, we only remember the set of conflicting patch names, and
use that afterwards to connect components that might otherwise appear as
unconnected. The docs for 'findComponents' explain this in greater detail.

Each resulting 'Component' is then converted to a set of plain prim 'FL's
(removing the prim patch IDs) and passed to the mangling function to
calculate the conflict markup as a single prim patch.

The result differs from that for RepoPatchV1 in that we do not merge the
maximal independent (i.e. non-conflicting) sets for each component. While
the latter gives a theoretically valid and more compact presentation,
typically with fewer alternatives, it has some disadvantages in practice:

  * Merging means that a single alternative no longer corresponds to a
    single named patch in our repo. Thus, even if we annotate alternatives
    with patch names or hashes (as planned for V3), identifying which part
    of an alternative belongs to which named patch requires additional
    mental effort during manual resolution.

  * The same original prim is now contained in more than one alternative,
    making it harder to manually resolve the conflict in a systematic way by
    applying difference between alternatives and the baseline step by step.

-}

instance (SignedId name, StorableId name, PrimPatch prim) =>
         Conflict (RepoPatchV3 name prim) where
  numConflicts (Conflictor _ x _) = S.size x
  numConflicts Prim{} = 0
  resolveConflicts context =
      map resolveOne . conflictingAlternatives context
    where
      resolveOne = mangleOrFail . map (mapSeal (mapFL_FL wnPatch))

conflictingAlternatives
  :: (SignedId name, StorableId name, PrimPatch prim)
  => RL (RepoPatchV3 name prim) wO wX
  -> RL (RepoPatchV3 name prim) wX wY
  -> [[Sealed (FL (PrimWithName name prim) wY)]]
conflictingAlternatives context =
  map (map ctxToFL . S.toList) . findComponents context

-- | A connected component of the conflict graph.
type Component name prim wY = S.Set (Contexted (PrimWithName name prim) wY)

{- | Construct the conflict graph by searching the history for unresolved
conflicts. The history is split into an initial 'RL' of patches (the
context) and a trailing 'RL' of patches we are interested in.

We examine patches starting with the head and going backwards, maintaining
the following state:

  @done@

    A list of 'Component's, initially empty, which will become the resulting
    conflict graph.

  @todo@

    A set of @name@s, initially empty, that are candidates for inspection,
    in addition to conflicted patches in the trailing 'RL'. We maintain the
    invariant that this set never contains the @name@ of any patch we have
    already traversed.

  @res@

    A list of two-element sets of @name@s, representing resolved direct
    conflicts. Used to post process the result (see below).

We inspect any conflictor in the trailing 'RL', as well as any patch whose
@name@ is in @todo@ throughout the history, terminating early if the
trailing 'RL' and @todo@ are both exhausted.

For each such candidate we first try to commute it to the head.

If that succeeds, then its commuted version must be a conflictor. (Either it
was a conflictor to begin with, in which case it remains one; or it is a
patch that a later conflictor conflicted with, and that means it must itself
become conflicted when commuted to the head.) The contexted patch that the
(commuted) conflictor represents, together with its set of conflicts, is
either added as a new component to @done@, or else is joined with some
already found component.

If the commute does not succeed, then this indicates that some later patch
has resolved (parts of) the conflict. So this patch makes no direct
contribution to the confict graph. However, it may still be part of a larger
transitive conflict and not all patches involved may have been fully
resolved. (Remember that the commute rules for V3 are such that a patch
depends on a conflictor if it depends on /any/ of the patches involved in
the conflict.) To make sure that the result is independent of the order of
patches, we need to remember all direct conflicts that the patch is part of
(by adding them to @res@). When the traversal terminates, we use this
information to join any components connected by these sets into larger
components. See the discussion below for details.

In both cases, if the patch is conflicted, we insert any patch that the
candidate conflicts with into @todo@ (and remove the patch itself). Note
that in order to maintain our invariant, we must extract the set of
conflicts from the patch /in its uncommuted form/. (If we took them from the
commuted version, then we might mark patches that we already traversed.)

The necessity to remember information about all direct conflicts until the
end, regardless of partial resolutions, can be seen with the following
example. Suppose we have patches A;B;C;D;E where E (a partial resolution)
depends (only) on C, and we have direct conflicts A/C, C/B, B/D. So D
commutes past E but conflicts only indirectly with C. Thus when we encounter
C and fail to remember the fact that it conflicts with A, we end up with two
components [{C,B},{A,D}] instead of a single transitive conflict [{A,B,C,D}]
that we would get when examining the patches in the order A;C;B;D;E.

On the other hand, suppose we have A;B;C;D;E;F, where A;B;C;D;E form a
transitive conflict chain (i.e. we have direct conflicts A/B, B/C, C/D,
D/E), and the partial resolution F depends on {B,C,D}. Note that /all/
direct conflicts involving C are resolved by F, so we expect to get the two
components {A,B} and {D,E}. Indeed, suppose the order were B;D;C;F;A;E, then
at the point after F is added we have a repo with all conflicts resolved.
Thus after adding A and E we should only see the direct conflicts of A and
E, i.e. A, B, D, and E. The slightly subtle implication is that when we
finally join components, we must do so for one conflict set at a time; it
would be wrong to first join conflict sets and then use those to join
components, since that would join {A,B} and {D,E}, even though there is no
conflict between the two sets.
-}
findComponents
  :: forall name prim wO wX wY
   . (SignedId name, StorableId name, PrimPatch prim)
  => RL (RepoPatchV3 name prim) wO wX
  -> RL (RepoPatchV3 name prim) wX wY
  -> [Component name prim wY]
findComponents context patches = go S.empty [] [] context patches NilFL where
  go :: S.Set name
     -> [Component name prim wY]
     -> [S.Set name]
     -> RL (RepoPatchV3 name prim) wO wA
     -> RL (RepoPatchV3 name prim) wA wB
     -> FL (RepoPatchV3 name prim) wB wY
     -> [Component name prim wY]
  go todo done res cs (ps :<: p) passedby
    | isConflicted p || ident p `S.member` todo
    , Just (_ :> p') <- commuteFL (p :> passedby) =
        go todo' (updDone p' done) res cs ps (p :>: passedby)
    | otherwise =
        go todo' done (updRes p res) cs ps (p :>: passedby)
    where
      todo' = S.map ctxId (conflicts p) <> (ident p -| todo)
  go todo done res _ NilRL _
    | S.null todo = sort $ map purgeDeps $ foldr joinOverlapping done res
  go todo done res (cs :<: p) NilRL passedby
    | ident p `S.member` todo
    , Just (_ :> p') <- commuteFL (p :> passedby) =
        go todo' (updDone p' done) res cs NilRL (p :>: passedby)
    | otherwise =
        go todo' done (updRes p res) cs NilRL (p :>: passedby)
    where
      todo' = ident p -| todo
  go _ _ _ NilRL NilRL _ = error "autsch, hit the bottom"

  updDone p' done = joinOrAddNew (allConflicts p') done

  -- Update the list of resolved direct conflicts, to be used in the last step
  -- to join unresolved transitive conflict sets. Note that this list contains
  -- only pairs i.e. two-element sets: for each conflicted patch p, we pair it
  -- with any patch it conflicts with. See the discussion of the algorithm
  -- above and tests/issue2727-resolutions-order-independent10.sh for an
  -- example case, found by QuickCheck.
  updRes p res = map (ident p `pair`) (conflictIds p) ++ res
    where
      pair a b = S.fromList [a, b]
      conflictIds = S.toList . S.map ctxId . conflicts

  conflicts (Conflictor _ x _) = x
  conflicts _ = S.empty

  allConflicts (Conflictor _ x cp) = cp +| x
  allConflicts _ = S.empty

  -- Join all components which overlap with the given set of IDs
  joinOverlapping ids cs =
    case partition (not . S.disjoint ids . S.map ctxId) cs of
      ([], to_keep) -> to_keep -- avoid adding empty components
      (to_join, to_keep) -> S.unions to_join : to_keep

  -- remove vertices that others depend on
  purgeDeps :: Component name prim wY -> Component name prim wY
  purgeDeps c = S.filter (\a -> not $ any (a `ctxDepends`) (a -| c)) c

-- | Add a set to a disjoint list of sets, such that we maintain the invariant
-- that the resulting list of sets is disjoint, and such that their unions are
-- equal to the unions of the inputs.
--
-- The tricky point here is that the new set may overlap with any number of
-- list elements; we must ensure they are all joined into a single set.
joinOrAddNew :: Ord a => S.Set a -> [S.Set a] -> [S.Set a]
joinOrAddNew c [] = [c]
joinOrAddNew c (d:ds)
  | not $ all (S.disjoint d) ds = error "precondition: sets are not disjoint"
  | c `S.disjoint` d = d : joinOrAddNew c ds
  | otherwise = joinOrAddNew (c `S.union` d) ds
