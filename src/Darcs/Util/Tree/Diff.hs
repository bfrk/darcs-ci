module Darcs.Util.Tree.Diff
  ( TreeDiff(..)
  , getTreeDiff
  , organise
  ) where

import Darcs.Prelude
import Darcs.Util.Tree (TreeItem(..))
import Darcs.Util.Path (AnchoredPath)

-- TODO this is isomorphic to These (TreeItem m)
data TreeDiff m
  = Added (TreeItem m)
  | Removed (TreeItem m)
  | Changed (TreeItem m)
            (TreeItem m)

getTreeDiff
  :: AnchoredPath
  -> Maybe (TreeItem m)
  -> Maybe (TreeItem m)
  -> (AnchoredPath, TreeDiff m)
getTreeDiff p Nothing (Just t) = (p, Added t)
getTreeDiff p (Just from) (Just to) = (p, Changed from to)
getTreeDiff p (Just t) Nothing = (p, Removed t)
getTreeDiff _ Nothing Nothing = error "impossible case" -- zipTrees should never return this

-- sort into removes, changes, adds, with removes in reverse-path order
-- and everything else in forward order
organise :: (AnchoredPath, TreeDiff m) -> (AnchoredPath, TreeDiff m) -> Ordering
organise (p1, Changed _ _) (p2, Changed _ _) = compare p1 p2
organise (p1, Added _) (p2, Added _) = compare p1 p2
organise (p1, Removed _) (p2, Removed _) = compare p2 p1
organise (_, Removed _) _ = LT
organise _ (_, Removed _) = GT
organise (_, Changed _ _) _ = LT
organise _ (_, Changed _ _) = GT
