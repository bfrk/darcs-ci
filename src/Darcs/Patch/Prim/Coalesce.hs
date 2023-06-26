{- | Generic coalesce functions

Some of the algorithms in this module do complex recursive operations on
sequences of patches in order to simplify them. These algorithms require
that we know whether some intermediate step has made any progress. If not,
we want to terminate or try something different.

We capture this as an effect by tagging intermediate data with the 'Any'
monoid, a newtype wrapper for 'Bool' with disjunction as 'mappend'. The
standard @instance 'Monoid' a => 'Monad' (a,)'@ defined in the base package
then gives use the desired semantics. That is, when we sequence operations
using '>>=', the result tells us whether 'Any' of the two operations have
made progress. -}

module Darcs.Patch.Prim.Coalesce
    ( coalesce
    , defaultTryToShrink
    , defaultSortCoalesceFL
    , withAnyToMaybe
    , sortCoalesceFL2
    ) where

import Darcs.Prelude

import Data.Maybe ( fromMaybe )
import Data.Monoid ( Any(..) )

import Darcs.Patch.Commute ( Commute(..) )
import Darcs.Patch.Invert ( Invert(..) )
import Darcs.Patch.Prim.Class ( PrimCoalesce(..), isIdentity)
import Darcs.Patch.Witnesses.Eq ( Eq2(..), EqCheck(..) )
import Darcs.Patch.Witnesses.Maybe ( Maybe2(..) )
import Darcs.Patch.Witnesses.Ordered ( FL(..), (:>)(..) )

-- | Either 'primCoalesce' or cancel inverses.
--
-- prop> primCoalesce (p :> q) == Just r => apply r = apply p >> apply q
coalesce :: PrimCoalesce prim => (prim :> prim) wX wY -> Maybe (Maybe2 prim wX wY)
coalesce (p1 :> p2)
  | IsEq <- invert p1 =\/= p2 = Just Nothing2
  | otherwise = Just2 <$> primCoalesce p1 p2

defaultTryToShrink :: PrimCoalesce prim => FL prim wX wY -> Maybe (FL prim wX wY)
defaultTryToShrink = withAnyToMaybe . sortCoalesceFL2

defaultSortCoalesceFL :: PrimCoalesce prim => FL prim wX wY -> FL prim wX wY
defaultSortCoalesceFL = snd . sortCoalesceFL2

-- | Conversion between @('Any', a)@ and @'Maybe' a@.
withAnyToMaybe :: (Any, a) -> Maybe a
withAnyToMaybe (Any True, x) = Just x
withAnyToMaybe (Any False, _) = Nothing

-- | The heart of 'sortCoalesceFL'.
sortCoalesceFL2 :: PrimCoalesce prim => FL prim wX wY -> (Any, FL prim wX wY)
sortCoalesceFL2 NilFL = (Any False, NilFL)
sortCoalesceFL2 (x:>:xs) = do
  xs' <- sortCoalesceFL2 xs
  case isIdentity x of
    IsEq -> (Any True, xs')
    NotEq -> pushCoalescePatch x xs'

-- | Try to coalesce the patch with any of the elements in the sequence,
-- using commutation to push it down the list, until either
--
--  (1) @new@ is 'LT' the next member of the list (using 'comparePrim')
-- 
--  (2) commutation fails or
-- 
--  (3) coalescing succeeds.
--
-- In case (1) we push the patch further, trying to coalesce it with any of its
-- successors and disregarding any ordering. If this is successful, we recurse
-- with the result, otherwise we leave the patch where it was, so the sequence
-- remains sorted.
--
-- In case (3) we recursively continue with the result unless that is empty.
-- 
-- The result is returned in the @('Any',)@ monad to indicate whether it was
-- able to shrink the patch sequence. To make this clear, we do /not/ track
-- whether sorting has made progress, only shrinking.
--
-- The precondition is that the input sequence is already sorted.
pushCoalescePatch
  :: forall prim wX wY wZ
   . PrimCoalesce prim
  => prim wX wY
  -> FL prim wY wZ
  -> (Any, FL prim wX wZ)
pushCoalescePatch new NilFL = (Any False, new:>:NilFL)
pushCoalescePatch new ps@(p :>: ps') =
  case coalesce (new :> p) of
    Just (Just2 new') -> (Any True, snd $ pushCoalescePatch new' ps')
    Just Nothing2 -> (Any True, ps')
    Nothing ->
      case comparePrim new p of
        LT ->
          case shrinkOne new ps of
            Just ps'' ->
              -- we have to start over here because shrinkOne may have
              -- destroyed the order
              sortCoalesceFL2 ps''
            Nothing -> (Any False, new :>: ps)
        _ ->
          case commute (new :> p) of
            Just (p' :> new') ->
              case pushCoalescePatch new' ps' of
                (Any True, r) -> (Any True, snd $ pushCoalescePatch p' r)
                (Any False, r) -> (Any False, p' :>: r)
            Nothing -> (Any False, new :>: ps)
  where
    -- Try to coalesce a patch with any element of an adjacent sequence,
    -- regardless of ordering. If successful, the result may not be
    -- sorted, even if the input was.
    shrinkOne :: prim wA wB -> FL prim wB wC -> Maybe (FL prim wA wC)
    shrinkOne _ NilFL = Nothing
    shrinkOne a (b :>: bs) =
      case coalesce (a :> b) of
        Just Nothing2 -> Just bs
        Just (Just2 ab) -> Just $ fromMaybe (ab :>: bs) $ shrinkOne ab bs
        Nothing -> do
          b' :> a' <- commute (a :> b)
          (b' :>:) <$> shrinkOne a' bs
