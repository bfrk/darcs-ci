{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TupleSections #-}

{- | Some of the algorithms in this module do complex recursive operations
on sequences of patches in order to simplify them. These algorithms require
that we know whether some intermediate step has made any progress. If not,
we want to terminate or try something different.

We capture this as an effect by tagging intermediate data with the 'Any'
monoid, a newtype wrapper for 'Bool' with disjunction as 'mappend'. The
standard @instance 'Monoid' a => 'Monad' (a,)'@ defined in the base package
then gives use the desired semantics. That is, when we sequence operations
using '>>=', the result tells us whether 'Any' of the two operations have
made progress. -}

module Darcs.Patch.Prim.V1.Coalesce
    ()
    where

import Darcs.Prelude

import qualified Data.Map as M
import Data.Maybe ( fromMaybe )
import Data.Monoid ( Any(..) )

import qualified Data.ByteString as B ( ByteString )

import System.FilePath ( (</>) )

import Darcs.Patch.Prim.Class ( PrimCoalesce(..) )
import Darcs.Patch.Prim.V1.Commute ()
import Darcs.Patch.Prim.V1.Core
    ( Prim(..), FilePatchType(..), DirPatchType(..)
    , comparePrim, isIdentity
    )
import Darcs.Patch.Prim.V1.Show ()
import Darcs.Patch.Witnesses.Eq ( Eq2(..), EqCheck(..) )
import Darcs.Patch.Witnesses.Ordered ( FL(..), (:>)(..), mapFL, concatFL )
import Darcs.Patch.Witnesses.Sealed ( Sealed2(..), unseal2 )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP )
import Darcs.Patch.Invert ( Invert(..) )
import Darcs.Patch.Commute ( Commute(..) )

import Darcs.Util.Global ( darcsdir )
import Darcs.Util.Path ( AnchoredPath, unsafeFloatPath )

-- | Conversion between @('Any', a)@ and @'Maybe' a@.
withAnyToMaybe :: (Any, a) -> Maybe a
withAnyToMaybe (Any True, x) = Just x
withAnyToMaybe (Any False, _) = Nothing

-- | Map a monadic function over an 'FL' of 'Prim's.
--
-- Be careful which 'Monad' to choose when using this function. For instance,
-- 'Maybe' would return 'Nothing' if any of the calls failed to shrink their
-- argument, which usually not what we want. A suitable candidate is @('Any',)@.
mapPrimFL :: Monad m
          => (forall wA wB . FL Prim wA wB -> m (FL Prim wA wB))
          -> FL Prim wX wY -> m (FL Prim wX wY)
mapPrimFL f ps =
  -- an optimisation; break the list up into independent sublists
  -- and apply f to each of them
  case mapM withPathAsKey $ mapFL Sealed2 ps of
    Just pairs ->
      concatFL .
      unsealList .
      M.elems <$>
      (mapM (fmap Sealed2 . f . unsealList . ($ [])) $
      M.fromListWith (flip (.)) $ map (\(k, v) -> (k, (v :))) pairs)
    Nothing -> f ps
  where
    unsealList :: [Sealed2 p] -> FL p wA wB
    unsealList = foldr ((:>:) . unseal2 unsafeCoerceP) (unsafeCoerceP NilFL)

    withPathAsKey :: Sealed2 Prim -> Maybe (AnchoredPath, Sealed2 Prim)
    withPathAsKey (Sealed2 p) = fmap (, Sealed2 p) $ getKey p

    getKey (FP fp _) = Just fp
    getKey (DP fp AddDir) = Just fp
    getKey (DP _ RmDir) = Nothing -- ordering is trickier with rmdir present
    getKey (Move {}) = Nothing
    getKey (ChangePref {}) = Just (unsafeFloatPath (darcsdir </> "prefs" </> "prefs"))

-- | Try to coalesce a patch with any element of an adjacent sequence,
-- regardless of ordering. If successful, the result may not be
-- sorted, even if the input was.
shrinkOne :: Prim wA wB -> FL Prim wB wC -> Maybe (FL Prim wA wC)
shrinkOne _ NilFL = Nothing
shrinkOne a (b :>: bs) =
  case coalesceOrCancel a b of
    Just NilFL -> Just bs
    Just (ab :>: NilFL) ->
      Just $ fromMaybe (ab :>: bs) $ shrinkOne ab bs
    Just _ -> error "postcondition of coalesceOrCancel"
    Nothing -> do
      b' :> a' <- commute (a :> b)
      (b':>:) <$> shrinkOne a' bs

-- | The heart of 'sortCoalesceFL'.
sortCoalesceFL2 :: FL Prim wX wY -> (Any, FL Prim wX wY)
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
pushCoalescePatch :: Prim wX wY -> FL Prim wY wZ -> (Any, FL Prim wX wZ)
pushCoalescePatch new NilFL = (Any False, new:>:NilFL)
pushCoalescePatch new ps@(p :>: ps') =
  case coalesceOrCancel new p of
    Just (new' :>: NilFL) -> (Any True, snd $ pushCoalescePatch new' ps')
    Just NilFL -> (Any True, ps')
    Just _ -> error "postcondition of coalesceOrCancel"
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

-- | Cancel patches if they are inverses of each other, or try to coalesce
-- them.
coalesceOrCancel :: Prim wX wY -> Prim wY wZ -> Maybe (FL Prim wX wZ)
coalesceOrCancel p1 p2
  | IsEq <- invert p1 =\/= p2 = Just NilFL
  | otherwise = fmap (:>: NilFL) $ coalescePair p1 p2

-- | @'coalescePair' p1 p2@ tries to combine @p1@ and @p2@ into a single
--   patch. For example, two hunk patches
--   modifying adjacent lines can be coalesced into a bigger hunk patch.
--   Or a patch which moves file A to file B can be coalesced with a
--   patch that moves file B into file C, yielding a patch that moves
--   file A to file C.
coalescePair :: Prim wX wY -> Prim wY wZ -> Maybe (Prim wX wZ)
coalescePair (FP f1 p1) (FP f2 p2)
  | f1 /= f2 = Nothing
  | otherwise = coalesceFilePrim f1 p1 p2
coalescePair (Move a b) (Move b' c) | b == b' = Just $ Move a c
coalescePair (FP a AddFile) (Move a' b) | a == a' = Just $ FP b AddFile
coalescePair (DP a AddDir) (Move a' b) | a == a' = Just $ DP b AddDir
coalescePair (Move a b) (FP b' RmFile) | b == b' = Just $ FP a RmFile
coalescePair (Move a b) (DP b' RmDir) | b == b' = Just $ DP a RmDir
{- we don't want to do that, of course:
coalescePair (FP a RmFile) (FP b AddFile) | a == a' = Just $ Move a' b
coalescePair (DP a RmDir) (DP b AddDir) | a == a' = Just $ Move a' b
-}
coalescePair (ChangePref p a b) (ChangePref p' b' c)
  | p == p' && b == b' = Just $ ChangePref p a c
coalescePair _ _ = Nothing

-- | If 'coalescePair' is "addition" then this is "subtraction".
decoalescePair :: Prim wX wZ -> Prim wX wY -> Maybe (Prim wY wZ)
-- These two cases make sense only if we decoalesce;
-- they correspond to the commented two cases for coalesce above
-- and are one reason we need to define this function as a primitive
decoalescePair (Move a b) (FP b' AddFile) | b == b' = Just (FP a RmFile)
decoalescePair (Move a b) (DP b' AddDir) | b == b' = Just (DP a RmDir)
decoalescePair (FP f1 p1) (FP f2 p2)
  | f1 /= f2 = Nothing
  | otherwise = decoalesceFilePrim f1 p1 p2
decoalescePair z x = coalescePair (invert x) z

coalesceFilePrim :: AnchoredPath -> FilePatchType wX wY -> FilePatchType wY wZ
                 -> Maybe (Prim wX wZ)
coalesceFilePrim f (Hunk line1 old1 new1) (Hunk line2 old2 new2)
    = coalesceHunk f line1 old1 new1 line2 old2 new2
-- Token replace patches operating right after (or before) AddFile (RmFile)
-- is an identity patch, as far as coalescing is concerned.
-- These two cases make no sense when we decoalesce, which is the second
-- reason decoalesce is defined as a primitive.
coalesceFilePrim f (AddFile) (TokReplace{}) = Just $ FP f AddFile
coalesceFilePrim f (TokReplace{}) (RmFile) = Just $ FP f RmFile
coalesceFilePrim f (TokReplace t1 a b) (TokReplace t2 b' c)
    | t1 == t2 && b == b' = Just $ FP f $ TokReplace t1 a c
coalesceFilePrim f (Binary o m') (Binary m n)
    | m == m' = Just $ FP f $ Binary o n
coalesceFilePrim _ _ _ = Nothing

decoalesceFilePrim :: AnchoredPath -> FilePatchType wX wZ -> FilePatchType wX wY
                   -> Maybe (Prim wY wZ)
-- These two cases must fail because the token replace patches that coalesce
-- has eliminated are irretrievably lost.
decoalesceFilePrim _ (AddFile) (RmFile) = Nothing
decoalesceFilePrim _ (RmFile) (TokReplace{}) = Nothing
decoalesceFilePrim f z x = coalesceFilePrim f (invert x) z

coalesceHunk :: AnchoredPath
             -> Int -> [B.ByteString] -> [B.ByteString]
             -> Int -> [B.ByteString] -> [B.ByteString]
             -> Maybe (Prim wX wY)
coalesceHunk f line1 old1 new1 line2 old2 new2
    | line2 == line1 && lengthold2 < lengthnew1 =
        if take lengthold2 new1 /= old2
        then Nothing
        else case drop lengthold2 new1 of
        extranew -> Just (FP f (Hunk line2 old1 (new2 ++ extranew)))
    | line2 == line1 && lengthold2 > lengthnew1 =
        if take lengthnew1 old2 /= new1
        then Nothing
        else case drop lengthnew1 old2 of
        extraold -> Just (FP f (Hunk line2 (old1 ++ extraold) new2))
    | line2 == line1 = if new1 == old2 then Just (FP f (Hunk line2 old1 new2))
                       else Nothing
    | line2 < line1 && lengthold2 >= line1 - line2 =
        case take (line1 - line2) old2 of
        extra-> coalesceHunk f line2 (extra ++ old1) (extra ++ new1) line2 old2 new2
    | line2 > line1 && lengthnew1 >= line2 - line1 =
        case take (line2 - line1) new1 of
        extra-> coalesceHunk f line1 old1 new1 line1 (extra ++ old2) (extra ++ new2)
    | otherwise = Nothing
    where lengthold2 = length old2
          lengthnew1 = length new1

instance PrimCoalesce Prim where
   tryToShrink = withAnyToMaybe . mapPrimFL sortCoalesceFL2
   sortCoalesceFL = snd . mapPrimFL sortCoalesceFL2
   coalesce (p1 :> p2) = coalesceOrCancel p1 p2
   primCoalesce = coalescePair
   primDecoalesce = decoalescePair
