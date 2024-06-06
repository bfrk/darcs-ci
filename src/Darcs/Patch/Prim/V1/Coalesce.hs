{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TupleSections #-}

module Darcs.Patch.Prim.V1.Coalesce
    ()
    where

import Darcs.Prelude

import qualified Data.Map as M

import qualified Data.ByteString as B ( ByteString )

import System.FilePath ( (</>) )

import Darcs.Patch.Prim.Class ( PrimCoalesce(..) )
import Darcs.Patch.Prim.Coalesce
import Darcs.Patch.Prim.V1.Commute ()
import Darcs.Patch.Prim.V1.Core ( DirPatchType(..), FilePatchType(..), Prim(..) )
import Darcs.Patch.Prim.V1.Show ()
import Darcs.Patch.Witnesses.Eq ( EqCheck(..) )
import Darcs.Patch.Witnesses.Ordered ( FL(..), concatFL, mapFL )
import Darcs.Patch.Witnesses.Sealed ( Sealed2(..), unseal2 )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP )

import Darcs.Util.Global ( darcsdir )
import Darcs.Util.Path ( AnchoredPath, unsafeFloatPath )

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
coalescePair (DP a AddDir) (Move a' b)  | a == a' = Just $ DP b AddDir
coalescePair (Move a b) (FP b' RmFile)  | b == b' = Just $ FP a RmFile
coalescePair (Move a b) (DP b' RmDir)   | b == b' = Just $ DP a RmDir
coalescePair (ChangePref p a b) (ChangePref p' b' c)
  | p == p' && b == b' = Just $ ChangePref p a c
coalescePair _ _ = Nothing

coalesceFilePrim :: AnchoredPath -> FilePatchType wX wY -> FilePatchType wY wZ
                 -> Maybe (Prim wX wZ)
coalesceFilePrim f (Hunk line1 old1 new1) (Hunk line2 old2 new2)
    = coalesceHunk f line1 old1 new1 line2 old2 new2
-- Token replace patches operating right after AddFile or before RmFile
-- is an identity patch, as far as coalescing is concerned.
coalesceFilePrim f (AddFile) (TokReplace{}) = Just $ FP f AddFile
coalesceFilePrim f (TokReplace{}) (RmFile) = Just $ FP f RmFile
coalesceFilePrim f (TokReplace t1 a b) (TokReplace t2 b' c)
    | t1 == t2 && b == b' = Just $ FP f $ TokReplace t1 a c
coalesceFilePrim f (Binary o m') (Binary m n)
    | m == m' = Just $ FP f $ Binary o n
coalesceFilePrim _ _ _ = Nothing

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

  primCoalesce = coalescePair

  isIdentity (FP _ (Binary old new)) | old == new = unsafeCoerceP IsEq
  isIdentity (FP _ (Hunk _ old new)) | old == new = unsafeCoerceP IsEq
  isIdentity (FP _ (TokReplace _ old new)) | old == new = unsafeCoerceP IsEq
  isIdentity (Move old new) | old == new = unsafeCoerceP IsEq
  isIdentity _ = NotEq

  -- Basically, identical patches are equal and
  -- @Move < DP < FP < ChangePref@.
  -- Everything else is compared in dictionary order of its arguments.
  comparePrim (Move a b) (Move c d) = compare (a, b) (c, d)
  comparePrim (Move _ _) _ = LT
  comparePrim _ (Move _ _) = GT
  comparePrim (DP d1 p1) (DP d2 p2) = compare (d1, p1) $ unsafeCoerceP (d2, p2)
  comparePrim (DP _ _) _ = LT
  comparePrim _ (DP _ _) = GT
  comparePrim (FP f1 fp1) (FP f2 fp2) =
    compare (f1, fp1) $ unsafeCoerceP (f2, fp2)
  comparePrim (FP _ _) _ = LT
  comparePrim _ (FP _ _) = GT
  comparePrim (ChangePref a1 b1 c1) (ChangePref a2 b2 c2) =
    compare (c1, b1, a1) (c2, b2, a2)
