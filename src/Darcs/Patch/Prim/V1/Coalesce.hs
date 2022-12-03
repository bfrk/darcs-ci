{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TupleSections #-}

module Darcs.Patch.Prim.V1.Coalesce
    ()
    where

import Darcs.Prelude

import qualified Data.Map as M

import qualified Data.ByteString as B ( ByteString )

import System.FilePath ( (</>) )

import Darcs.Patch.Inspect ( PatchInspect(..) )
import Darcs.Patch.Prim.Class ( PrimCoalesce(..) )
import Darcs.Patch.Prim.Coalesce
import Darcs.Patch.Prim.V1.Core ( TreePatchType(..), FilePatchType(..), Prim(..) )
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
    getKey (TP (RmDir _)) = Nothing -- ordering is trickier with rmdir present
    getKey (TP tp) = case listTouchedFiles tp of [fp] -> Just fp; _ -> Nothing
    getKey (CP {}) = Just (unsafeFloatPath (darcsdir </> "prefs" </> "prefs"))

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
coalescePair (TP (Move a b)) (TP (Move b' c))  | b == b' = Just $ TP $ Move a c
coalescePair (TP (AddFile a)) (TP (Move a' b)) | a == a' = Just $ TP $ AddFile b
coalescePair (TP (AddDir a)) (TP (Move a' b))  | a == a' = Just $ TP $ AddDir b
coalescePair (TP (Move a b)) (TP (RmFile b'))  | b == b' = Just $ TP $ RmFile a
coalescePair (TP (Move a b)) (TP (RmDir b'))   | b == b' = Just $ TP $ RmDir a
-- Token replace patches operating right after AddFile or before RmFile
-- is an identity patch, as far as coalescing is concerned.
coalescePair (TP (AddFile f)) (FP f' (TokReplace{})) | f == f' = Just $ TP $ AddFile f
coalescePair (FP f (TokReplace{})) (TP (RmFile f'))  | f == f' = Just $ TP $ RmFile f
coalescePair (CP p a b) (CP p' b' c)
  | p == p' && b == b' = Just $ CP p a c
coalescePair _ _ = Nothing

coalesceFilePrim :: AnchoredPath -> FilePatchType wX wY -> FilePatchType wY wZ
                 -> Maybe (Prim wX wZ)
coalesceFilePrim f (Hunk line1 old1 new1) (Hunk line2 old2 new2)
    = coalesceHunk f line1 old1 new1 line2 old2 new2
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
  isIdentity (TP (Move old new)) | old == new = unsafeCoerceP IsEq
  isIdentity _ = NotEq

  comparePrim p1 p2 = compare p1 (unsafeCoerceP p2)
