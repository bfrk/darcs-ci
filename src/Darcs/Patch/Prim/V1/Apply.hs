{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiWayIf #-}
module Darcs.Patch.Prim.V1.Apply () where

import Darcs.Prelude

import Control.Monad ( (>=>) )
import Control.Monad.Catch ( MonadThrow(throwM) )
import qualified Data.ByteString as B ( ByteString, empty, null )

import Darcs.Patch.Apply ( Apply(..) )
import Darcs.Patch.ApplyMonad ( ApplyMonad(..), ApplyMonadTree(..) )
import Darcs.Patch.Prim.Class ( PrimApply(..) )
import Darcs.Patch.Prim.V1.Core ( Prim(..), TreePatchType(..) )
import Darcs.Patch.Prim.V1.FilePatch
    ( FilePatchType(..)
    , applyFilePatch
    , showApplyFilePatchError
    )
import Darcs.Patch.Repair ( RepairToFL(..), mapMaybeSnd )
import Darcs.Patch.Witnesses.Ordered ( FL(..), mapFL_FL, spanFL, (:>)(..) )
import Darcs.Util.Path ( AnchoredPath, displayPath )
import Darcs.Util.Printer ( renderString )
import Darcs.Util.Tree ( Tree )

instance Apply Prim where
    type ApplyState Prim = Tree
    apply (TP p) = apply p
    apply (FP f p) = mModifyFilePS f $ applyFP f p
    apply (CP p f t) = mChangePref p f t

instance RepairToFL Prim where
    applyAndTryToFixFL (TP (RmFile f)) = do
      x <- mReadFilePS f
      mRemoveFile f
      return $
        if B.null x
          then Nothing
          else Just
                 ( "WARNING: Fixing removal of non-empty file " ++ displayPath f
                 , FP f (Binary x B.empty) :>: TP (RmFile f) :>: NilFL
                 )
    applyAndTryToFixFL (TP p) = mapMaybeSnd (mapFL_FL TP) <$> applyAndTryToFixFL p
    applyAndTryToFixFL (FP f (Binary old new)) = do
      x <- mReadFilePS f
      mModifyFilePS f (\_ -> return new)
      return $
        if x /= old
          then Just
                 ( "WARNING: Fixing binary patch to " ++ displayPath f
                 , FP f (Binary x new) :>: NilFL
                 )
          else Nothing
    applyAndTryToFixFL p = apply p >> return Nothing

instance PrimApply Prim where
    applyPrimFL = applyFL

-- | Apply adjacent 'FP' type patches to the same file as a batch.
applyFL :: ApplyMonad Tree m => FL Prim wX wY -> m ()
applyFL NilFL = return ()
applyFL (p@(FP f _) :>: ps) =
  case spanFL fp_to_same_file ps of
    (xs :> ps') -> do
      mModifyFilePS f $ applyFPs f $ mapFL_FL from_fp (p :>: xs)
      applyPrimFL ps'
  where
    fp_to_same_file (FP f' _) = f == f'
    fp_to_same_file _ = False
    from_fp (FP _ fp) = fp
    from_fp _ = error "postcondition of spanFL"
applyFL (p :>: ps) = apply p >> applyFL ps

applyFPs
  :: MonadThrow m
  => AnchoredPath
  -> FL FilePatchType wX wY
  -> B.ByteString
  -> m B.ByteString
applyFPs _ NilFL = return
applyFPs f (p :>: ps) = applyFP f p >=> applyFPs f ps

applyFP
  :: MonadThrow m
  => AnchoredPath
  -> FilePatchType wX wY
  -> B.ByteString
  -> m B.ByteString
applyFP f p fc = do
  case applyFilePatch p fc of
    Left e -> throwM $ userError $ renderString $ showApplyFilePatchError f e
    Right fc' -> return fc'
