{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Darcs.Patch.Prim.V1.TreePatch
    ( TreePatchType(..)
    , orIfEq
    ) where

import Darcs.Prelude

import Darcs.Patch.Apply ( Apply(..) )
import Darcs.Patch.ApplyMonad ( ApplyMonadTree(..) )
import Darcs.Patch.Commute ( Commute(..), invertCommute, trivialCommute )
import Darcs.Patch.Inspect ( PatchInspect(..) )
import Darcs.Patch.Invert ( Invert(..) )
import Darcs.Patch.Prim.Class ( PrimDetails(..), PrimRead(..), PrimShow(..) )
import Darcs.Patch.Read ( readFileName )
import Darcs.Patch.Repair ( RepairToFL(..) )
import Darcs.Patch.Show ( formatFileName )
import Darcs.Patch.SummaryData ( SummDetail(..), SummOp(..) )
import Darcs.Patch.Witnesses.Eq ( Eq2(..) )
import Darcs.Patch.Witnesses.Ordered ( FL(..), (:>)(..) )
import Darcs.Patch.Witnesses.Sealed ( seal )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP, unsafeCoercePStart )
import Darcs.Util.Parser ( choice, skipSpace, string )
import Darcs.Util.Path ( AnchoredPath, displayPath, isPrefix, movedirfilename )
import Darcs.Util.Printer ( blueText, (<+>) )
import Darcs.Util.Tree ( Tree )


type role TreePatchType nominal nominal

-- | Change of the tree structure
data TreePatchType wX wY
  = Move AnchoredPath AnchoredPath
  | RmDir AnchoredPath
  | AddDir AnchoredPath
  | RmFile AnchoredPath
  | AddFile AnchoredPath
  deriving (Eq, Show)

orIfEq :: Ordering -> Ordering -> Ordering
orIfEq EQ x = x
orIfEq lt_or_gt _ = lt_or_gt

-- | The reason the 'Ord' instances are manually defined is that we have to
-- keep the ordering exactly as it was, because, unfortunately, RepoPatchV2
-- internally uses sortCoalesceFL when commuting conflictors.
instance Ord (TreePatchType wX wY) where
  compare (Move a b) (Move c d) = compare (a, b) (c, d)
  compare (Move _ _) _ = LT
  compare _ (Move _ _) = GT
  compare (RmDir d1) (RmDir d2)   = compare d1 d2
  compare (RmDir d1) (AddDir d2)  = compare d1 d2 `orIfEq` LT
  compare (AddDir d1) (RmDir d2)  = compare d1 d2 `orIfEq` GT
  compare (AddDir d1) (AddDir d2) = compare d1 d2
  compare (AddDir _) _ = LT
  compare (RmDir _) _ = LT
  compare _ (AddDir _) = GT
  compare _ (RmDir _) = GT
  compare (RmFile f1) (RmFile f2)   = compare f1 f2
  compare (RmFile f1) (AddFile f2)  = compare f1 f2 `orIfEq` LT
  compare (AddFile f1) (RmFile f2)  = compare f1 f2 `orIfEq` GT
  compare (AddFile f1) (AddFile f2) = compare f1 f2

instance Eq2 TreePatchType where
  unsafeCompare a b = a == unsafeCoerceP b

instance PrimShow TreePatchType where
  showPrim fmt (Move f f') =
    blueText "move" <+> formatFileName fmt f <+> formatFileName fmt f'
  showPrim fmt (RmDir d) = blueText "rmdir" <+> formatFileName fmt d
  showPrim fmt (AddDir d) = blueText "adddir" <+> formatFileName fmt d
  showPrim fmt (RmFile f) = blueText "rmfile" <+> formatFileName fmt f
  showPrim fmt (AddFile f) = blueText "addfile" <+> formatFileName fmt f

instance PrimRead TreePatchType where
  readPrim fmt = do
    skipSpace
    seal <$>
      choice
        [ string "addfile" >> AddFile <$> readFileName fmt
        , string "adddir" >> AddDir <$> readFileName fmt
        , string "rmfile" >> RmFile <$> readFileName fmt
        , string "rmdir" >> RmDir <$> readFileName fmt
        , string "move" >> Move <$> readFileName fmt <*> readFileName fmt
        ]

instance Invert TreePatchType where
  invert (Move p1 p2) = Move p2 p1
  invert (RmDir p) = AddDir p
  invert (AddDir p) = RmDir p
  invert (RmFile p) = AddFile p
  invert (AddFile p) = RmFile p

instance PatchInspect TreePatchType where
  listTouchedFiles (Move p1 p2) = [p1, p2]
  listTouchedFiles (AddDir p) = [p]
  listTouchedFiles (RmDir p) = [p]
  listTouchedFiles (AddFile p) = [p]
  listTouchedFiles (RmFile p) = [p]
  hunkMatches _ _ = False

instance PrimDetails TreePatchType where
  summarizePrim (Move f1 f2) = [SummMv f1 f2]
  summarizePrim (RmDir d) = [SummRmDir d]
  summarizePrim (AddDir d) = [SummAddDir d]
  summarizePrim (RmFile f) = [SummFile SummRm f 0 0 0]
  summarizePrim (AddFile f) = [SummFile SummAdd f 0 0 0]

instance Apply TreePatchType where
  type ApplyState TreePatchType = Tree
  apply (RmFile f) = mRemoveFile f
  apply (AddFile f) = mCreateFile f
  apply (AddDir d) = mCreateDirectory d
  apply (RmDir d) = mRemoveDirectory d
  apply (Move f f') = mRename f f'

instance RepairToFL TreePatchType where
  applyAndTryToFixFL (AddFile f) = do
    exists <- mDoesFileExist f
    if | exists ->
         return $
         Just
           ( "WARNING: Dropping add of existing file " ++ displayPath f
             -- the old context was wrong, so we have to coerce
           , unsafeCoercePStart NilFL)
       | otherwise ->
         do mCreateFile f
            return Nothing
  applyAndTryToFixFL (AddDir f) = do
    exists <- mDoesDirectoryExist f
    if | exists ->
         return $
         Just
           ( "WARNING: Dropping add of existing directory " ++ displayPath f
             -- the old context was wrong, so we have to coerce
           , unsafeCoercePStart NilFL)
       | otherwise ->
         do mCreateDirectory f
            return Nothing
  applyAndTryToFixFL p@(Move old new) = do
    old_is_file <- mDoesFileExist old
    old_is_dir <- mDoesDirectoryExist old
    new_is_file <- mDoesFileExist new
    new_is_dir <- mDoesDirectoryExist new
    if | not (old_is_file || old_is_dir) ->
         return $
         Just
           ( "WARNING: Dropping move patch with non-existing source " ++
             displayPath old
           , unsafeCoercePStart NilFL
           )
       | new_is_file || new_is_dir ->
         return $
         Just
           ( "WARNING: Dropping move patch with existing target " ++
             displayPath old
           , unsafeCoercePStart NilFL
           )
       | otherwise -> apply p >> return Nothing
  applyAndTryToFixFL p = apply p >> return Nothing

instance Commute TreePatchType where
  commute pair@(p1 :> p2) =
    case (listTouchedFiles p1, listTouchedFiles p2) of
      ([f1],[f2])
        | isPrefix f1 f2 || isPrefix f2 f1 -> Nothing
        | otherwise -> trivialCommute pair
      ([f],[o,n])
        -- note that isPrefix includes path equality
        | isPrefix f o || isPrefix f n -> Nothing
        | otherwise ->
          return (unsafeCoerceP p2 :> unsafeCoerceP (pathMap (movedirfilename o n) p1))
      ([_,_],[_]) -> invertCommute commute pair
      ([o1,n1],[o2,n2])
        | o1 == n2 || n1 == o2 -> Nothing
        | o1 == o2 || n1 == n2 -> Nothing
        | o2 `isPrefix` o1 && n1 `isPrefix` n2 -> Nothing
        | otherwise ->
          return (unsafeCoerceP (pathMap (movedirfilename n1 o1) p2) :>
                  unsafeCoerceP (pathMap (movedirfilename o2 n2) p1))
      _ -> error "impossible case"
    where
      pathMap f (Move old new) = Move (f old) (f new)
      pathMap f (RmDir d) = RmDir (f d)
      pathMap f (AddDir d) = AddDir (f d)
      pathMap f (RmFile d) = RmFile (f d)
      pathMap f (AddFile d) = AddFile (f d)
    
