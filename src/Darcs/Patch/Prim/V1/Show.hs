{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ViewPatterns, UndecidableInstances #-}
module Darcs.Patch.Prim.V1.Show
    ( showHunk )
    where

import Darcs.Prelude

import Darcs.Util.ByteString ( fromPS2Hex )
import qualified Data.ByteString as B (ByteString, length, take, drop)

import Darcs.Patch.Apply ( Apply(..), ObjectIdOfPatch )
import Darcs.Patch.FileHunk ( FileHunk(..), showFileHunk )
import Darcs.Patch.Format ( FileNameFormat )
import Darcs.Patch.Show ( formatFileName )
import Darcs.Patch.Permutations () -- for Invert instance of FL
import Darcs.Patch.Prim.Class ( PrimShow(..) )
import Darcs.Patch.Prim.V1.Core
     ( Prim(..), FilePatchType(..), DirPatchType(..) )
import Darcs.Patch.Prim.V1.Details ()
import Darcs.Patch.Viewing ( showContextHunk )
import Darcs.Patch.Witnesses.Show ( Show1, Show2 )

import Darcs.Util.Path ( AnchoredPath )
import Darcs.Util.Printer ( Doc, vcat,
                 text, userchunk, invisibleText, invisiblePS, blueText,
                 ($$), (<+>)
               )
import Darcs.Util.Tree ( Tree )


instance Show2 Prim
instance Show1 (Prim wX)
deriving instance Show (Prim wX wY)
deriving instance Show (FilePatchType wX wY)
deriving instance Show (DirPatchType wX wY)

instance (Apply Prim, ApplyState Prim ~ Tree, ObjectIdOfPatch Prim ~ AnchoredPath) =>
         PrimShow Prim where
  showPrim fmt (FP f AddFile) = showAddFile fmt f
  showPrim fmt (FP f RmFile)  = showRmFile fmt f
  showPrim fmt (FP f (Hunk line old new))  = showHunk fmt f line old new
  showPrim fmt (FP f (TokReplace t old new))  = showTok fmt f t old new
  showPrim fmt (FP f (Binary old new))  = showBinary fmt f old new
  showPrim fmt (DP d AddDir) = showAddDir fmt d
  showPrim fmt (DP d RmDir)  = showRmDir fmt d
  showPrim fmt (Move f f') = showMove fmt f f'
  showPrim _ (ChangePref p f t) = showChangePref p f t
  showPrimWithContextAndApply fmt p@(FP f (Hunk line old new)) = do
    r <- showContextHunk fmt (FileHunk f line old new)
    apply p
    return r
  showPrimWithContextAndApply fmt p = do
    apply p
    return $ showPrim fmt p

showAddFile :: FileNameFormat -> AnchoredPath -> Doc
showAddFile fmt f = blueText "addfile" <+> formatFileName fmt f

showRmFile :: FileNameFormat -> AnchoredPath -> Doc
showRmFile fmt f = blueText "rmfile" <+> formatFileName fmt f

showMove :: FileNameFormat -> AnchoredPath -> AnchoredPath -> Doc
showMove fmt d d' = blueText "move" <+> formatFileName fmt d <+> formatFileName fmt d'

showChangePref :: String -> String -> String -> Doc
showChangePref p f t = blueText "changepref" <+> text p
                    $$ userchunk f
                    $$ userchunk t

showAddDir :: FileNameFormat -> AnchoredPath -> Doc
showAddDir fmt d = blueText "adddir" <+> formatFileName fmt d

showRmDir :: FileNameFormat -> AnchoredPath -> Doc
showRmDir fmt d = blueText "rmdir" <+> formatFileName fmt d

showHunk :: FileNameFormat -> AnchoredPath -> Int -> [B.ByteString] -> [B.ByteString] -> Doc
showHunk fmt f line old new = showFileHunk fmt (FileHunk f line old new)

showTok :: FileNameFormat -> AnchoredPath -> String -> String -> String -> Doc
showTok fmt f t o n = blueText "replace" <+> formatFileName fmt f
                                     <+> text "[" <> userchunk t <> text "]"
                                     <+> userchunk o
                                     <+> userchunk n

showBinary :: FileNameFormat -> AnchoredPath -> B.ByteString -> B.ByteString -> Doc
showBinary fmt f o n =
    blueText "binary" <+> formatFileName fmt f
 $$ invisibleText "oldhex"
 $$ vcat (map makeprintable $ breakEvery 78 $ fromPS2Hex o)
 $$ invisibleText "newhex"
 $$ vcat (map makeprintable $ breakEvery 78 $ fromPS2Hex n)
     where makeprintable ps = invisibleText "*" <> invisiblePS ps

breakEvery :: Int -> B.ByteString -> [B.ByteString]
breakEvery n ps | B.length ps < n = [ps]
                 | otherwise = B.take n ps : breakEvery n (B.drop n ps)
