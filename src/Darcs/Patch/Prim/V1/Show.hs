{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ViewPatterns, UndecidableInstances #-}
module Darcs.Patch.Prim.V1.Show
    ( showHunk )
    where

import Darcs.Prelude

import Darcs.Util.ByteString ( fromPS2Hex )
import qualified Data.ByteString as B (ByteString, length, take, drop)

import Darcs.Patch.FileHunk ( FileHunk(..), showFileHunk )
import Darcs.Patch.Permutations ()
import Darcs.Patch.Prim.V1.Core ( DirPatchType(..), FilePatchType(..), Prim(..) )
import Darcs.Patch.Prim.V1.Details ()
import Darcs.Patch.Show ( ShowPatchBasic(..) )
import Darcs.Patch.Witnesses.Show ( Show1, Show2 )

import Darcs.Util.Path ( AnchoredPath, anchorPath )
import Darcs.Util.Printer
    ( Doc
    , blueText
    , invisiblePS
    , invisibleText
    , text
    , userchunk
    , vcat
    , ($$)
    , (<+>)
    )

instance Show2 Prim
instance Show1 (Prim wX)
deriving instance Show (Prim wX wY)
deriving instance Show (FilePatchType wX wY)
deriving instance Show (DirPatchType wX wY)

instance ShowPatchBasic Prim where
  showPatch (FP f AddFile) = showAddFile f
  showPatch (FP f RmFile)  = showRmFile f
  showPatch (FP f (Hunk line old new))  = showHunk f line old new
  showPatch (FP f (TokReplace t old new))  = showTok f t old new
  showPatch (FP f (Binary old new))  = showBinary f old new
  showPatch (DP d AddDir) = showAddDir d
  showPatch (DP d RmDir)  = showRmDir d
  showPatch (Move f f') = showMove f f'
  showPatch (ChangePref p f t) = showChangePref p f t

showAddFile :: AnchoredPath -> Doc
showAddFile f = blueText "addfile" <+> showFileName f

showRmFile :: AnchoredPath -> Doc
showRmFile f = blueText "rmfile" <+> showFileName f

showMove :: AnchoredPath -> AnchoredPath -> Doc
showMove d d' = blueText "move" <+> showFileName d <+> showFileName d'

showChangePref :: String -> String -> String -> Doc
showChangePref p f t = blueText "changepref" <+> text p
                    $$ userchunk f
                    $$ userchunk t

showAddDir :: AnchoredPath -> Doc
showAddDir d = blueText "adddir" <+> showFileName d

showRmDir :: AnchoredPath -> Doc
showRmDir d = blueText "rmdir" <+> showFileName d

showHunk :: AnchoredPath -> Int -> [B.ByteString] -> [B.ByteString] -> Doc
showHunk f line old new = showFileHunk (FileHunk () f line old new)

showTok :: AnchoredPath -> String -> String -> String -> Doc
showTok f t o n = blueText "replace" <+> showFileName f
                                     <+> text "[" <> userchunk t <> text "]"
                                     <+> userchunk o
                                     <+> userchunk n

showBinary :: AnchoredPath -> B.ByteString -> B.ByteString -> Doc
showBinary f o n =
    blueText "binary" <+> showFileName f
 $$ invisibleText "oldhex"
 $$ vcat (map makeprintable $ breakEvery 78 $ fromPS2Hex o)
 $$ invisibleText "newhex"
 $$ vcat (map makeprintable $ breakEvery 78 $ fromPS2Hex n)
     where makeprintable ps = invisibleText "*" <> invisiblePS ps

breakEvery :: Int -> B.ByteString -> [B.ByteString]
breakEvery n ps | B.length ps < n = [ps]
                 | otherwise = B.take n ps : breakEvery n (B.drop n ps)

showFileName :: AnchoredPath -> Doc
showFileName = text . anchorPath "."
