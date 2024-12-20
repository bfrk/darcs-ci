{-# OPTIONS_GHC -Wno-orphans #-}
module Darcs.Patch.V1.Show () where

import Darcs.Prelude

import Darcs.Patch.Show ( ShowPatchBasic(..) )
import Darcs.Patch.V1.Core ( RepoPatchV1(..) )

import Darcs.Util.Printer ( Doc, text, blueText, ($$), (<+>) )


showMerger :: ShowPatchBasic prim
           => String
           -> RepoPatchV1 prim wA wB
           -> RepoPatchV1 prim wD wE
           -> Doc
showMerger merger_name p1 p2 =
    blueText merger_name <+> text "0.0" <+> blueText "("
                           $$ showPatch p1
                           $$ showPatch p2
                           $$ blueText ")"

instance ShowPatchBasic prim => ShowPatchBasic (RepoPatchV1 prim) where
    showPatch (PP p) = showPatch p
    showPatch (Merger _ _ p1 p2) = showMerger "merger" p1 p2
    showPatch (Regrem _ _ p1 p2) = showMerger "regrem" p1 p2
