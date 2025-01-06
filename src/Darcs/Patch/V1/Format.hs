{-# OPTIONS_GHC -Wno-orphans #-}
module Darcs.Patch.V1.Format () where

import Darcs.Prelude

import Darcs.Patch.Format ( FormatPatch(..) )
import Darcs.Patch.V1.Core ( RepoPatchV1(..) )
import Darcs.Patch.Witnesses.Ordered ( FL(..), mapFL )
import Darcs.Util.Format ( Format, ascii, vcat, (<+>) )

formatMerger
  :: FormatPatch prim
  => String
  -> RepoPatchV1 prim wA wB
  -> RepoPatchV1 prim wD wE
  -> Format
formatMerger merger_name p1 p2 =
  vcat
    [ ascii merger_name <+> ascii "0.0 ("
    , formatPatch p1
    , formatPatch p2
    , ascii ")"
    ]

instance FormatPatch prim => FormatPatch (RepoPatchV1 prim) where
  formatPatch (PP p            ) = formatPatch p
  formatPatch (Merger _ _ p1 p2) = formatMerger "merger" p1 p2
  formatPatch (Regrem _ _ p1 p2) = formatMerger "regrem" p1 p2
  -- exceptional legacy encoding of patch lists
  formatPatchFL (p :>: NilFL) = formatPatch p
  formatPatchFL ps = vcat [ascii "{", vcat (mapFL formatPatch ps), ascii "}"]
