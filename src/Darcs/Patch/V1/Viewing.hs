{-# OPTIONS_GHC -fno-warn-orphans #-}
module Darcs.Patch.V1.Viewing () where

import Darcs.Prelude

import Darcs.Patch.Apply ( apply )
import Darcs.Patch.Prim ( PrimPatch )
import Darcs.Patch.Show ( ShowPatch(..), ShowContextPatch(..), showPatch )
import Darcs.Patch.Summary ( plainSummary, plainSummaryFL )

import Darcs.Patch.V1.Apply ()
import Darcs.Patch.V1.Core ( RepoPatchV1(..) )
import Darcs.Patch.V1.Show ()

instance PrimPatch prim => ShowContextPatch (RepoPatchV1 prim) where
    showPatchWithContextAndApply f (PP p) = showPatchWithContextAndApply f p
    showPatchWithContextAndApply f p = apply p >> return (showPatch f p)

instance PrimPatch prim => ShowPatch (RepoPatchV1 prim) where
    summary = plainSummary
    summaryFL = plainSummaryFL
    thing _ = "change"
