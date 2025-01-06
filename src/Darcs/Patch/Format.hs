module Darcs.Patch.Format
    ( FormatPatch(..)
    ) where

import Darcs.Patch.Witnesses.Ordered ( FL(..), mapFL )
import Darcs.Util.Format ( Format, vcat )

class FormatPatch p where
  formatPatch :: p wX wY -> Format
  -- | Part of the class so we can override formatting of patch lists
  -- for individual instances (to support legacy formats)
  formatPatchFL :: FL p wX wY -> Format
  formatPatchFL ps = vcat (mapFL formatPatch ps)
