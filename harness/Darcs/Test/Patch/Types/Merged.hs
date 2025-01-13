module Darcs.Test.Patch.Types.Merged
  ( Merged
  , typedMerge
  ) where

import Darcs.Test.Patch.Merge.Checked
import Darcs.Patch.Witnesses.Unsafe
import Darcs.Patch.Witnesses.Ordered
import Darcs.Patch.Merge ( Merge(..), mergerFLFL )

-- | A witness type that makes the result witness of merging explicit:
--
--  wA    ----> Merged wA wB
--   ^           ^
--   |           |
--   |           |
--  wBase ----> wB
--
-- It's quite ad hoc, for example we don't define a type for 'wBase'.
data Merged wA wB

-- | A wrapper around 'merge' for FL that checks each individual merge,
-- and also returns a more strongly typed witness than the usual existential.
typedMerge
  :: CheckedMerge p
  => (FL p :\/: FL p) wA wB
  -> (FL p wA (Merged wA wB), FL p wB (Merged wA wB))
typedMerge (p :\/: q) =
  case mergerFLFL (checkedMerger merge) (p :\/: q) of
    (q' :/\: p') -> (unsafeCoercePEnd q', unsafeCoercePEnd p')
