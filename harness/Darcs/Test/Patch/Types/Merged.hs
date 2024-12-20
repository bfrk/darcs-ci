module Darcs.Test.Patch.Types.Merged
  ( Merged
  , typedMerge
  ) where

import Darcs.Test.Patch.Merge.Checked
import Darcs.Patch.Witnesses.Unsafe
import Darcs.Patch.Witnesses.Ordered
import Darcs.Patch.Merge ( Merge(..), mergerFLFL )

{- | A witness type that makes the result witness of merging explicit:

>  wA -----> wY = Merged wA wB
>   ^        ^
>   |        |
>   |        |
>   |        |
>  wX -----> wB

It's quite ad hoc. Even if we add @wX@ as a third parameter, as in

> typedMerge :: p wX wA -> p wX wB -> (p wA (Merged wA wX wB), p wB (Merged wA wX wB))

this breaks down as soon as we try to exploit symmetries. For instance, the
symmetry of merge requires that

> Merged wA wX wB ~ Merged wB wX wA

and, given something like

> typedCommute :: p wX wA -> p wA wY -> (p wX wB, p wB wY)

the merge-commute law would require

> wB ~ Merged wY wA wX ~ Merged (Merged wB wX wA) wA wX

etc. In fact, we want equalities corresponding to all 8 symmetries of a
square (the group D4 with 4 rotations and 4 reflections).

The problem here seems to be that we use '*' (aka 'Type') for witnesses.
With a dedicated witness kind we could define our own equality rules and
hope that we can convince the type checker to make use of them. -}

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
