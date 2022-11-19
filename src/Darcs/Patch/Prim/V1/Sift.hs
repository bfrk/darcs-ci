{-# OPTIONS_GHC -fno-warn-orphans #-}
module Darcs.Patch.Prim.V1.Sift () where

import Prelude ()
import Darcs.Prelude

import Darcs.Patch.Prim.V1.Coalesce () -- instance PrimCoalesce Prim
import Darcs.Patch.Prim.V1.Commute () -- instance Commute Prim
import Darcs.Patch.Prim.V1.Core ( Prim(..), FilePatchType(..) )

import Darcs.Patch.Permutations ( partitionFL )
import Darcs.Patch.Prim.Class ( PrimCoalesce(tryToShrink), PrimSift(..) )
import Darcs.Patch.Witnesses.Ordered ( (:>)(..), (+>+) )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..) )

-- | 'Prim's that are candidates for sifting.
primIsSiftable :: Prim wX wY -> Bool
primIsSiftable (FP _ (Binary _ _)) = True
primIsSiftable (FP _ (Hunk _ _ _)) = True
primIsSiftable _ = False

{- TODO The algorithm here is Prim.V1 specific only via primIsSiftable. We
could further simplify the interface by turning primIsSiftable into the only
method of this class; siftForPending would then become a plain overloaded
function. Since we also use tryToShrink from class PrimCoalesce, it may make
sense to add primIsSiftabe there and eliminate this class. This should be
postponed until we have cleaned up class PrimCoalesce. -}

instance PrimSift Prim where
  -- | Alternately 'sift' and 'tryToShrink' until shrinking no longer reduces
  -- the length of the sequence. Here, 'sift' means to commute siftable
  -- patches to the end of the sequence and then drop them.
  siftForPending ps =
    case sift ps of
      Sealed sifted ->
        case tryToShrink sifted of
          Nothing -> Sealed sifted
          Just shrunk -> siftForPending shrunk
    where
      sift xs =
        case partitionFL (not . primIsSiftable) xs of
          (not_siftable :> deps :> _) -> Sealed (not_siftable +>+ deps)
