{-# OPTIONS_GHC -fno-warn-orphans #-}
module Darcs.Patch.V3 ( RepoPatchV3 ) where

import Darcs.Prelude

import Darcs.Patch.Annotate ()
import Darcs.Patch.Conflict
    ( Conflict(..)
    , ConflictDetails
    , Unravelled
    , mangleOrFail
    , mapConflictDetails
    )
import Darcs.Patch.FromPrim ( FromPrim(..) )
import Darcs.Patch.Prim.Class ( PrimPatch )
import Darcs.Patch.Prim.Named
    ( NamedPrim
    , PrimPatchId
    , anonymousNamedPrim
    , namedPrim
    , positivePrimPatchIds
    )
import Darcs.Patch.Prim.WithName ( wnPatch )
import qualified Darcs.Patch.V3.Core as Core ( RepoPatchV3(..) )
import Darcs.Patch.V3.Resolution
    ( conflictingAlternatives
    , patchIsConflicted
    )
import Darcs.Patch.Witnesses.Sealed ( mapSeal )
import Darcs.Patch.Witnesses.Ordered ( FL(..), mapFL_FL )

type RepoPatchV3 = Core.RepoPatchV3 PrimPatchId

-- This instance is specialised to PrimPatchId because it is dependent
-- on the relationship between PatchInfo and PrimPatchId
instance FromPrim (RepoPatchV3 prim) where
  fromAnonymousPrim = Core.Prim . anonymousNamedPrim
  fromPrim pid p = Core.Prim (namedPrim pid p)
  fromPrims = go . positivePrimPatchIds
    where
      go :: [PrimPatchId] -> FL prim wX wY -> FL (RepoPatchV3 prim) wX wY
      go _ NilFL = NilFL
      go (pid:pids) (p:>:ps) = fromPrim pid p :>: go pids ps
      go [] _ = error "positivePrimPatchIds should return an infinite list"

instance PrimPatch prim => Conflict (RepoPatchV3 prim) where
  isConflicted = patchIsConflicted
  resolveConflicts context =
      map resolveOne . conflictingAlternatives context
    where
      resolveOne :: Unravelled (NamedPrim prim) wX -> ConflictDetails prim wX
      resolveOne
        | False==True = mapConflictDetails wnPatch . mangleOrFail
        | True = mangleOrFail . map (mapSeal (mapFL_FL wnPatch))
