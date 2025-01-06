-- | Generic wrapper for prim patches to give them an identity.
module Darcs.Patch.Prim.WithName
  ( PrimWithName(..)
  ) where

import Darcs.Prelude

import Darcs.Patch.Annotate ( Annotate(..) )
import Darcs.Patch.Apply ( Apply(..) )
import Darcs.Patch.Commute ( Commute(..) )
import Darcs.Patch.Format ( FormatPatch(..) )
import Darcs.Patch.Ident
    ( Ident(..)
    , PatchId
    , SignedId(..)
    , StorableId(..)
    )
import Darcs.Patch.Inspect ( PatchInspect(..) )
import Darcs.Patch.FileHunk ( IsHunk(..), FileHunk(..) )
import Darcs.Patch.Prim.Class ( PrimApply(..), PrimDetails(..) )
import Darcs.Patch.Invert ( Invert(..) )
import Darcs.Patch.Merge ( CleanMerge(..) )
import Darcs.Patch.Read ( ReadPatch(..), ReadPatches(..) )
import Darcs.Patch.Repair ( RepairToFL(..) )
import Darcs.Patch.Show
    ( ShowPatchBasic(..)
    , ShowPatch(..)
    , ShowContextPatch(..)
    )
import Darcs.Patch.Summary ( plainSummaryPrim, plainSummaryPrims )
import Darcs.Patch.Witnesses.Eq ( Eq2(..), EqCheck(..) )
import Darcs.Patch.Witnesses.Ordered ( mapFL_FL, (:>)(..), (:\/:)(..), (:/\:)(..) )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..) )
import Darcs.Patch.Witnesses.Show ( Show1, Show2, appPrec, showsPrec2 )

import Darcs.Util.Printer
import qualified Darcs.Util.Format as F

-- |A 'PrimWithName' is a general way of associating an identity
-- with an underlying (presumably unnamed) primitive type. This is
-- required, for example, for V3 patches.
-- Normally the members of the 'name' type will be generated in
-- some way when a patch is initially created, to guarantee global
-- unqiueness across all repositories.
data PrimWithName name p wX wY =
  PrimWithName { wnName :: !name, wnPatch :: !(p wX wY) }

type instance PatchId (PrimWithName name p) = name

instance SignedId name => Ident (PrimWithName name p) where
  ident = wnName

instance (Eq name, Eq2 p) => Eq2 (PrimWithName name p) where
  PrimWithName i p =\/= PrimWithName j q
    | i == j, IsEq <- p =\/= q = IsEq
    | otherwise = NotEq

instance (Invert p, SignedId name) => Invert (PrimWithName name p) where
  invert (PrimWithName i p) = PrimWithName (invertId i) (invert p)

instance PatchInspect p => PatchInspect (PrimWithName name p) where
  listTouchedFiles = listTouchedFiles . wnPatch
  hunkMatches m = hunkMatches m . wnPatch

instance (Show2 p, Show name) => Show (PrimWithName name p wX wY) where
  showsPrec d (PrimWithName i p) =
    showParen (d > appPrec)
      $ showString "PrimWithName "
      . showsPrec (appPrec + 1) i
      . showString " "
      . showsPrec2 (appPrec + 1) p

instance (Show2 p, Show name) => Show1 (PrimWithName name p wX)

instance (Show2 p, Show name) => Show2 (PrimWithName name p)

instance Apply p => Apply (PrimWithName name p) where
  type ApplyState (PrimWithName name p) = ApplyState p
  apply = apply . wnPatch
  unapply = unapply . wnPatch

instance Apply p => RepairToFL (PrimWithName name p) where
  applyAndTryToFixFL p = apply p >> return Nothing

instance Annotate p => Annotate (PrimWithName name p) where
  annotate = annotate . wnPatch

instance (IsHunk p, Print name) => IsHunk (PrimWithName name p) where
  type ExtraData (PrimWithName name p) = (name, ExtraData p)
  isHunk (PrimWithName name p) = do
    FileHunk xd oid l n o <- isHunk p
    return $ FileHunk (name, xd) oid l n o
  fromHunk (FileHunk (name, xd) oid l n o) =
    PrimWithName name (fromHunk (FileHunk xd oid l n o))

instance PrimApply p => PrimApply (PrimWithName name p) where
  applyPrimFL = applyPrimFL . mapFL_FL wnPatch

instance PrimDetails p => PrimDetails (PrimWithName name p) where
  summarizePrim = summarizePrim . wnPatch

-- this is the most important definition:
-- it ensures that a patch conflicts with itself
instance (SignedId name, Commute p) => Commute (PrimWithName name p) where
  commute (PrimWithName i1 p1 :> PrimWithName i2 p2)
    -- We should never get into a situation where we try
    -- to commute identical patches
    | i1 == i2 = error "internal error: trying to commute identical patches"
    -- whereas this case is the equivalent of merging a patch
    -- with itself, so it is correct to just report that they don't commute
    | i1 == invertId i2 = Nothing
    | otherwise = do
        p2' :> p1' <- commute (p1 :> p2)
        return (PrimWithName i2 p2' :> PrimWithName i1 p1')

instance (SignedId name, CleanMerge p) => CleanMerge (PrimWithName name p) where
  cleanMerge (PrimWithName i1 p1 :\/: PrimWithName i2 p2)
    | i1 == i2 = error "cannot cleanMerge identical patches"
    | otherwise = do
        p2' :/\: p1' <- cleanMerge (p1 :\/: p2)
        return $ PrimWithName i2 p2' :/\: PrimWithName i1 p1'

instance (StorableId name, ReadPatch p) => ReadPatch (PrimWithName name p) where
  readPatch' = do
      name <- readId
      Sealed p <- readPatch'
      return (Sealed (PrimWithName name p))

instance (StorableId name, ReadPatch p) => ReadPatches (PrimWithName name p)

instance (StorableId name, ShowPatchBasic p) => ShowPatchBasic (PrimWithName name p) where
  showPatch (PrimWithName name p) = showId name $$ showPatch p

instance (StorableId name, FormatPatch p) => FormatPatch (PrimWithName name p) where
  formatPatch (PrimWithName name p) = formatId name F.$$ formatPatch p

instance (StorableId name, PrimDetails p, ShowPatch p) => ShowPatch (PrimWithName name p) where
  content = content . wnPatch
  description = description . wnPatch
  summary = plainSummaryPrim . wnPatch
  summaryFL = plainSummaryPrims False
  thing = thing . wnPatch
  things = things . wnPatch

instance (StorableId name, ShowContextPatch p) => ShowContextPatch (PrimWithName name p) where
  showPatchWithContextAndApply (PrimWithName name p) = do
    r <- showPatchWithContextAndApply p
    return $ showId name $$ r
