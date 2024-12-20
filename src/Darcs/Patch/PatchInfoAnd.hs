-- Copyright (C) 2006 David Roundy
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; see the file COPYING.  If not, write to
-- the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
-- Boston, MA 02110-1301, USA.

module Darcs.Patch.PatchInfoAnd
    ( PatchInfoAnd
    , PatchInfoAndG
    , piap
    , n2pia
    , fmapPIAP
    , fmapFLPIAP
    , hopefully
    , info
    , hopefullyM
    , createHashed
    , extractHash
    , unavailable
    , patchDesc
    ) where

import Darcs.Prelude

import Control.Exception ( Exception, throw )
import Data.Typeable ( Typeable )
import System.IO.Unsafe ( unsafeInterleaveIO )

import Darcs.Patch.Apply ( Apply(..) )
import Darcs.Patch.Commute ( Commute(..) )
import Darcs.Patch.Conflict ( Conflict(..) )
import Darcs.Patch.Debug ( PatchDebug(..) )
import Darcs.Patch.Effect ( Effect(..) )
import Darcs.Patch.FromPrim ( PrimPatchBase(..) )
import Darcs.Patch.Ident ( Ident(..), PatchId )
import Darcs.Patch.Info ( PatchInfo, justName, showPatchInfo )
import Darcs.Patch.Inspect ( PatchInspect(..) )
import Darcs.Patch.Merge ( CleanMerge(..), Merge(..) )
import Darcs.Patch.Named ( Named, fmapFL_Named )
import Darcs.Patch.Repair ( Repair(..), RepairToFL )
import Darcs.Patch.Show ( ShowContextPatch(..), ShowPatch(..), ShowPatchBasic(..) )
import Darcs.Patch.Summary ( Summary )
import Darcs.Patch.Witnesses.Ordered
    ( FL
    , mapFL
    , mapRL_RL
    , (:/\:)(..)
    , (:>)(..)
    , (:\/:)(..)
    )
import Darcs.Patch.Witnesses.Sealed ( Sealed, mapSeal, seal )
import Darcs.Patch.Witnesses.Show ( Show1, Show2 )
import Darcs.Util.Exception ( prettyException )
import Darcs.Util.Printer ( Doc, renderString, text, vcat, ($$) )
import Darcs.Util.SignalHandler ( catchNonSignal )
import Darcs.Util.ValidHash ( PatchHash )

-- | Specialized variant of 'PatchInfoAndG', where the underlying patch is
-- always a 'Named' patch. These are the patches we normally store and read
-- in a repository.
type PatchInfoAnd p = PatchInfoAndG (Named p)

-- | The type of patches we can read from inventories. Inventories have the
-- 'PatchInfo' and the (SHA256) hash of the patch content, but not the content
-- itself. So a 'PatchInfoAnd' always has a 'PatchInfo', but may be missing the
-- underlying patch. It may also be missing a content hash because operations
-- that (potentially) modify the content (like commutation) invalidate the
-- hash.
--
-- This generalized type is mostly needed to deal with legacy rebase versions.
data PatchInfoAndG p wA wB =
  PIAP PatchInfo
       (Maybe PatchHash)
       (Hopefully p wA wB)
  deriving (Show)

-- | Like 'Either String' but with type witnesses.
data Hopefully p wX wY = Unavailable String | Actually (p wX wY)
  deriving Show

fmapH :: (p wX wY -> b wX wY) -> Hopefully p wX wY -> Hopefully b wX wY
fmapH _ (Unavailable e) = Unavailable e
fmapH f (Actually p) = Actually (f p)

-- | The 'PatchInfo' of a 'PatchInfoAndG'
info :: PatchInfoAndG p wA wB -> PatchInfo
info (PIAP i _ _) = i

-- | Just the name part of the info as a 'String'.
patchDesc :: forall p wX wY . PatchInfoAnd p wX wY -> String
patchDesc p = justName $ info p

-- | Create a 'PatchInfoAndG' with the given info and patch and no hash.
piap :: PatchInfo -> p wA wB -> PatchInfoAndG p wA wB
piap i p = PIAP i Nothing (Actually p)

-- | Create a 'PatchInfoAndG' from a patch with an identity whose type
-- coincides with 'PatchInfo'.
n2pia :: (Ident p, PatchId p ~ PatchInfo) => p wX wY -> PatchInfoAndG p wX wY
n2pia x = ident x `piap` x

fmapFLPIAP
  :: (FL p wX wY -> FL q wX wY) -> PatchInfoAnd p wX wY -> PatchInfoAnd q wX wY
fmapFLPIAP f (PIAP i _ hp) = PIAP i Nothing (fmapH (fmapFL_Named f) hp)

fmapPIAP
  :: (p wX wY -> q wX wY) -> PatchInfoAndG p wX wY -> PatchInfoAndG q wX wY
fmapPIAP f (PIAP i _ hp) = PIAP i Nothing (fmapH f hp)

-- | Using a special exception type here means that is is treated as
-- regular failure, and not as a bug in Darcs.
data PatchNotAvailable = PatchNotAvailable Doc
  deriving Typeable

instance Exception PatchNotAvailable

instance Show PatchNotAvailable where
  show (PatchNotAvailable e) = renderString e

-- | Return 'Just' the patch content or 'Nothing' if it is unavailable.
hopefullyM :: PatchInfoAndG p wA wB -> Maybe (p wA wB)
hopefullyM (PIAP _ _ hp) =
  case hp of
    Actually p -> return p
    Unavailable _ -> Nothing

-- | Try to get a patch from a 'PatchInfoAndG'. If it fails, it throws a
-- 'PatchNotAvailable' exception.
hopefully :: PatchInfoAndG p wA wB -> p wA wB
-- Note: the lazy pattern match is required by the way this function is used
hopefully ~(PIAP pinf _ hp) =
  case hp of
    Actually p -> p
    Unavailable e -> throw $ PatchNotAvailable $
      text "failed to read patch:" $$ showPatchInfo pinf $$ text e

-- | Construct an 'Unavailable' patch. Used e.g. when reading the context
-- part of a patch bundle.
unavailable :: PatchInfo -> String -> PatchInfoAndG p wX wY
unavailable i e = PIAP i Nothing (Unavailable e)

-- | Lift an 'IO' action that reads a patch (given its hash) to one
-- that reads a 'PatchInfoAndG'. The read action is delayed using
-- 'unsafeInterleaveIO' and all non-signal exceptions it may throw
-- are handled by creating an 'Unavailable' patch.
createHashed
  :: PatchInfo
  -> PatchHash
  -> IO (Sealed (p wX))
  -> IO (Sealed (PatchInfoAndG p wX))
createHashed i h reader =
  mapSeal (PIAP i (Just h)) <$>
    unsafeInterleaveIO ((mapSeal Actually <$> reader) `catchNonSignal` handler)
  where
    handler e = return $ seal $ Unavailable $ prettyException e

-- | Return either the hash (if available) or else the underlying patch.
extractHash :: PatchInfoAndG p wA wB -> Either (p wA wB) PatchHash
extractHash (PIAP _ (Just h) _) = Right h
extractHash p = Left (hopefully p)

-- * Instances defined only for PatchInfoAnd

instance Show2 p => Show1 (PatchInfoAnd p wX)

instance Show2 p => Show2 (PatchInfoAnd p)

instance RepairToFL p => Repair (PatchInfoAnd p) where
    applyAndTryToFix p = do mp' <- applyAndTryToFix $ hopefully p
                            case mp' of
                              Nothing -> return Nothing
                              Just (e,p') -> return $ Just (e, n2pia p')

-- * Instances defined for PatchInfoAndG

instance PrimPatchBase p => PrimPatchBase (PatchInfoAndG p) where
   type PrimOf (PatchInfoAndG p) = PrimOf p

type instance PatchId (PatchInfoAndG p) = PatchInfo

instance Ident (PatchInfoAndG p) where
    ident = info

instance ShowPatchBasic p => ShowPatchBasic (PatchInfoAndG p) where
    showPatch (PIAP n _ p) =
      case p of
        Actually x -> showPatch x
        Unavailable _ -> showPatchInfo n

instance ShowContextPatch p => ShowContextPatch (PatchInfoAndG p) where
  showPatchWithContextAndApply (PIAP n _ p) =
    case p of
      Actually x -> showPatchWithContextAndApply x
      Unavailable _ -> return $ showPatchInfo n

instance (Summary p, ShowPatch p) => ShowPatch (PatchInfoAndG p) where
    description (PIAP n _ _) = showPatchInfo n
    summary (PIAP _ _ p) =
      case p of
        Actually x -> summary x
        Unavailable _ -> text $ "[patch summary is unavailable]"
    summaryFL = vcat . mapFL summary
    content (PIAP _ _ p) =
      case p of
        Actually x -> content x
        Unavailable _ -> text $ "[patch content is unavailable]"

instance (PatchId p ~ PatchInfo, Commute p) => Commute (PatchInfoAndG p) where
    commute (x :> y) = do y' :> x' <- commute (hopefully x :> hopefully y)
                          return $ (ident y `piap` y') :> (ident x `piap` x')

instance (PatchId p ~ PatchInfo, CleanMerge p) =>
         CleanMerge (PatchInfoAndG p) where
    cleanMerge (x :\/: y)
      | ident x == ident y = error "cannot cleanMerge identical PatchInfoAndG"
      | otherwise = do
          y' :/\: x' <- cleanMerge (hopefully x :\/: hopefully y)
          return $ (ident y `piap` y') :/\: (ident x `piap` x')

instance (PatchId p ~ PatchInfo, Merge p) => Merge (PatchInfoAndG p) where
    merge (x :\/: y)
      | ident x == ident y = error "cannot merge identical PatchInfoAndG"
      | otherwise =
          case merge (hopefully x :\/: hopefully y) of
            y' :/\: x' -> (ident y `piap` y') :/\: (ident x `piap` x')

instance PatchInspect p => PatchInspect (PatchInfoAndG p) where
    listTouchedFiles = listTouchedFiles . hopefully
    hunkMatches f = hunkMatches f . hopefully

instance Apply p => Apply (PatchInfoAndG p) where
    type ApplyState (PatchInfoAndG p) = ApplyState p
    apply = apply . hopefully
    unapply = unapply . hopefully

instance Effect p => Effect (PatchInfoAndG p) where
    effect = effect . hopefully

instance PatchDebug p => PatchDebug (PatchInfoAndG p)

instance (Commute p, Conflict p, Summary p, PrimPatchBase p, ShowPatch p) => Conflict (PatchInfoAnd p) where
    numConflicts = numConflicts . hopefully
    -- Note: this relies on the laziness of 'hopefully' for efficiency
    -- and correctness in the face of lazy repositories
    resolveConflicts context patches =
      resolveConflicts (mapRL_RL hopefully context) (mapRL_RL hopefully patches)
