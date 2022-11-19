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
    ( Hopefully
    , PatchInfoAnd
    , PatchInfoAndG
    , piap
    , n2pia
    , patchInfoAndPatch
    , fmapPIAP
    , fmapFLPIAP
    , hopefully
    , info
    , hopefullyM
    , createHashed
    , extractHash
    , actually
    , unavailable
    , patchDesc
    ) where

import Darcs.Prelude

import Control.Exception ( Exception, throw )
import System.IO.Unsafe ( unsafeInterleaveIO )
import Data.Typeable ( Typeable )

import Darcs.Patch.Apply ( Apply(..) )
import Darcs.Patch.Commute ( Commute(..) )
import Darcs.Patch.Conflict ( Conflict(..) )
import Darcs.Patch.Debug ( PatchDebug(..) )
import Darcs.Patch.Effect ( Effect(..) )
import Darcs.Patch.Format ( PatchListFormat )
import Darcs.Patch.FromPrim ( PrimPatchBase(..) )
import Darcs.Patch.Ident ( Ident(..), PatchId )
import Darcs.Patch.Info ( PatchInfo, displayPatchInfo, justName, showPatchInfo )
import Darcs.Patch.Inspect ( PatchInspect(..) )
import Darcs.Patch.Merge ( CleanMerge(..), Merge(..) )
import Darcs.Patch.Named ( Named, fmapFL_Named )
import Darcs.Patch.Read ( ReadPatch(..) )
import Darcs.Patch.Repair ( Repair(..), RepairToFL )
import Darcs.Patch.Show ( ShowPatch(..) )
import Darcs.Patch.Show ( ShowContextPatch(..), ShowPatchBasic(..) )
import Darcs.Patch.Summary ( Summary )
import Darcs.Patch.Witnesses.Eq ( Eq2(..) )
import Darcs.Patch.Witnesses.Ordered
    ( (:/\:)(..)
    , (:>)(..)
    , (:\/:)(..)
    , FL
    , mapFL
    , mapRL_RL
    )
import Darcs.Patch.Witnesses.Sealed ( Sealed(Sealed), mapSeal, seal )
import Darcs.Patch.Witnesses.Show ( Show1, Show2 )
import Darcs.Util.Exception ( prettyException )
import Darcs.Util.Printer ( Doc, renderString, text, vcat, ($$) )
import Darcs.Util.SignalHandler ( catchNonSignal )
import Darcs.Util.ValidHash ( PatchHash )

-- | @'Hopefully' p C@ @(x y)@ is @'Either' String (p C@ @(x y))@ in a
-- form adapted to darcs patches. The @C@ @(x y)@ represents the type
-- witness for the patch that should be there. The @Hopefully@ type
-- just tells whether we expect the patch to be hashed or not, and
-- 'SimpleHopefully' does the real work of emulating
-- 'Either'. @Hopefully sh@ represents an expected unhashed patch, and
-- @Hashed hash sh@ represents an expected hashed patch with its hash.
data Hopefully a wX wY
    = Hopefully (SimpleHopefully a wX wY)
    | Hashed PatchHash (SimpleHopefully a wX wY)
    deriving Show

-- | @SimpleHopefully@ is a variant of @Either String@ adapted for
-- type witnesses. @Actually@ is the equivalent of @Right@, while
-- @Unavailable@ is @Left@.
data SimpleHopefully a wX wY = Actually (a wX wY) | Unavailable String
    deriving Show

type PatchInfoAnd p = PatchInfoAndG (Named p)

-- | @'PatchInfoAnd' p wA wB@ represents a hope we have to get a
-- patch through its info. We're not sure we have the patch, but we
-- know its info.
data PatchInfoAndG p wA wB =
  -- TODO Should the PatchInfo really be strict here and in Named?
  -- Sharing it with the one inside the Named (if present) would probably
  -- consume a lot less memory. Similarly when we manipulate (commute, merge)
  -- patches. For the vast majority of patches their PatchInfo never changes
  -- once it is read from disk.
  PIAP !PatchInfo
       (Hopefully p wA wB)
  deriving (Show)

fmapH :: (a wX wY -> b wW wZ) -> Hopefully a wX wY -> Hopefully b wW wZ
fmapH f (Hopefully sh) = Hopefully (ff sh)
    where ff (Actually a) = Actually (f a)
          ff (Unavailable e) = Unavailable e
fmapH f (Hashed _ sh) = Hopefully (ff sh)
    where ff (Actually a) = Actually (f a)
          ff (Unavailable e) = Unavailable e

info :: PatchInfoAndG p wA wB -> PatchInfo
info (PIAP i _) = i

patchDesc :: forall p wX wY . PatchInfoAnd p wX wY -> String
patchDesc p = justName $ info p

-- | @'piap' i p@ creates a PatchInfoAnd containing p with info i.
piap :: PatchInfo -> p wA wB -> PatchInfoAndG p wA wB
piap i p = PIAP i (Hopefully $ Actually p)

-- | @n2pia@ creates a PatchInfoAnd representing a @Named@ patch.
n2pia :: (Ident p, PatchId p ~ PatchInfo) => p wX wY -> PatchInfoAndG p wX wY
n2pia x = ident x `piap` x

patchInfoAndPatch :: PatchInfo -> Hopefully p wA wB -> PatchInfoAndG p wA wB
patchInfoAndPatch =  PIAP

fmapFLPIAP :: (FL p wX wY -> FL q wX wY)
           -> PatchInfoAnd p wX wY -> PatchInfoAnd q wX wY
fmapFLPIAP f (PIAP i hp) = PIAP i (fmapH (fmapFL_Named f) hp)

fmapPIAP :: (p wX wY -> q wX wY)
           -> PatchInfoAndG p wX wY -> PatchInfoAndG q wX wY
fmapPIAP f (PIAP i hp) = PIAP i (fmapH f hp)

-- | @'hopefully' hp@ tries to get a patch from a 'PatchInfoAnd'
-- value. If it fails, it outputs an error \"failed to read patch:
-- \<description of the patch>\". We get the description of the patch
-- from the info part of 'hp'
hopefully :: PatchInfoAndG p wA wB -> p wA wB
hopefully = conscientiously $ \e -> text "failed to read patch:" $$ e

-- | Using a special exception type here means that is is treated as
-- regular failure, and not as a bug in Darcs.
data PatchNotAvailable = PatchNotAvailable Doc
  deriving Typeable

instance Exception PatchNotAvailable

instance Show PatchNotAvailable where
  show (PatchNotAvailable e) = renderString e

-- | @'conscientiously' er hp@ tries to extract a patch from a 'PatchInfoAnd'.
-- If it fails, it applies the error handling function @er@ to a description
-- of the patch info component of @hp@.
-- Note: this function must be lazy in its second argument, which is why we
-- use a lazy pattern match.
conscientiously :: (Doc -> Doc)
                -> PatchInfoAndG p wA wB -> p wA wB
conscientiously er ~(PIAP pinf hp) =
    case hopefully2either hp of
      Right p -> p
      Left e -> throw $ PatchNotAvailable $ er (displayPatchInfo pinf $$ text e)

-- | Return 'Just' the patch content or 'Nothing' if it is unavailable.
hopefullyM :: PatchInfoAndG p wA wB -> Maybe (p wA wB)
hopefullyM (PIAP _ hp) = case hopefully2either hp of
                              Right p -> return p
                              Left _ -> Nothing

-- Any recommendations for a nice adverb to name the below?
hopefully2either :: Hopefully a wX wY -> Either String (a wX wY)
hopefully2either (Hopefully (Actually p)) = Right p
hopefully2either (Hashed _ (Actually p)) = Right p
hopefully2either (Hopefully (Unavailable e)) = Left e
hopefully2either (Hashed _ (Unavailable e)) = Left e

actually :: a wX wY -> Hopefully a wX wY
actually = Hopefully . Actually

createHashed :: PatchHash -> (PatchHash -> IO (Sealed (a wX))) -> IO (Sealed (Hopefully a wX))
createHashed h f = mapSeal (Hashed h) `fmap` unsafeInterleaveIO (f' `catchNonSignal` handler)
  where
  f' = do Sealed x <- f h
          return (Sealed (Actually x))
  handler e = return $ seal $ Unavailable $ prettyException e

extractHash :: PatchInfoAndG p wA wB -> Either (p wA wB) PatchHash
extractHash (PIAP _ (Hashed sh _)) = Right sh
extractHash hp = Left $ conscientiously (\e -> text "unable to read patch:" $$ e) hp

unavailable :: String -> Hopefully a wX wY
unavailable = Hopefully . Unavailable

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

getHopefully :: Hopefully p wX wY -> SimpleHopefully p wX wY
getHopefully (Hashed _ x) = x
getHopefully (Hopefully x) = x

instance Eq2 p => Eq2 (SimpleHopefully p) where
    Actually p1 `unsafeCompare` Actually p2 = p1 `unsafeCompare` p2
    _ `unsafeCompare` _ = error "cannot compare unavailable patches"

instance Eq2 p => Eq2 (Hopefully p) where
    Hashed h1 _ `unsafeCompare` Hashed h2 _ = h1 == h2
    hp1 `unsafeCompare` hp2 =
      getHopefully hp1 `unsafeCompare` getHopefully hp2

instance Eq2 p => Eq2 (PatchInfoAndG p) where
    PIAP i1 p1 `unsafeCompare` PIAP i2 p2 = i1 == i2 && p1 `unsafeCompare` p2

type instance PatchId (PatchInfoAndG p) = PatchInfo

instance Ident (PatchInfoAndG p) where
    ident (PIAP i _) = i

instance PatchListFormat (PatchInfoAndG p)

instance ShowPatchBasic p => ShowPatchBasic (PatchInfoAndG p) where
    showPatch f (PIAP n p) =
      case hopefully2either p of
        Right x -> showPatch f x
        Left _ -> showPatchInfo f n

instance ShowContextPatch p => ShowContextPatch (PatchInfoAndG p) where
  showContextPatch f (PIAP n p) =
    case hopefully2either p of
      Right x -> showContextPatch f x
      Left _ -> return $ showPatchInfo f n

instance (Summary p, PatchListFormat p,
          ShowPatch p) => ShowPatch (PatchInfoAndG p) where
    description (PIAP n _) = displayPatchInfo n
    summary (PIAP _ p) =
      case hopefully2either p of
        Right x -> summary x
        Left _ -> text $ "[patch summary is unavailable]"
    summaryFL = vcat . mapFL summary
    content (PIAP _ p) =
      case hopefully2either p of
        Right x -> content x
        Left _ -> text $ "[patch content is unavailable]"

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

instance ( ReadPatch p, Ident p, PatchId p ~ PatchInfo
         ) => ReadPatch (PatchInfoAndG p) where
    readPatch' = mapSeal n2pia <$> readPatch'

instance Effect p => Effect (PatchInfoAndG p) where
    effect = effect . hopefully

instance PatchDebug p => PatchDebug (PatchInfoAndG p)

instance (Commute p, Conflict p, Summary p, PrimPatchBase p, PatchListFormat p, ShowPatch p) => Conflict (PatchInfoAnd p) where
    isConflicted = isConflicted . hopefully
    -- Note: this relies on the laziness of 'hopefully' for efficiency
    -- and correctness in the face of lazy repositories
    resolveConflicts context patches =
      resolveConflicts (mapRL_RL hopefully context) (mapRL_RL hopefully patches)
