-- it is stupid that we need UndecidableInstances just to call another
-- type function (see instance Apply below which requires this)
{-# LANGUAGE UndecidableInstances #-}
module Darcs.Patch.V2.Prim ( Prim(..) ) where

import Darcs.Prelude

import Control.Monad ( (<=<) )
import Data.Coerce (coerce )

import Darcs.Patch.Annotate ( Annotate )
import Darcs.Patch.Apply ( Apply(..) )
import Darcs.Patch.Commute ( Commute(..) )
import Darcs.Patch.FileHunk ( IsHunk )
import Darcs.Patch.Format ( FormatPatch(..) )
import Darcs.Patch.Inspect ( PatchInspect )
import Darcs.Patch.Invert ( Invert )
import Darcs.Patch.Merge ( CleanMerge )
import Darcs.Patch.Read ( ReadPatch(..), ReadPatches(..), legacyReadPatchFL' )
import Darcs.Patch.Repair ( RepairToFL(..) )
import Darcs.Patch.Show
    ( ShowPatchBasic(..)
    , ShowPatch(..)
    , ShowContextPatch(..)
    )
import Darcs.Patch.Summary ( plainSummaryPrim, plainSummaryPrims )

import Darcs.Patch.Witnesses.Eq ( Eq2 )
import Darcs.Patch.Witnesses.Show ( Show1, Show2 )
import Darcs.Patch.Witnesses.Sealed ( mapSeal )

import Darcs.Patch.Prim.Class
    ( PrimConstruct(..), PrimCoalesce(..)
    , PrimDetails(..)
    , PrimApply(..)
    , PrimSift(..)
    , PrimMangleUnravelled(..)
    )
import qualified Darcs.Patch.Prim.V1 as Base ( Prim, formatPrim, readPrim )

import Darcs.Util.ByteString ( decodeLocale )
import Darcs.Util.Format ( userchunk )
import Darcs.Util.Path ( anchorPath, decodeWhite, encodeWhite, floatPath )

newtype Prim x y = Prim { unPrim :: Base.Prim x y } deriving
    ( Annotate
    , Apply
    , CleanMerge
    , Commute
    , Invert
    , IsHunk
    , Eq2
    , PatchInspect
    , PrimApply
    , PrimCoalesce
    , PrimConstruct
    , PrimDetails
    , PrimMangleUnravelled
    , PrimSift
    , Show
    , ShowContextPatch
    , ShowPatchBasic
    )

instance Show1 (Prim wX)

instance Show2 Prim

instance ReadPatch Prim where
  readPatch' = fmap (mapSeal Prim) (Base.readPrim decodePath)
    where
      decodePath = floatPath <=< decodeWhite . decodeLocale

instance ReadPatches Prim where
  readPatchFL' = legacyReadPatchFL'

instance ShowPatch Prim where
  summary = plainSummaryPrim . unPrim
  summaryFL = plainSummaryPrims False
  thing _ = "change"

instance FormatPatch Prim where
  formatPatch = Base.formatPrim encodePath . unPrim
    where
      encodePath = userchunk . encodeWhite . anchorPath "."

instance RepairToFL Prim where
  applyAndTryToFixFL = fmap coerce . applyAndTryToFixFL . unPrim
