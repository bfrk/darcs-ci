{-# OPTIONS_GHC -Wno-orphans #-}
module Darcs.Patch.Prim.FileUUID ( Prim ) where

import Darcs.Prelude

import Darcs.Patch.Prim.FileUUID.Apply ()
import Darcs.Patch.Prim.FileUUID.Coalesce ()
import Darcs.Patch.Prim.FileUUID.Commute ()
import Darcs.Patch.Prim.FileUUID.Core ( Prim, UUID )
import Darcs.Patch.Prim.FileUUID.Details ()
import Darcs.Patch.Prim.FileUUID.Format ()
import Darcs.Patch.Prim.FileUUID.Read ()
import Darcs.Patch.Prim.FileUUID.Show ( showUUID )

import Darcs.Patch.Object ( ObjectId(..) )
import Darcs.Patch.Prim.Class
  ( PrimMangleUnravelled(..)
  )

-- dummy implementation
instance PrimMangleUnravelled Prim where
  mangleUnravelled _ = Nothing

instance ObjectId UUID where
  showObjectId = showUUID
