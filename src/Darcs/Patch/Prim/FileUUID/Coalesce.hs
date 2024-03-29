{-# OPTIONS_GHC -Wno-orphans -Wno-missing-methods #-}
module Darcs.Patch.Prim.FileUUID.Coalesce () where

import Darcs.Prelude

import Darcs.Patch.Prim.Class ( PrimCoalesce(..), PrimSift(..) )
import Darcs.Patch.Prim.FileUUID.Commute ()
import Darcs.Patch.Prim.FileUUID.Core ( Prim )

-- none of the methods are implemented
instance PrimCoalesce Prim where
  sortCoalesceFL = id -- just so that we can use it in the tests

-- none of the methods are implemented
instance PrimSift Prim
