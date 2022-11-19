{-# OPTIONS_GHC -fno-warn-orphans #-}
module Darcs.Patch.Prim
    ( PrimApply(..)
    , PrimCoalesce(..)
    , PrimConstruct(..)
    , PrimDetails(..)
    , PrimMangleUnravelled(..)
    , PrimPatch
    , PrimRead(..)
    , PrimShow(..)
    , PrimSift(..)
    , Mangled
    , Unravelled
    , canonizeFL
    ) where

import Darcs.Patch.Prim.Class
    ( PrimApply(..)
    , PrimCoalesce(..)
    , PrimConstruct(..)
    , PrimDetails(..)
    , PrimMangleUnravelled(..)
    , PrimPatch
    , PrimRead(..)
    , PrimShow(..)
    , PrimSift(..)
    , Mangled
    , Unravelled
    )
import Darcs.Patch.Prim.Canonize
    ( canonizeFL
    )
