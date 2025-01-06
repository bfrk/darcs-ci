module Darcs.Patch.Prim
    ( PrimApply(..)
    , PrimCoalesce(..)
    , PrimConstruct(..)
    , PrimDetails(..)
    , PrimMangleUnravelled(..)
    , PrimPatch
    , PrimSift(..)
    , Mangled
    , Unravelled
    , canonizeFL
    , coalesce
    ) where

import Darcs.Patch.Prim.Class
    ( PrimApply(..)
    , PrimCoalesce(..)
    , PrimConstruct(..)
    , PrimDetails(..)
    , PrimMangleUnravelled(..)
    , PrimPatch
    , PrimSift(..)
    , Mangled
    , Unravelled
    )
import Darcs.Patch.Prim.Canonize ( canonizeFL )
import Darcs.Patch.Prim.Coalesce ( coalesce )
