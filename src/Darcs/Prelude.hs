-- Copyright (C) 2015 Ganesh Sittampalam
-- BSD3

{-
This module abstracts over the differences in the Haskell Prelude over
multiple GHC versions, hides some symbols that are exported by the
Prelude but clash with common names in the Darcs code, and re-exports
occasional functions from non-Prelude models that are used widely in Darcs.

Broadly it exports everything that the latest Prelude supports, minus the
things we explicitly exclude. Since we now use the NoImplicitPrelude extension,
every module must import it explicitly.

By convention everything from Darcs.Prelude is imported:

    import Darcs.Prelude

If necessary more things can be hidden in the 'Darcs.Prelude' import if they
clash with something local, but consider whether to either hide them
globally instead or to choose a different name for the local thing.

If something is needed from the Prelude that's hidden by default, then add
it to the Prelude import.
-}

module Darcs.Prelude
    ( module Prelude
    , module Control.Applicative
    , module Data.Kind
    , module Data.List
    , module Data.Monoid
    , Semigroup(..)
    , module Data.Traversable
    ) where

import Prelude hiding
    (
      -- because it's a good name for a PatchInfo
      pi
    ,
      -- because they're in the new Prelude but only in Control.Applicative
      -- in older GHCs
      Applicative(..), (<$>), (<*>)
    ,
      -- because it's in the new Prelude but only in Data.Monoid in older GHCs
      Monoid(..)
    ,
      -- because it's in the new Prelude but only in Data.Semigroup in older GHCs
      Semigroup(..)
    ,
      -- because it's in the new Prelude but only in Data.Traversable in older GHCs
      traverse
    ,
      -- because it's a good name for a patch log
      log
    ,
      -- used by the options system
      (^)
    ,
      -- used by various code for no particularly good reason
      lookup, pred
    )

import Control.Applicative ( Applicative(..), (<$>), (<*>) )
import Data.List ( foldl' )
import Data.Kind ( Type )
import Data.Monoid ( Monoid(..) )
import Data.Semigroup ( Semigroup(..) )
import Data.Traversable ( traverse )
