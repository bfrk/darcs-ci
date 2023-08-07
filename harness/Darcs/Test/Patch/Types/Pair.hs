module Darcs.Test.Patch.Types.Pair ( Pair(..) ) where

import Darcs.Prelude

import Darcs.Patch.Witnesses.Ordered
import Darcs.Patch.Witnesses.Show

import Darcs.Test.Patch.RepoModel

newtype Pair p wX wY = Pair { getPair :: (p :> p) wX wY }
  deriving Show

instance Show2 p => Show2 (Pair p)

type instance ModelOf (Pair p) = ModelOf p
