module Darcs.Patch.Invert
       ( Invert(..), invertFL, invertRL
       )
       where

import Darcs.Prelude

import Darcs.Patch.Witnesses.Ordered
    ( FL(..), RL(..), reverseFL, reverseRL, (:>)(..) )

-- | The 'invert' operation must be self-inverse, i.e. an involution:
--
-- prop> invert . invert = id
class Invert p where
    invert :: p wX wY -> p wY wX

invertFL :: Invert p => FL p wX wY -> RL p wY wX
invertFL NilFL = NilRL
invertFL (x:>:xs) = invertFL xs :<: invert x

invertRL :: Invert p => RL p wX wY -> FL p wY wX
invertRL NilRL = NilFL
invertRL (xs:<:x) = invert x :>: invertRL xs

instance Invert p => Invert (FL p) where
    invert = reverseRL . invertFL

instance Invert p => Invert (RL p) where
    invert = reverseFL . invertRL

instance Invert p => Invert (p :> p) where
  invert (a :> b) = invert b :> invert a
