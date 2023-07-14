module Darcs.Patch.Commute
    ( Commute(..)
    , commuteFL
    , commuteRL
    , commuteRLFL
    , selfCommuter
    ) where

import Darcs.Prelude

import Darcs.Patch.CommuteFn
    ( CommuteFn
    , commuterIdFL
    , commuterRLId
    , commuterRLFL
    )
import Darcs.Patch.Witnesses.Ordered
    ( FL(..), RL(..), reverseFL, reverseRL,
    (:>)(..) )

{- | Class of patches that that can be commuted.

Instances should obey the following laws:

[commute-symmetry]

    prop> commute (p:>q) == Just (q':>p') <=> commute (q':>p') == Just (p':>q)

[invert-commute]

    If patches are invertible, then

    prop> commute (p:>q) == Just (q':>p') <=> commute (invert q:>invert p) == Just (invert p':>invert q')

The more general law

[square-commute]

    prop> commute (p:>q) == Just (q':>p') => commute (invert p:>q') == Just (q:>invert p')

is valid in general only provided we know (a priori) that @'commute' ('invert'
p':>'q')@ succeeds, in other words, that p and q are not in conflict with each
other. See "Darcs.Patch.CommuteNoConflicts" for an extended discussion.

-}
class Commute p where
    commute :: (p :> p) wX wY -> Maybe ((p :> p) wX wY)

instance Commute p => Commute (FL p) where
    {-# INLINE commute #-}
    commute (NilFL :> x) = Just (x :> NilFL)
    commute (x :> NilFL) = Just (NilFL :> x)
    commute (xs :> ys) = do
        ys' :> rxs' <- commuteRLFL (reverseFL xs :> ys)
        return $ ys' :> reverseRL rxs'

-- |'commuteRLFL' commutes an 'RL' past an 'FL'.
{-# INLINE commuteRLFL #-}
commuteRLFL :: Commute p => (RL p :> FL p) wX wY
            -> Maybe ((FL p :> RL p) wX wY)
commuteRLFL = commuterRLFL commute

instance Commute p => Commute (RL p) where
    {-# INLINE commute #-}
    commute (xs :> ys) = do
        fys' :> xs' <- commuteRLFL (xs :> reverseRL ys)
        return (reverseFL fys' :> xs')

-- |'commuteRL' commutes a RL past a single element.
{-# INLINE commuteRL #-}
commuteRL :: Commute p => (RL p :> p) wX wY -> Maybe ((p :> RL p) wX wY)
commuteRL = commuterRLId commute

-- |'commuteFL' commutes a single element past a FL.
{-# INLINE commuteFL #-}
commuteFL :: Commute p => (p :> FL p) wX wY -> Maybe ((FL p :> p) wX wY)
commuteFL = commuterIdFL commute

-- |Build a commuter between a patch and itself using the operation from the type class.
selfCommuter :: Commute p => CommuteFn p p
selfCommuter = commute
