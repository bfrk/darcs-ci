-- Copyright (C) 2002-2003,2007 David Roundy
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

module Darcs.Test.Patch.Properties.V1Set2
    ( propCommuteInverse, propPatchAndInverseIsIdentity
    , propSimpleSmartMergeGoodEnough, propCommuteEquivalency
    , propMergeValid, propInverseValid, propOtherInverseValid
    , propCommuteEitherOrder
    , propCommuteEitherWay, propCommuteTwice
    , propMergeIsCommutableAndCorrect, propMergeIsSwapable
    -- TODO: these are exported temporarily to mark them as used
    -- Figure out whether to enable or remove the tests.
    , propUnravelThreeMerge, propUnravelSeqMerge
    , propUnravelOrderIndependent
    ) where

import Prelude ()
import Darcs.Prelude

import Test.QuickCheck
import Data.Maybe ( isJust )

import Darcs.Test.Patch.Properties.Check ( Check, checkAPatch )

import Darcs.Patch ( invert, commute, merge )
import Darcs.Patch.Commute ( Commute )
import Darcs.Patch.Invert ( Invert )
import Darcs.Patch.V1.Commute ( unravel, merger )
import Darcs.Patch.Witnesses.Eq
import Darcs.Patch.Witnesses.Ordered
import Darcs.Patch.Witnesses.Sealed
    ( Sealed(Sealed), unseal
    , Sealed2(..)
    )
import Darcs.Patch.Witnesses.Unsafe

import Darcs.Test.Patch.Arbitrary.RepoPatchV1 (Patch)

propInverseValid :: Sealed2 (FL Patch) -> Bool
propInverseValid (Sealed2 p1) = checkAPatch (invert p1:>:p1:>:NilFL)
propOtherInverseValid :: Sealed2 (FL Patch) -> Bool
propOtherInverseValid (Sealed2 p1) = checkAPatch (p1:>:invert p1:>:NilFL)

propCommuteTwice :: Sealed2 (FL Patch :> FL Patch) -> Property
propCommuteTwice (Sealed2 (p1:>p2)) =
    (doesCommute p1 p2) ==> (Just (p1:>p2) == (commute (p1:>p2) >>= commute))

doesCommute :: (Eq2 p, Invert p, Commute p, Check p) => p wX wY -> p wY wZ -> Bool
doesCommute p1 p2 =
    commute (p1:>p2) /= Nothing && checkAPatch (p1:>:p2:>:NilFL)

propCommuteEquivalency :: Sealed2 (FL Patch :> FL Patch) -> Property
propCommuteEquivalency (Sealed2 (p1:>p2)) =
    (doesCommute p1 p2) ==>
    case commute (p1:>p2) of
    Just (p2':>p1') -> checkAPatch (p1:>:p2:>:invert p1':>:invert p2':>:NilFL)
    _ -> error "impossible case"

propCommuteEitherWay :: Sealed2 (FL Patch :> FL Patch) -> Property
propCommuteEitherWay (Sealed2 (p1:>p2)) =
    doesCommute p1 p2 ==> doesCommute (invert p2) (invert p1)

propCommuteEitherOrder :: Sealed2 (FL Patch :> FL Patch :> FL Patch) -> Property
propCommuteEitherOrder (Sealed2 (p1:>p2:>p3)) =
    checkAPatch (p1:>:p2:>:p3:>:NilFL) &&
    doesCommute p1 (p2+>+p3) &&
    doesCommute p2 p3 ==>
    case commute (p1:>p2) of
    Nothing -> False
    Just (p2':>p1') ->
        case commute (p1':>p3) of
        Nothing -> False
        Just (p3':>_) ->
            case commute (p2':>p3') of
            Nothing -> False
            Just (p3'' :> _) ->
                case commute (p2:>p3) of
                Nothing -> False
                Just (p3'a:>_) ->
                    case commute (p1:>p3'a) of
                    Just (p3''a:>_) -> isIsEq (p3''a =\/= p3'')
                    Nothing -> False

propPatchAndInverseIsIdentity :: Sealed2 (FL Patch :> FL Patch) -> Property
propPatchAndInverseIsIdentity (Sealed2 (p1:>p2)) =
    checkAPatch (p1:>:p2:>:NilFL) && (commute (p1:>p2) /= Nothing) ==>
    case commute (p1:>p2) of
    Just (p2':>_) -> case commute (invert p1:>p2') of
                    Nothing -> True -- This is a subtle distinction.
                    Just (p2'':>_) -> isIsEq (p2'' =\/= p2)
    Nothing -> error "impossible case"

propMergeIsCommutableAndCorrect :: Sealed2 (FL Patch :\/: FL Patch) -> Property
propMergeIsCommutableAndCorrect (Sealed2 (p1:\/:p2)) =
    checkAPatch (invert p1:>:p2:>:NilFL) ==>
    case merge (p2:\/:p1) of
    p1' :/\: p2' ->
        case commute (p1:>p2') of
        Nothing -> False
        Just (p2'':>p1'') -> isIsEq (p2'' =\/= p2) && isIsEq (p1' =/\= p1'')

propMergeIsSwapable :: Sealed2 (FL Patch :\/: FL Patch) -> Property
propMergeIsSwapable (Sealed2 (p1:\/:p2)) =
    checkAPatch (invert p1:>:p2:>:NilFL) ==>
    case merge (p2:\/:p1) of
    p1' :/\: p2' ->
           case merge (p1:\/:p2) of
           p2''' :/\: p1''' -> isIsEq (p1' =\/= p1''') && isIsEq (p2' =\/= p2''')

propMergeValid :: Sealed2 (FL Patch :\/: FL Patch) -> Property
propMergeValid (Sealed2 (p1:\/:p2)) =
    checkAPatch (invert p1:>:p2:>:NilFL) ==>
    case merge (p2:\/:p1) of
    _ :/\: p2' ->
        checkAPatch (invert p1:>:p2:>:invert p2:>:p1:>:p2':>:NilFL)

propSimpleSmartMergeGoodEnough :: Sealed2 (FL Patch :\/: FL Patch) -> Property
propSimpleSmartMergeGoodEnough (Sealed2 (p1:\/:p2)) =
    checkAPatch (invert p1:>:p2:>:NilFL) ==>
    case simpleSmartMerge (p1 :\/: p2) of
      Nothing -> True
      Just (Sealed p1'a)
       -> isJust ((do
                    p1o :> _ <- commute (p2 :> p1'a)
                    IsEq <- return $ p1o =\/= p1
                    Sealed p2'a <- simpleSmartMerge (p2 :\/: p1)
                    p2b :> p1'b <- commute (p1 :> p2'a)
                    IsEq <- return $ p2 =\/= p2b
                    IsEq <- return $ p1'a =\/= p1'b
                    return ()) :: Maybe ())

simpleSmartMerge :: (Commute p, Invert p) => (p :\/: p) wX wY -> Maybe (Sealed (p wY))
simpleSmartMerge (p1 :\/: p2) =
  case commute (invert p2 :> p1) of
  Just (p1':>_) -> Just (Sealed p1')
  Nothing -> Nothing

-- | The conflict resolution code (glump) begins by "unravelling" the merger
-- into a set of sequences of patches.  Each sequence of patches corresponds
-- to one non-conflicted patch that got merged together with the others.  The
-- result of the unravelling of a series of merges must obviously be
-- independent of the order in which those merges are performed.  This
-- unravelling code (which uses the unwind code mentioned above) uses probably
-- the second most complicated algorithm.  Fortunately, if we can successfully
-- unravel the merger, almost any function of the unravelled merger satisfies
-- the two constraints mentioned above that the conflict resolution code must
-- satisfy.
propUnravelThreeMerge :: Patch wX wY -> Patch wX wZ -> Patch wX wW -> Property
propUnravelThreeMerge p1 p2 p3 =
    checkAPatch (invert p1:>:p2:>:invert p2:>:p3:>:NilFL) ==>
    (unravel $ unseal unsafeCoercePEnd $ merger "0.0"
         (unseal unsafeCoercePEnd (merger "0.0" p2 p3))
         (unseal unsafeCoercePEnd (merger "0.0" p2 p1))) ==
    (unravel $ unseal unsafeCoercePEnd $ merger "0.0"
         (unseal unsafeCoercePEnd (merger "0.0" p1 p3))
         (unseal unsafeCoercePEnd (merger "0.0" p1 p2)))

propUnravelSeqMerge :: Patch wX wY -> Patch wX wZ -> Patch wZ wW -> Property
propUnravelSeqMerge p1 p2 p3 =
    checkAPatch (invert p1:>:p2:>:p3:>:NilFL) ==>
    (unravel $ unseal unsafeCoercePEnd $ merger "0.0"
                   p3
                   (unseal unsafeCoercePEnd $ merger "0.0" p2 p1)) ==
    (unravel $ unseal unsafeCoercePEnd $ merger "0.0"
                      (unseal unsafeCoercePEnd $ merger "0.0" p2 p1)
                      p3)

propUnravelOrderIndependent :: Patch wX wY -> Patch wX wZ -> Property
propUnravelOrderIndependent p1 p2 =
    checkAPatch (invert p1:>:p2:>:NilFL) ==>
    (unravel $ unseal unsafeCoercePEnd $ merger "0.0" p2 p1) ==
    (unravel $ unseal unsafeCoerceP $ merger "0.0" p1 p2)

-- |In order for merges to work right with commuted patches, inverting a patch
-- past a patch and its inverse had golly well better give you the same patch
-- back again.
propCommuteInverse :: Sealed2 (FL Patch :> FL Patch) -> Property
propCommuteInverse (Sealed2 (p1 :> p2)) =
    doesCommute p1 p2 ==> case commute (p1 :> p2) of
                           Nothing -> error "impossible case"
                           Just (_ :> p1') ->
                               case commute (p1' :> invert p2) of
                               Nothing -> False
                               Just (_ :> p1'') -> isIsEq (p1'' =/\= p1)
