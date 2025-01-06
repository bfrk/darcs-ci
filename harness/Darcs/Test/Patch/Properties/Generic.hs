--  Copyright (C) 2007 David Roundy
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2, or (at your option)
--  any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; see the file COPYING.  If not, write to
--  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
--  Boston, MA 02110-1301, USA.

{-# LANGUAGE ViewPatterns #-}
module Darcs.Test.Patch.Properties.Generic
    ( invertInvolution
    , inverseComposition
    , invertRollback
    , recommute
    , commuteInverses
    , effectPreserving
    , inverseDoesntCommute
    , permutivity
    , squareCommuteLaw
    , mergeEitherWay
    , formatRead
    , mergeEitherWayValid
    , mergeCommute
    , mergeConsistent
    , mergeArgumentsConsistent
    , coalesceEffectPreserving
    , coalesceCommute
    , notCoalesceAndCommute
    , PatchProperty
    , MergeProperty
    , SequenceProperty
    , SequencePairProperty
    , propPrimPairCoverage
    ) where

import Darcs.Prelude

import Darcs.Test.Patch.RepoModel
    ( ModelOf
    , RepoModel
    , RepoState
    , eqModel
    , maybeFail
    , repoApply
    , showModel
    )
import Darcs.Test.Util.TestResult
    ( TestResult
    , failed
    , maybeFailed
    , rejected
    , succeeded
    , classify
    )
import Darcs.Test.Patch.Arbitrary.Generic ( MightHaveDuplicate(..) )
import Darcs.Test.Patch.Properties.Check ( Check, checkAPatch )
import Darcs.Test.Patch.Types.Pair ( Pair(..) )
import Darcs.Test.Patch.WithState ( WithState(..) )

import Control.Monad ( msum )

import Darcs.Patch.Witnesses.Show ( Show2(..), show2 )
import Darcs.Patch.Read ( ReadPatch )
import Darcs.Patch.Show ( ShowPatchBasic, showPatch )
import Darcs.Patch ()
import Darcs.Patch.Apply ( ApplyState, ObjectIdOfPatch )
import Darcs.Patch.Commute ( Commute, commute, commuteFL )
import Darcs.Patch.CommuteFn ( CommuteFn )
import Darcs.Patch.Format ( FormatPatch(..) )
import Darcs.Patch.FileHunk
import Darcs.Patch.Merge ( Merge(merge) )
import Darcs.Patch.Read ( readPatch )
import Darcs.Test.Patch.RepoModel ( RepoApply )
import Darcs.Patch.Invert ( Invert(..) )
import Darcs.Patch.Prim (PrimCoalesce(..) )
import Darcs.Patch.Witnesses.Eq ( Eq2(..), EqCheck(..) )
import Darcs.Patch.Witnesses.Ordered
    ( (:/\:)(..)
    , (:>)(..)
    , (:\/:)(..)
    , FL(..)
    , RL(..)
    , mapFL
    )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..) )
import qualified Darcs.Util.Format as F ( toStrictByteString )
import Darcs.Util.Printer
    ( Doc
    , greenText
    , packedString
    , redText
    , text
    , userchunkPS
    , vcat
    , ($$)
    )
--import Darcs.ColorPrinter ( traceDoc )

import Test.QuickCheck (Property, checkCoverage, cover)

type PatchProperty p = forall wA wB. p wA wB -> TestResult
-- type PairProperty p = forall wA wB. (p :> p) wA wB -> TestResult
type MergeProperty p = forall wA wB. (FL p :\/: FL p) wA wB -> TestResult
type SequenceProperty p = forall wA wB. RL p wA wB -> TestResult
type SequencePairProperty p = forall wA wB. (RL p :> RL p) wA wB -> TestResult

-- * Classifiers

type Square p q wA wB wC wD = (p wA wB, q wB wD, q wA wC, p wC wD)

-- | Both patches are unmodified
trivial_square :: (Eq2 p, Eq2 q) => Square p q wA wB wC wD -> Bool
trivial_square (x1, y1, y2, x2) =
  x1 `unsafeCompare` x2 && y1 `unsafeCompare` y2

classify_nontrivial_square
  :: (Eq2 p, Eq2 q) => Square p q wA wB wC wD -> TestResult -> TestResult
classify_nontrivial_square x =
  classify (not (trivial_square x)) "nontrivial"

-- | A commuting 'Cube' with all witnesses existentially hidden except those
-- corresponding to the start state and the end state. The witnesses are named
-- after the set of patches preceding it, with @wO@ corresponding to the origin
-- and e.g. @wXY@ corresponding to the state after applying @x@ and @y@.
data Cube p q r wO wXYZ where
  Cube
    :: p wO wX -> q wX wXY -> r wXY wXYZ  -- x, y, z
    -> p wY wXY -> q wO wY -> r wY wYZ    -- x', ...
    -> p wYZ wXYZ -> q wZ wYZ -> r wO wZ  -- x'', ...
    -> p wZ wXZ -> q wXZ wXYZ -> r wX wXZ -- x''', ...
    -> Cube p q r wO wXYZ

-- | A 'Cube' is nontrivial (by definition) if, geometrically speaking,  for
-- all three sets of parallel edges, not all of them are structurally equal.
--
-- In terms of patches, this means that all commutations involving the "same"
-- pair of patches contain at least one nontrivial
nontrivial_cube :: (Eq2 p, Eq2 q, Eq2 r) => Cube p q r wO wXYZ -> String
nontrivial_cube (Cube x y z x' y' z' x'' y'' z'' x''' y''' z''') =
 ("triviality index (0-3): " ++) $ show $ length $ filter id
    [ trivial x x' x'' x'''
    , trivial y y' y'' y'''
    , trivial z z' z'' z'''
    ]
  where
    trivial p p' p'' p''' =
      and
        [ unsafeCompare p p'
        , unsafeCompare p p''
        , unsafeCompare p p'''
        , unsafeCompare p' p''
        , unsafeCompare p' p'''
        , unsafeCompare p'' p'''
        ]

classify_nontrivial_cube
  :: (Eq2 p, Eq2 q, Eq2 r) => Cube p q r wO wXYZ -> TestResult -> TestResult
classify_nontrivial_cube x =
  classify True (nontrivial_cube x)

-- * Helpers

displayPatchFL :: ShowPatchBasic p => FL p wX wY -> Doc
displayPatchFL = vcat . mapFL showPatch

-- * Properties

-- | @A^^=A@
invertInvolution :: (Invert p, Eq2 p, ShowPatchBasic p) => p wA wB -> TestResult
invertInvolution p =
  let p' = invert (invert p)
  in case p =\/= p' of
    IsEq  -> succeeded
    NotEq ->
      failed $ redText "p /= p^^, where"
      $$ text "##p=" $$ showPatch p
      $$ text "##p^^=" $$ showPatch p'

-- | @(AB)^ = B^A^@
inverseComposition :: (Invert p, Eq2 p, ShowPatchBasic p)
                   => Pair p wX wY -> TestResult
inverseComposition (Pair (a :> b)) =
  let ab = a:>:b:>:NilFL
      iab = invert ab
      ibia = invert b:>:invert a:>:NilFL
  in case iab =\/= ibia of
    IsEq -> succeeded
    NotEq ->
      failed $ redText "ab^ /= b^a^, where"
      $$ text "##ab=" $$ displayPatchFL ab
      $$ text "##(ab)^=" $$ displayPatchFL iab
      $$ text "##b^a^=" $$ displayPatchFL ibia

-- | @ apply A x = y ==> apply A^ y = x@
invertRollback
  :: ( ApplyState p ~ RepoState model
     , Invert p
     , ShowPatchBasic p
     , RepoModel model
     , model ~ ModelOf p
     , RepoApply p
     )
  => WithState p wA wB
  -> TestResult
invertRollback (WithState a x b) =
  case maybeFail $ repoApply b (invert x) of
    Nothing -> failed $ redText "x^ not applicable to b."
    Just a' ->
      if a' `eqModel` a
        then
          succeeded
        else
          failed $
            redText "##original repo a:" $$ text (showModel a) $$
            redText "##with patch x:" $$ showPatch x $$
            redText "##results in b:" $$ text (showModel b) $$
            redText "##but (invert x):" $$ showPatch (invert x) $$
            redText "##applied to b is a':" $$ text (showModel a') $$
            redText "##which is not equal to a."

-- | recommute   AB ↔ B′A′ if and only if B′A′ ↔ AB
recommute :: (ShowPatchBasic p, Eq2 p, MightHaveDuplicate p)
          => CommuteFn p p
          -> Pair p wA wB -> TestResult
recommute c (Pair (x :> y)) =
    case c (x :> y) of
    Nothing -> rejected
    Just (y' :> x')
      -- this test unfortunately fails on some V2 patches that contain duplicates
      -- after the commute. While in theory the underlying bug should be fixed,
      -- we don't know how to and even if we did, it would probably involve a repository
      -- migration to a new patch type.
      | hasDuplicate y' || hasDuplicate x' -> rejected
      | otherwise ->
       classify_nontrivial_square (x, y, y', x') $
       case c (y' :> x') of
         Nothing -> failed (redText "failed, where x" $$ showPatch x $$
                              redText ":> y" $$ showPatch y $$
                              redText "y'" $$ showPatch y' $$
                              redText ":> x'" $$ showPatch x')
         Just (x'' :> y'') ->
             case y'' =/\= y of
             NotEq -> failed (redText "y'' =/\\= y failed, where x" $$ showPatch x $$
                              redText ":> y" $$ showPatch y $$
                              redText "y'" $$ showPatch y' $$
                              redText ":> x'" $$ showPatch x' $$
                              redText "x''" $$ showPatch x'' $$
                              redText ":> y''" $$ showPatch y'')
             IsEq -> case x'' =/\= x of
                     NotEq -> failed (
                              redText "x'' /= x, where x" $$ showPatch x $$
                              redText ":> y" $$ showPatch y $$
                              redText "y'" $$ showPatch y' $$
                              redText ":> x'" $$ showPatch x' $$
                              redText "x''" $$ showPatch x'' $$
                              redText ":> y''" $$ showPatch y'')
                     IsEq -> succeeded

-- | commuteInverses   AB ↔ B′A′ if and only if B⁻¹A⁻¹ ↔ A′⁻¹B′⁻¹
commuteInverses :: (Invert p, ShowPatchBasic p, Eq2 p)
                => CommuteFn p p
                -> Pair p wA wB -> TestResult
commuteInverses c (Pair (x :> y)) =
    case c (x :> y) of
    Nothing ->
      -- check that inverse commute neither
      case c (invert y :> invert x) of
        Just _ -> failed $
          redText "second commute did not fail"
          $$ redText "x" $$ showPatch x
          $$ redText "y" $$ showPatch y
          $$ redText "invert y" $$ showPatch (invert y)
          $$ redText "invert x" $$ showPatch (invert x)
        Nothing -> succeeded
    Just (y' :> x') ->
        classify_nontrivial_square (x, y, y', x') $
        case c (invert y :> invert x) of
        Nothing -> failed $ redText "second commute failed" $$
                            redText "x" $$ showPatch x $$ redText "y" $$ showPatch y $$
                            redText "invert y" $$ showPatch (invert y) $$ redText "invert x" $$ showPatch (invert x)
        Just (ix' :> iy') ->
            case invert ix' =/\= x' of
            NotEq -> failed $ redText "invert ix' /= x'" $$
                              redText "x" $$ showPatch x $$
                              redText "y" $$ showPatch y $$
                              redText "y'" $$ showPatch y' $$
                              redText "x'" $$ showPatch x' $$
                              redText "ix'" $$ showPatch ix' $$
                              redText "iy'" $$ showPatch iy' $$
                              redText "invert ix'" $$ showPatch (invert ix') $$
                              redText "invert iy'" $$ showPatch (invert iy')
            IsEq -> case y' =\/= invert iy' of
                    NotEq -> failed $ redText "y' /= invert iy'" $$ showPatch iy' $$ showPatch y'
                    IsEq -> succeeded

-- | effect preserving: @AB <--> B'A' => apply(AB) = apply(B'A')@
effectPreserving
  :: ( RepoModel model
     , model ~ ModelOf p
     , ApplyState p ~ RepoState model
     , ShowPatchBasic p
     , RepoApply p
     , Eq2 p
     )
  => CommuteFn p p
  -> WithState (Pair p) wA wB
  -> TestResult
effectPreserving c (WithState r (Pair (x :> y)) r') =
  case c (x :> y) of
    Nothing -> rejected
    Just (y' :> x') ->
      classify_nontrivial_square (x, y, y', x') $
      case maybeFail $ repoApply r y' of
        Nothing ->
          failed
          $  redText "##x" $$ showPatch x
          $$ redText "##y" $$ showPatch y
          $$ redText "##y'" $$ showPatch y'
          $$ redText "##x'" $$ showPatch x'
          $$ redText "##y' is not applicable to r"
          $$ displayModel r
        Just r_y' ->
          case maybeFail $ repoApply r_y' x' of
            Nothing ->
              failed
              $  redText "##x" $$ showPatch x
              $$ redText "##y" $$ showPatch y
              $$ redText "##y'" $$ showPatch y'
              $$ redText "##x'" $$ showPatch x'
              $$ redText "##x' is not applicable to r_y'"
              $$ displayModel r_y'
            Just r_y'x' ->
              if r_y'x' `eqModel` r'
                then succeeded
                else
                  failed
                  $  redText "##x" $$ showPatch x
                  $$ redText "##y" $$ showPatch y
                  $$ redText "##y'" $$ showPatch y'
                  $$ redText "##x'" $$ showPatch x'
                  $$ redText "##r_y'x'"
                  $$ displayModel r_y'x'
                  $$ redText "##is not equal to r'"
                  $$ displayModel r'
  where
    displayModel = text . showModel

-- | squareCommuteLaw   If AB ↔ B′A′ then A⁻¹B′ ↔ BA′⁻¹
squareCommuteLaw
  :: (Invert p, ShowPatchBasic p, Eq2 p)
  => CommuteFn p p
  -> Pair p wA wB
  -> TestResult
squareCommuteLaw c (Pair (x :> y)) =
  case c (x :> y) of
    Nothing -> rejected
    Just (y' :> x') ->
      classify_nontrivial_square (x, y, y', x') $
      case c (invert x :> y') of
        Nothing ->
          failed $
          redText "-------- original (x :> y)" $$
          showPatch x $$ redText ":>" $$ showPatch y $$
          redText "-------- result (y' :> x')" $$
          showPatch y' $$ redText ":>" $$ showPatch x' $$
          redText "-------- failed commute (invert x :> y')" $$
          showPatch (invert x) $$ redText ":>" $$ showPatch y'
        Just (y'' :> ix') ->
          case y'' =\/= y of
            NotEq ->
              failed $ redText "y'' /= y" $$
              redText "x" $$ showPatch x $$
              redText "y" $$ showPatch y $$
              redText "y'" $$ showPatch y' $$
              redText "x'" $$ showPatch x' $$
              redText "y''" $$ showPatch y'' $$
              redText "ix'" $$ showPatch ix'
            IsEq ->
              case x' =\/= invert ix' of
                NotEq ->
                  failed $ redText "x' /= invert ix'" $$
                  redText "x" $$ showPatch x $$
                  redText "y" $$ showPatch y $$
                  redText "y'" $$ showPatch y' $$
                  redText "x'" $$ showPatch x' $$
                  redText "invert x" $$ showPatch (invert x) $$
                  redText "y'" $$ showPatch y' $$
                  redText "invert ix'" $$ showPatch (invert ix')
                IsEq -> succeeded

permutivity :: (ShowPatchBasic p, Eq2 p)
            => CommuteFn p p
            -> (p :> p :> p) wA wB -> TestResult
permutivity c (x :> y :> z) =
  case c (x :> y) of
   Nothing -> rejected
   Just (y1 :> x1) ->
    case c (y :> z) of
    Nothing -> rejected
    Just (z2 :> y2) ->
      case c (x :> z2) of
      Nothing ->
        case c (x1 :> z) of
          Just _ -> failed $ redText "##partial permutivity:" $$
            redText "##x" $$ showPatch x $$
            redText "##y" $$ showPatch y $$
            redText "##z" $$ showPatch z $$
            redText "##y1" $$ showPatch y1 $$
            redText "##x1" $$ showPatch x1 $$
            redText "##z2" $$ showPatch z2 $$
            redText "##y2" $$ showPatch y2 $$
            redText "##x :> z2 does not commute, whereas x1 :> z does"
          Nothing -> classify True "partial" succeeded
      Just (z3 :> x3) ->
        case c (x1 :> z) of
          Nothing ->
            failed $ redText "##permutivity1:" $$
              redText "##x" $$ showPatch x $$
              redText "##y" $$ showPatch y $$
              redText "##z" $$ showPatch z $$
              redText "##y1" $$ showPatch y1 $$
              redText "##y2" $$ showPatch y2 $$
              redText "##failed commute with z of" $$
              redText "##x1" $$ showPatch x1 $$
              redText "##whereas x commutes with" $$
              redText "##z2" $$ showPatch z2
          Just (z4 :> x4) ->
            --traceDoc (greenText "third commuted" $$
            --          greenText "about to commute" $$
            --          greenText "y1" $$ showPatch y1 $$
            --          greenText "z4" $$ showPatch z4) $
            case c (y1 :> z4) of
            Nothing ->
              failed $ redText "##permutivity2:" $$
                redText "##failed to commute y1 with z4, where" $$
                redText "##x" $$ showPatch x $$
                redText "##y" $$ showPatch y $$
                redText "##z" $$ showPatch z $$
                redText "##y1" $$ showPatch y1 $$
                redText "##x1" $$ showPatch x1 $$
                redText "##z2" $$ showPatch z2 $$
                redText "##y2" $$ showPatch y2 $$
                redText "##z3" $$ showPatch z3 $$
                redText "##x3" $$ showPatch x3 $$
                redText "##z4" $$ showPatch z4 $$
                redText "##x4" $$ showPatch x4
            Just (z3_ :> y4)
                | IsEq <- z3_ =\/= z3 ->
                     --traceDoc (greenText "passed z3") $ error "foobar test" $
                     case c (y4 :> x4) of
                     Nothing -> failed $
                        redText "##permutivity5: input was" $$
                        redText "##x" $$ showPatch x $$
                        redText "##y" $$ showPatch y $$
                        redText "##z" $$ showPatch z $$
                        redText "##z3" $$ showPatch z3 $$
                        redText "##z4" $$ showPatch z4 $$
                        redText "##failed commute of" $$
                        redText "##y4" $$ showPatch y4 $$
                        redText "##x4" $$ showPatch x4 $$
                        redText "##whereas commute of x and y give" $$
                        redText "##y1" $$ showPatch y1 $$
                        redText "##x1" $$ showPatch x1
                     Just (x3_ :> y2_)
                          | NotEq <- x3_ =\/= x3 ->
                              failed $
                                redText "##permutivity6: x3_ /= x3" $$
                                redText "##x3_" $$ showPatch x3_ $$
                                redText "##x3" $$ showPatch x3
                          | NotEq <- y2_ =/\= y2 ->
                              failed $
                                redText "##permutivity7: y2_ /= y2" $$
                                redText "##y2_" $$ showPatch y2_ $$
                                redText "##y2" $$ showPatch y2
                          | otherwise ->
                              classify_nontrivial_cube
                                (Cube y1 x1 z y x z2 y2 x3 z3 y4 x4 z4) succeeded
                | otherwise ->
                    failed $ redText "##permutivity failed" $$
                             redText "##z3" $$ showPatch z3 $$
                             redText "##z3_" $$ showPatch z3_

mergeArgumentsConsistent :: (ShowPatchBasic p) =>
                              (forall wX wY . p wX wY -> Maybe Doc)
                           -> (p :\/: p) wA wB -> TestResult
mergeArgumentsConsistent isConsistent (x :\/: y) =
  maybeFailed $
    msum [(\z -> redText "mergeArgumentsConsistent x" $$ showPatch x $$ z) `fmap` isConsistent x,
          (\z -> redText "mergeArgumentsConsistent y" $$ showPatch y $$ z) `fmap` isConsistent y]

mergeConsistent
  :: (Eq2 p, ShowPatchBasic p, Merge p)
  => (forall wX wY . p wX wY -> Maybe Doc)
  -> (p :\/: p) wA wB
  -> TestResult
mergeConsistent isConsistent (x :\/: y) =
    case merge (x :\/: y) of
    y' :/\: x' ->
      classify_nontrivial_square (x, y', y, x') $
      maybeFailed $
        msum [(\z -> redText "mergeConsistent x" $$ showPatch x $$ z) `fmap` isConsistent x,
              (\z -> redText "mergeConsistent y" $$ showPatch y $$ z) `fmap` isConsistent y,
              (\z -> redText "mergeConsistent x'" $$ showPatch x' $$ z $$
                     redText "where x' comes from x" $$ showPatch x $$
                     redText "and y" $$ showPatch y) `fmap` isConsistent x',
              (\z -> redText "mergeConsistent y'" $$ showPatch y' $$ z) `fmap` isConsistent y']

-- merge (A\/B) = B'/\A' <==> merge (B\/A) = A'/\B'
--  or, equivalently,
-- merge . swap_par = swap_antipar . merge
--  where swap_par  (A\/B) = B\/A and swap_antipar (A/\B) = B/\A
-- It should not be needed to test this, since it follows from
-- mergeCommute and recommute.
mergeEitherWay :: (Eq2 p, ShowPatchBasic p, Merge p)
               => (p :\/: p) wX wY -> TestResult
mergeEitherWay (x :\/: y) =
  case merge (x :\/: y) of
    y' :/\: x' ->
      classify_nontrivial_square (x, y', y, x') $
      case merge (y :\/: x) of
        x'' :/\: y''
          | IsEq <- x'' =\/= x'
          , IsEq <- y'' =\/= y' -> succeeded
          | otherwise ->
            failed $
              redText "##x" $$ showPatch x $$
              redText "##y" $$ showPatch y $$
              redText "##y'" $$ showPatch y' $$
              redText "##x'" $$ showPatch x' $$
              redText "##x''" $$ showPatch x'' $$
              redText "##y''" $$ showPatch y'' $$
              redText "##x'' /= x' or y'' /= y'"

-- | @merge (A\/B) = B'/\A' ==> AB' <--> BA'@
mergeCommute :: (Eq2 p, ShowPatchBasic p, Commute p, Merge p, MightHaveDuplicate p)
             => (p :\/: p) wX wY -> TestResult
mergeCommute (x :\/: y) =
    case merge (x :\/: y) of
    y' :/\: x'
     -- this test unfortunately fails on some V2 patches that contain duplicates
     -- after the merge. While in theory the underlying bug should be fixed,
     -- we don't know how to and even if we did, it would probably involve a repository
     -- migration to a new patch type.
     | hasDuplicate x' || hasDuplicate y' -> rejected
     | otherwise ->
        classify_nontrivial_square (x, y', y, x') $
        case commute (x :> y') of
        Nothing -> failed $ redText "mergeCommute 1" $$
                            redText "x" $$ showPatch x $$
                            redText "y" $$ showPatch y $$
                            redText "x'" $$ showPatch x' $$
                            redText "y'" $$ showPatch y'
        Just (y_ :> x'_)
            | IsEq <- y_ =\/= y,
              IsEq <- x'_ =\/= x' ->
                      case commute (y :> x') of
                      Nothing -> failed $ redText "mergeCommute 2 failed" $$
                                          redText "x" $$ showPatch x $$
                                          redText "y" $$ showPatch y $$
                                          redText "x'" $$ showPatch x' $$
                                          redText "y'" $$ showPatch y'
                      Just (x_ :> y'_)
                           | IsEq <- x_ =\/= x,
                             IsEq <- y'_ =\/= y' -> succeeded
                           | otherwise -> failed $ redText "mergeCommute 3" $$
                                                   redText "x" $$ showPatch x $$
                                                   redText "y" $$ showPatch y $$
                                                   redText "x'" $$ showPatch x' $$
                                                   redText "y'" $$ showPatch y' $$
                                                   redText "x_" $$ showPatch x_ $$
                                                   redText "y'_" $$ showPatch y'_
            | otherwise -> failed $ redText "mergeCommute 4" $$
                                    redText "x" $$ showPatch x $$
                                    redText "y" $$ showPatch y $$
                                    redText "x'" $$ showPatch x' $$
                                    redText "y'" $$ showPatch y' $$
                                    redText "x'_" $$ showPatch x'_ $$
                                    redText "y_" $$ showPatch y_


-- | Coalescing is effect preserving
coalesceEffectPreserving
  :: ( ShowPatchBasic prim
     , ApplyState prim ~ RepoState (ModelOf prim)
     , RepoModel (ModelOf prim)
     , RepoApply prim
     )
  => (forall wX wY . (prim :> prim) wX wY -> Maybe (FL prim wX wY))
  -> WithState (Pair prim) wA wB
  -> TestResult
coalesceEffectPreserving j (WithState r (Pair (a :> b)) r') =
  case j (a :> b) of
       Nothing -> rejected
       Just x  -> case maybeFail $ repoApply r x of
                       Nothing  -> failed $ redText "x is not applicable to r."
                                        $$ text (showModel r)
                                        $$ showPatch x
                                        $$ redText "a:>b"
                                        $$ showPatch a $$ showPatch b
                                        $$ redText "r'="
                                        $$ text (showModel r')
                       Just r_x -> if r_x `eqModel` r'
                                      then succeeded
                                      else failed $ redText "r_x /= r', r="
                                        $$ text (showModel r)
                                        $$ redText "a:>b="
                                        $$ showPatch a $$ showPatch b
                                        $$ redText "x="
                                        $$ showPatch x
                                        $$ redText "r'="
                                        $$ text (showModel r')
                                        $$ redText "r_x="
                                        $$ text (showModel r_x)

-- | Just X = coalesce (BC), A(BC) <--> (B'C')A', AX <--> X'A''
--   ==>
--   A' = A'', coalesce (B'C') = Just X'
coalesceCommute
  :: (Commute prim, Eq2 prim, ShowPatchBasic prim)
  => (forall wX wY . (prim :> prim) wX wY -> Maybe (FL prim wX wY))
  -> (prim :> prim :> prim) wA wB
  -> TestResult
coalesceCommute j (a :> b :> c) =
    case j (b :> c) of
    Nothing -> rejected
    Just x  ->
      case commuteFL (a :> b :>: c :>: NilFL) of
        Just (b' :>: c' :>: NilFL :> a') ->
          case commuteFL (a :> x) of
            Just (x' :> a'') ->
              case a'' =/\= a' of
                NotEq ->
                  failed $ greenText "a'' =/\\= a' failed"
                    $$ display1
                    $$ display2
                IsEq ->
                  case j (b' :> c') of
                    Nothing ->
                      failed $ greenText "coalesce (b':>c') failed"
                        $$ display1
                        $$ display2
                    Just x'' ->
                      case x' =\/= x'' of
                        NotEq ->
                          failed $ greenText "x' =\\/= x'' failed"
                            $$ display1
                            $$ display2
                            $$ display3
                        IsEq -> succeeded
                      where
                        display3 = redText "## coalesce (b':>c') => x''"
                                   $$ showPatch x''
              where
                display2 =
                     redText "## commute (a:>x) => x'" $$ showPatch x'
                  $$ redText "## :> a''" $$ showPatch a''
            _ -> failed $ greenText "commute a x failed" $$ display1
          where
            display1 =
                 redText "## a" $$ showPatch a
              $$ redText "## b" $$ showPatch b
              $$ redText "## c" $$ showPatch c
              $$ redText "## coalesce (b:>c) => x" $$ showPatch x
              $$ redText "## commute (a:>b:>c) => a'" $$ showPatch a'
              $$ redText "## b'" $$ showPatch b'
              $$ redText "## c'" $$ showPatch c'
        _ -> rejected

formatRead :: (Show2 p, Eq2 p, ReadPatch p, FormatPatch p) => p wA wB -> TestResult
formatRead p =
  let ps = F.toStrictByteString (formatPatch p)
   in case readPatch ps of
        Left e -> failed (redText "unable to read " $$ packedString ps $$ text e)
        Right (Sealed p')
          | IsEq <- p' =\/= p -> succeeded
          | otherwise ->
            failed $ vcat
              [ redText "patch p"
              , text (show2 p)
              , redText "serialized as"
              , userchunkPS ps
              , redText "reads as p'"
              , text (show2 p')
              ]

-- vim: fileencoding=utf-8 :

mergeEitherWayValid
  :: (Check p, Eq2 p, Merge p, Invert p, ShowPatchBasic p)
  => (p :\/: p) wX wY
  -> TestResult
mergeEitherWayValid (p1 :\/: p2) =
  case merge (p1 :\/: p2) of
    p2' :/\: p1' ->
      classify_nontrivial_square (p1, p2', p2, p1') $
      case p2 :>: p1' :>: NilFL of
        combo2 ->
          case merge (p2 :\/: p1) of
            _ :/\: p2'' ->
              case p1 :>: p2'' :>: NilFL of
                combo1
                  | not $ checkAPatch combo1 ->
                      failed $ text "combo1 invalid: p1="
                      $$ showPatch p1
                      $$ text "p2="
                      $$ showPatch p2
                      $$ text "combo1="
                      $$ vcat (mapFL showPatch combo1)
                  | checkAPatch (invert combo1 :>: combo2 :>: NilFL) ->
                      succeeded
                  | otherwise ->
                      failed $ text "merge both ways invalid: p1="
                      $$ showPatch p1
                      $$ text "p2="
                      $$ showPatch p2
                      $$ text "combo1="
                      $$ vcat (mapFL showPatch combo1)
                      $$ text "combo2="
                      $$ vcat (mapFL showPatch combo2)

inverseDoesntCommute :: (ShowPatchBasic p, Invert p, Commute p)
                     => p wY1 wY2 -> TestResult
inverseDoesntCommute x =
  case commute (x :> invert x) of
    Nothing -> succeeded
    Just (ix' :> x') -> failed $ redText "x:" $$ showPatch x
      $$ redText "commutes with x^ to ix':" $$ showPatch ix'
      $$ redText "x':" $$ showPatch x'

-- | This property states that two patches cannot both commute and coalesce.
-- It has a single exception for Prim.V1, namely adjacent
-- hunks that both add and remove lines.
notCoalesceAndCommute
  :: (Eq (ObjectIdOfPatch p), IsHunk p, PrimCoalesce p, ShowPatchBasic p)
  => Pair p wX wY -> TestResult
notCoalesceAndCommute (Pair pair@(p1 :> p2))
  | Just (FileHunk _ f1 l1 (length -> o1) (length -> n1)) <- isHunk p1
  , Just (FileHunk _ f2 l2 (length -> o2) (length -> n2)) <- isHunk p2
  , f1 == f2
  , l1 + n1 == l2 || l2 + o2 == l1
  , o1 > 0, n1 > 0, o2 > 0, n2 > 0 = rejected
  | Just _ <- commute pair
  , Just _ <- primCoalesce p1 p2 =
      failed $
        text "patches coalesce and commute:" $$
        showPatch p1 $$
        showPatch p2
  | otherwise = succeeded

-- This property is just to check the coverage of pairs,
-- it doesn't test any actual property.
propPrimPairCoverage :: forall prim wX wY . (Eq2 prim, Commute prim) => Pair prim wX wY -> Property
propPrimPairCoverage (Pair pq) =
  checkCoverage $
  -- The coverage percentages should pass reliably, but
  -- could be dropped a bit if not.
  let theKind = classifyCommute pq (commute pq) in
    cover 20 (theKind == Failed) "Not Commutable" $
    cover 60 (theKind /= Failed) "Commutable" $
    cover 20 (theKind == Changed) "Representation Changed" $
    True

data CommuteKind = Failed | Unchanged | Changed
  deriving (Eq, Show)

classifyCommute :: Eq2 prim => (prim :> prim) wX wY -> Maybe ((prim :> prim) wX wY) -> CommuteKind
classifyCommute _ Nothing = Failed
classifyCommute (p :> q) (Just (q' :> p'))
  | unsafeCompare p p' && unsafeCompare q q' = Unchanged
  | otherwise = Changed
