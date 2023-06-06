module Darcs.Test.Patch.Properties.RepoPatch
    ( propConsistentTreeFlattenings
    , propConsistentReorderings
    , propResolutionsDontConflict
    , propResolutionsOrderIndependent
    , FromPrimT
    ) where

import Prelude ()
import Darcs.Prelude

import Data.Maybe ( catMaybes )

import Darcs.Test.Patch.Arbitrary.Generic
  ( MergeableSequence, mergeableSequenceToRL, PrimBased )
import Darcs.Test.Patch.Arbitrary.PatchTree
  ( Tree, flattenTree, G2(..), mapTree )
import Darcs.Test.Patch.Merge.Checked ( CheckedMerge )
import Darcs.Test.Patch.WithState
import Darcs.Test.Patch.RepoModel ( RepoModel, repoApply, showModel, eqModel, RepoState
                                  , Fail, maybeFail, ModelOf )
import Darcs.Test.Util.TestResult ( TestResult, failed, rejected, succeeded )

import Darcs.Util.Printer ( text, redText, ($$), vsep )

import Darcs.Patch.Conflict ( Conflict(..), ConflictDetails(..), Unravelled )
import Darcs.Patch.Apply ( Apply(..) )
import Darcs.Patch.Merge ( Merge, mergeList )
import Darcs.Patch.Permutations ( permutationsRL, (=\~/=) )
import Darcs.Patch.RepoPatch ( Commute, RepoPatch )
import Darcs.Patch.Show ( displayPatch )

import Darcs.Patch.Witnesses.Eq ( Eq2, isIsEq )
import Darcs.Patch.Witnesses.Ordered ( RL(..) )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..), unseal, Sealed2(..) )
import Darcs.Patch.Witnesses.Show ( Show2 )

assertEqualFst :: (RepoModel a, Show b, Show c) => (Fail (a x), b) -> (Fail (a x), c) -> Bool
assertEqualFst (x,bx) (y,by)
    | Just x' <- maybeFail x, Just y' <- maybeFail y, x' `eqModel` y' = True
    | Nothing <- maybeFail x, Nothing <- maybeFail y = True
    | otherwise = error ("Not really equal:\n" ++ showx ++ "\nand\n" ++ showy
                         ++ "\ncoming from\n" ++ show bx ++ "\nand\n" ++ show by)
      where showx | Just x' <- maybeFail x = showModel x'
                  | otherwise = "Nothing"
            showy | Just y' <- maybeFail y = showModel y'
                  | otherwise = "Nothing"

type FromPrimT rp p = forall wX wY. p wX wY -> rp p wX wY

-- | This property states that any flattening of a 'Tree' of prim patches,
-- when applied to the start state, produces the same end state.
propConsistentTreeFlattenings :: forall rp prim model.
                                 ( RepoModel model
                                 , RepoState model ~ ApplyState prim
                                 , ApplyState (rp prim) ~ ApplyState prim
                                 , Merge (rp prim)
                                 , Apply (rp prim)
                                 , Show2 (rp prim) )
                              => FromPrimT rp prim
                              -> Sealed (WithStartState model (Tree prim))
                              -> TestResult
propConsistentTreeFlattenings fromPrim (Sealed (WithStartState start t)) =
  case flattenTree (mapTree fromPrim t) of
    Sealed (G2 flat') ->
      -- Limit the number of tree flattenings to something sane, as
      -- the length of the original list can grow exponentially.
      let flat = take 20 flat' in
      case map (start `repoApply`) flat of
        rms ->
          if and $ zipWith assertEqualFst (zip rms flat) (tail $ zip rms flat)
            then succeeded
            else failed $ redText "oops"

-- | This property states that all reorderings of a sequence of patches,
-- when applied to the same state, give the same result state.
propConsistentReorderings :: ( RepoPatch p
                             , RepoModel (ModelOf p)
                             , RepoState (ModelOf p) ~ ApplyState p
                             , CheckedMerge p
                             , PrimBased p
                             )
                          => Sealed2 (WithStartState2 (MergeableSequence p))
                          -> TestResult
propConsistentReorderings (Sealed2 (WithStartState2 start ms)) =
  case mapM (repoApply start) $ permutationsRL ps of
    Left e -> failed $ redText "could not apply all reorderings:" $$ text (show e)
    Right [] -> error "we should have at least one permutation!"
    Right [_] -> rejected -- only one permutation -> nothing to test
    Right results -> eql results
  where
    eql [] = error "impossible"
    eql [_] = succeeded
    eql (r1:r2:rs)
      | r1 `eqModel` r2 = eql (r2:rs)
      | otherwise =
          failed
          $ redText "result states differ: r1="
          $$ text (showModel r1)
          $$ redText "r2="
          $$ text (showModel r2)
    ps = mergeableSequenceToRL ms

-- | This property states that the standard conflict resolutions for a
-- sequence of patches are independent of any reordering of the sequence.
propResolutionsOrderIndependent :: RepoPatch p => RL p wX wY -> TestResult
propResolutionsOrderIndependent ps =
    check $ map withConflictParts pss
  where
    withConflictParts qs =
      (Sealed qs, map conflictParts $ resolveConflicts NilRL qs)
    pss = permutationsRL ps
    check [] = error "we should have at least one permutation!"
    check [_] = rejected
    check xs = eql xs
    eql [] = error "impossible"
    eql [_] = succeeded
    eql ((ps1,r1):(ps2,r2):rs)
      | listEqBy eqUnravelled r1 r2 = eql ((ps2,r2):rs)
      | otherwise =
          failed $ vsep
            [ redText "resolutions differ: r1="
            , text (show r1)
            , redText "r2="
            , text (show r2)
            , text "for patches"
            , unseal displayPatch ps1
            , text "versus"
            , unseal displayPatch ps2
            ]

-- | Equality for 'Unravelled' is modulo order of patches.
eqUnravelled :: (Commute p, Eq2 p) => Unravelled p wX -> Unravelled p wX -> Bool
eqUnravelled = listEqBy eq where
  eq (Sealed ps) (Sealed qs) = isIsEq $ ps =\~/= qs

-- | Generic list equality with explicitly given comparison for elements.
listEqBy :: (a -> a -> Bool) -> [a] -> [a] -> Bool
listEqBy _ [] [] = True
listEqBy eq (x:xs) (y:ys) = x `eq` y && listEqBy eq xs ys
listEqBy _ _ _ = False

-- | This property states that the standard conflict resolutions for a
-- sequence of patches do not themselves conflict with each other.
propResolutionsDontConflict :: RepoPatch p => RL p wX wY -> TestResult
propResolutionsDontConflict patches =
  case mergeList $ catMaybes $ map conflictMangled $ resolveConflicts NilRL patches of
    Right _ -> succeeded
    Left (Sealed ps, Sealed qs) ->
      failed
        $ redText "resolutions conflict:"
        $$ displayPatch ps
        $$ redText "conflicts with"
        $$ displayPatch qs
        $$ redText "for sequence"
        $$ displayPatch patches
