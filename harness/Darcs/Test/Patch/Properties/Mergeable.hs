module Darcs.Test.Patch.Properties.Mergeable
    ( propConsistentTreeFlattenings
    , propConsistentReorderings
    , propResolutionsDontConflict
    , propResolutionsOrderIndependent
    , FromPrimT
    ) where

import Darcs.Prelude

import Data.Maybe ( catMaybes )
import Safe ( tailErr )

import Darcs.Patch.Conflict ( ConflictDetails(..), Unravelled )
import Darcs.Patch.Merge ( CleanMerge, mergeList )
import Darcs.Patch.Permutations ( permutationsRL, (=\~/=) )
import Darcs.Patch.RepoPatch
    ( ApplyState
    , Commute
    , Conflict(..)
    , Eq2
    , Merge
    , PrimOf
    , ShowPatchBasic
    )
import Darcs.Patch.Show ( showPatch )
import Darcs.Patch.Witnesses.Eq ( isIsEq )
import Darcs.Patch.Witnesses.Ordered ( RL(..), (:>)(..) )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..), Sealed2(..), unseal )
import Darcs.Patch.Witnesses.Show ( Show2 )
import Darcs.Util.Printer ( redText, text, vsep, ($$) )

import Darcs.Test.Patch.Arbitrary.Generic ( PrimBased )
import Darcs.Test.Patch.Arbitrary.PatchTree ( G2(..), Tree, flattenTree, mapTree )
import Darcs.Test.Patch.Merge.Checked ( CheckedMerge )
import Darcs.Test.Patch.RepoModel
    ( Fail
    , ModelOf
    , RepoApply
    , RepoModel
    , RepoState
    , eqModel
    , maybeFail
    , repoApply
    , showModel
    )
import Darcs.Test.Patch.Types.MergeableSequence
    ( MergeableSequence
    , mergeableSequenceToRL
    )
import Darcs.Test.Patch.WithState
import Darcs.Test.Util.TestResult ( TestResult, failed, rejected, succeeded )

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
                                 , Show2 (rp prim)
                                 , RepoApply (rp prim)
                                 )
                              => FromPrimT rp prim
                              -> Sealed (WithStartState model (Tree prim))
                              -> TestResult
propConsistentTreeFlattenings fromPrim' (Sealed (WithStartState start t)) =
  case flattenTree (mapTree fromPrim' t) of
    Sealed (G2 flat') ->
      -- Limit the number of tree flattenings to something sane, as
      -- the length of the original list can grow exponentially.
      let flat = take 20 flat' in
      case map (start `repoApply`) flat of
        rms ->
          if and $ zipWith assertEqualFst (zip rms flat) (tailErr $ zip rms flat)
            then succeeded
            else failed $ redText "oops"

-- | This property states that all reorderings of a sequence of patches,
-- when applied to the same state, give the same result state.
propConsistentReorderings
  :: ( RepoModel (ModelOf p)
     , RepoState (ModelOf p) ~ ApplyState p
     , CheckedMerge p
     , PrimBased p
     , RepoApply p
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
propResolutionsOrderIndependent
  :: ( Commute p
     , Conflict p
     , ShowPatchBasic p
     , Eq2 (PrimOf p)
     , Show2 (PrimOf p)
     , Commute (PrimOf p)
     )
  => (RL p :> RL p) wX wY
  -> TestResult
propResolutionsOrderIndependent (ctx :> ps) =
    check [withConflictParts cs qs | cs <- css, qs <- pss]
  where
    withConflictParts cs qs =
      (Sealed (cs :> qs), map conflictParts $ resolveConflicts cs qs)
    pss = permutationsRL ps
    css = permutationsRL ctx
    check [] = error "we should have at least one permutation!"
    check [_] = rejected
    check xs = eql xs
    eql [] = error "impossible"
    eql [_] = succeeded
    eql ((cps1,r1):(cps2,r2):rs)
      | listEqBy eqUnravelled r1 r2 = eql ((cps2,r2):rs)
      | otherwise =
          failed $ vsep
            [ redText "resolutions differ: r1="
            , text (show r1)
            , redText "r2="
            , text (show r2)
            , unseal displayPair cps1
            , text "versus"
            , unseal displayPair cps2
            ]
    displayPair (as :> bs) =
      vsep
        [ text "for context"
        , showPatch as
        , text "and patches"
        , showPatch bs
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
propResolutionsDontConflict
  :: ( Conflict p
     , ShowPatchBasic p
     , CleanMerge (PrimOf p)
     , ShowPatchBasic (PrimOf p)
     )
  => RL p wX wY
  -> TestResult
propResolutionsDontConflict patches =
  case mergeList $ catMaybes $ map conflictMangled $ resolveConflicts NilRL patches of
    Right _ -> succeeded
    Left (Sealed ps, Sealed qs) ->
      failed
        $ redText "resolutions conflict:"
        $$ showPatch ps
        $$ redText "conflicts with"
        $$ showPatch qs
        $$ redText "for sequence"
        $$ showPatch patches
