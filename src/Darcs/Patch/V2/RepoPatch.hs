-- Copyright (C) 2007 David Roundy
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

{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Darcs.Patch.V2.RepoPatch
    ( RepoPatchV2(..)
    , isConsistent
    , isForward
    , isDuplicate
    , mergeUnravelled
    ) where

import Darcs.Prelude hiding ( (*>) )

import Control.Monad ( mplus, liftM )
import qualified Data.ByteString.Char8 as BC ( ByteString, pack )
import Data.Maybe ( fromMaybe )
import Data.List ( partition )
import Data.List.Ordered ( nubSort )

import Darcs.Patch.Commute ( commuteFL, commuteRL
                           , commuteRLFL, Commute(..) )
import Darcs.Patch.CommuteFn ( CommuteFn, invertCommuter )
import Darcs.Patch.CommuteNoConflicts ( CommuteNoConflicts(..), mergeNoConflicts )
import Darcs.Patch.Conflict ( Conflict(..), combineConflicts, mangleOrFail )
import Darcs.Patch.Debug
import Darcs.Patch.Effect ( Effect(..) )
import Darcs.Patch.FileHunk ( IsHunk(..) )
import Darcs.Patch.Format ( PatchListFormat(..), ListFormat(ListFormatV2) )
import Darcs.Patch.Ident ( PatchId )
import Darcs.Patch.Invert ( invertFL, invertRL, Invert(..) )
import Darcs.Patch.Merge ( CleanMerge(..), Merge(..), swapMerge )
import Darcs.Patch.FromPrim
    ( FromPrim(..)
    , ToPrim(..)
    , PrimPatchBase(..)
    )
import Darcs.Patch.Prim ( PrimPatch, applyPrimFL )
import Darcs.Patch.Read ( bracketedFL, ReadPatch(..) )
import Darcs.Util.Parser ( skipSpace, string, choice )
import Darcs.Patch.Repair ( mapMaybeSnd, RepairToFL(..), Check(..) )
import Darcs.Patch.Apply ( Apply(..) )
import Darcs.Patch.Inspect ( PatchInspect(..) )
import Darcs.Patch.Permutations ( commuteWhatWeCanFL, commuteWhatWeCanRL
                                , genCommuteWhatWeCanRL, removeRL, removeFL
                                , removeSubsequenceFL, nubFL, (=\~/=), (=/~\=) )
import Darcs.Patch.Show
    ( ShowPatch(..), ShowPatchBasic(..), ShowContextPatch(..), ShowPatchFor(..)
    , displayPatch )
import Darcs.Patch.Summary
    ( Summary(..)
    , ConflictState(..)
    , IsConflictedPrim(..)
    , plainSummary
    )
import Darcs.Patch.Unwind ( Unwind(..), mkUnwound )
import Darcs.Patch.V2.Non ( Non(..), Nonable(..), unNon, showNons, showNon
                          , readNons, readNon, commutePrimsOrAddToCtx
                          , commuteOrAddToCtx, commuteOrAddToCtxRL
                          , commuteOrRemFromCtx, commuteOrRemFromCtxFL
                          , remNons, (*>), (>*), (*>>), (>>*) )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP )
import Darcs.Patch.Witnesses.Eq ( Eq2(..), EqCheck(..) )
import Darcs.Patch.Witnesses.Ordered
    ( FL(..), RL(..), Fork(..), (:>)(..), (+>+), (+<+)
    , mapFL, mapFL_FL, reverseFL, (:\/:)(..), (:/\:)(..)
    , reverseRL, lengthFL, lengthRL, nullFL, initsFL )
import Darcs.Patch.Witnesses.Sealed
    ( FlippedSeal(..), Sealed(Sealed), mapSeal
    , unseal )
import Darcs.Patch.Witnesses.Show ( Show1, Show2, showsPrec2, appPrec )

import Darcs.Util.Path ( AnchoredPath )
import Darcs.Util.Printer ( Doc, renderString, blueText, redText, (<+>), ($$), vcat )

-- |'RepoPatchV2' is used to represents prim patches that are duplicates of, or
-- conflict with, another prim patch in the repository.
--
-- @Normal prim@: A primitive patch
--
-- @Duplicate x@: This patch has no effect since @x@ is already present in the
-- repository.
--
-- @Etacilpud x: invert (Duplicate x)@
--
-- @Conflictor ix xx x@:
-- @ix@ is the set of patches:
--   * that conflict with @x@ and also conflict with another patch in the
--     repository.
--   * that conflict with a patch that conflict with @x@
--
-- @xx@ is the sequence of patches that conflict *only* with @x@
--
-- @x@ is the original, conflicting patch.
--
-- @ix@ and @x@ are stored as @Non@ objects, which include any necessary
--  context to uniquely define the patch that is referred to.
--
-- The intuition is that a Conflictor should have the effect of inverting any
-- patches that 'x' conflicts with, that haven't already been undone by another
-- Conflictor in the repository.
-- Therefore, the effect of a Conflictor is @invert xx@.
--
-- @InvConflictor ix xx x@: like @invert (Conflictor ix xx x)@
data RepoPatchV2 prim wX wY where
    Duplicate :: Non (RepoPatchV2 prim) wX -> RepoPatchV2 prim wX wX
    Etacilpud :: Non (RepoPatchV2 prim) wX -> RepoPatchV2 prim wX wX
    Normal :: prim wX wY -> RepoPatchV2 prim wX wY
    Conflictor :: [Non (RepoPatchV2 prim) wX] -> FL prim wX wY
               -> Non (RepoPatchV2 prim) wX -> RepoPatchV2 prim wY wX
    InvConflictor :: [Non (RepoPatchV2 prim) wX] -> FL prim wX wY
                  -> Non (RepoPatchV2 prim) wX -> RepoPatchV2 prim wX wY

instance PrimPatch prim => PrimPatchBase (RepoPatchV2 prim) where
   type PrimOf (RepoPatchV2 prim) = prim

-- | 'isDuplicate' @p@ is @True@ if @p@ is either a 'Duplicate' or 'Etacilpud'
-- patch.
isDuplicate :: RepoPatchV2 prim wS wY -> Bool
isDuplicate (Duplicate _) = True
isDuplicate (Etacilpud _) = True
isDuplicate _ = False

-- | 'isForward' @p@ is @True@ if @p@ is either an 'InvConflictor' or
-- 'Etacilpud'.
isForward :: PrimPatch prim => RepoPatchV2 prim wS wY -> Maybe Doc
isForward p = case p of
    p@(InvConflictor{}) -> justRedP "An inverse conflictor" p
    p@(Etacilpud _) -> justRedP "An inverse duplicate" p
    _ -> Nothing
  where
    justRedP msg p = Just $ redText msg $$ displayPatch p

-- |'mergeUnravelled' is used when converting from Darcs V1 patches (Mergers)
-- to Darcs V2 patches (Conflictors).
mergeUnravelled :: PrimPatch prim => [Sealed ((FL prim) wX)]
                -> Maybe (FlippedSeal (RepoPatchV2 prim) wX)
mergeUnravelled [] = Nothing
mergeUnravelled [_] = Nothing
mergeUnravelled ws =
    case mergeUnravelled_private ws of
        Nothing -> Nothing
        Just NilRL -> error "found no patches in mergeUnravelled"
        Just (_ :<: z) -> Just $ FlippedSeal z
  where
    notNullS :: Sealed ((FL prim) wX) -> Bool
    notNullS (Sealed NilFL) = False
    notNullS _ = True

    mergeUnravelled_private :: PrimPatch prim => [Sealed (FL prim wX)]
                            -> Maybe (RL (RepoPatchV2 prim) wX wX)
    mergeUnravelled_private xs = let nonNullXs = filter notNullS xs in
        reverseFL `fmap` mergeConflictingNons (map sealed2non nonNullXs)

    -- | 'sealed2non' @(Sealed xs)@ converts @xs@ to a 'Non'.
    -- @xs@ must be non-empty since we split this list at the last patch,
    -- taking @init xs@ as the context of @last xs@.
    sealed2non :: Sealed ((FL prim) wX) -> Non (RepoPatchV2 prim) wX
    sealed2non (Sealed xs) =
        case reverseFL xs of
            ys :<: y -> Non (mapFL_FL Normal $ reverseRL ys) y
            NilRL -> error "NilFL encountered in sealed2non"

mergeConflictingNons :: PrimPatch prim => [Non (RepoPatchV2 prim) wX]
                     -> Maybe (FL (RepoPatchV2 prim) wX wX)
mergeConflictingNons ns = mcn $ map unNon ns
    where mcn :: PrimPatch prim => [Sealed (FL (RepoPatchV2 prim) wX)]
              -> Maybe (FL (RepoPatchV2 prim) wX wX)
          mcn [] = Just NilFL
          -- Apparently, the joinEffects call is a safety check "and could be
          -- removed when we're sure of the code"!
          mcn [Sealed p] = case joinEffects p of
                               NilFL -> Just p
                               _ -> Nothing
          mcn (Sealed p1:Sealed p2:zs) =
            case pullCommon p1 p2 of
                Fork c ps qs ->
                    case merge (ps :\/: qs) of
                        qs' :/\: _ -> mcn (Sealed (c +>+ ps +>+ qs'):zs)

joinEffects :: forall p wX wY . (Effect p, Invert (PrimOf p),
            Commute (PrimOf p), Eq2 (PrimOf p)) => p wX wY
            -> FL (PrimOf p) wX wY
joinEffects = joinInverses . effect
    where joinInverses :: FL (PrimOf p) wA wB -> FL (PrimOf p) wA wB
          joinInverses NilFL = NilFL
          joinInverses (p :>: ps) =
              let ps' = joinInverses ps in
              fromMaybe (p :>: ps') $ removeFL (invert p) ps'

assertConsistent :: PrimPatch prim => RepoPatchV2 prim wX wY
                 -> RepoPatchV2 prim wX wY
assertConsistent x = maybe x (error . renderString) $ do
    e <- isConsistent x
    Just (redText "Inconsistent patch:" $$ displayPatch x $$ e)

-- | @mergeAfterConflicting@ takes as input a sequence of conflicting patches
-- @xxx@ (which therefore have no effect) and a sequence of primitive patches
-- @yyy@ that follow said sequence of conflicting patches, and may depend upon
-- some of the conflicting patches (as a resolution).

-- The output is two sequences of patches the first consisting of a set of
-- mutually-conflicting patches, and the second having the same effect as the
-- original primitive patch sequence in the input.

-- So far as I can tell, the second output is always identical to @mapFL Normal
-- yyy@

-- The first output is the set of patches from @xxx@ that are depended upon by
-- @yyy@.
mergeAfterConflicting :: PrimPatch prim => FL (RepoPatchV2 prim) wX wX
                      -> FL prim wX wY -> Maybe ( FL (RepoPatchV2 prim) wX wX
                                                 , FL (RepoPatchV2 prim) wX wY)
mergeAfterConflicting xxx yyy = mac (reverseFL xxx) yyy NilFL
  where
    mac :: PrimPatch prim
        => RL (RepoPatchV2 prim) wX wY -> FL prim wY wZ
        -> FL (RepoPatchV2 prim) wZ wA
        -> Maybe (FL (RepoPatchV2 prim) wX wX, FL (RepoPatchV2 prim) wX wA)
    mac NilRL xs goneby = case joinEffects goneby of
                              NilFL -> Just (NilFL, mapFL_FL Normal xs)
                              _ -> Nothing
    mac (ps :<: p) xs goneby =
        case commuteFL (p :> mapFL_FL Normal xs) of
            Nothing ->
                case genCommuteWhatWeCanRL commuteNoConflicts (ps :> p) of
                    a :> p' :> b ->
                        do (b', xs') <- mac b xs goneby
                           let pa = joinEffects $ a :<: p'
                           NilFL <- return pa
                           return (reverseRL (a :<: p') +>+ b', xs')
                        `mplus`
                        do NilFL <- return goneby
                           NilFL <- return $ joinEffects (ps :<: p)
                           return (reverseRL (ps :<: p), mapFL_FL Normal xs)
            Just (l :> p'') ->
                case allNormal l of
                    Just xs'' -> mac ps xs'' (p'' :>: goneby)
                    Nothing ->
                        case genCommuteWhatWeCanRL commuteNoConflicts (ps :> p) of
                            a :> p' :> b ->
                                do (b', xs') <- mac b xs goneby
                                   let pa = joinEffects $ a :<: p'
                                   NilFL <- return pa
                                   return (reverseRL (a :<: p') +>+ b', xs')

geteff :: PrimPatch prim => [Non (RepoPatchV2 prim) wX] -> FL prim wX wY
       -> ([Non (RepoPatchV2 prim) wX], FL (RepoPatchV2 prim) wX wY)
geteff _ NilFL = ([], NilFL)
geteff ix (x :>: xs) | Just ix' <- mapM (commuteOrRemFromCtx (Normal x)) ix =
    case geteff ix' xs of
        (ns, xs') -> ( non (Normal x) : map (commuteOrAddToCtx (Normal x)) ns
                     , Normal x :>: xs')
geteff ix xx =
    case mergeConflictingNons ix of
        Nothing -> error $ renderString $
            redText "mergeConflictingNons failed in geteff: ix" $$
            displayNons ix $$ redText "xx" $$ displayPatch xx
        Just rix ->
            case mergeAfterConflicting rix xx of
                Just (a, x) ->
                    ( map (commuteOrAddToCtxRL (reverseFL a)) $ toNons x
                    , a +>+ x)
                Nothing ->
                    error $ renderString $
                        redText "mergeAfterConflicting failed in geteff" $$
                        redText "where ix" $$ displayNons ix $$
                        redText "and xx" $$ displayPatch xx $$
                        redText "and rix" $$ displayPatch rix

xx2nons :: PrimPatch prim => [Non (RepoPatchV2 prim) wX] -> FL prim wX wY
        -> [Non (RepoPatchV2 prim) wX]
xx2nons ix xx = fst $ geteff ix xx

xx2patches :: PrimPatch prim => [Non (RepoPatchV2 prim) wX] -> FL prim wX wY
           -> FL (RepoPatchV2 prim) wX wY
xx2patches ix xx = snd $ geteff ix xx

-- | If @xs@ consists only of 'Normal' patches, 'allNormal' @xs@ returns
--   @Just pxs@ those patches (so @lengthFL pxs == lengthFL xs@).
--   Otherwise, it returns 'Nothing'.
allNormal :: FL (RepoPatchV2 prim) wX wY -> Maybe (FL prim wX wY)
allNormal (Normal x :>: xs) = (x  :>: ) `fmap` allNormal xs
allNormal NilFL = Just NilFL
allNormal _ = Nothing

-- | This is used for unit-testing and for internal sanity checks
isConsistent :: PrimPatch prim => RepoPatchV2 prim wX wY -> Maybe Doc
isConsistent (Normal _) = Nothing
isConsistent (Duplicate _) = Nothing
isConsistent (Etacilpud _) = Nothing
isConsistent c@(InvConflictor{}) = isConsistent (invert c)
isConsistent (Conflictor im mm m@(Non deps _))
    | not $ everyoneConflicts im =
        Just $ redText "Someone doesn't conflict in im in isConsistent"
    | Just _ <- commuteOrRemFromCtxFL rmm m, _ :>: _ <- mm =
        Just $ redText "m doesn't conflict with mm in isConsistent"
    | any (\x -> any (x `conflictsWith`) nmm) im =
        Just $ redText "mm conflicts with im in isConsistent where nmm is" $$
               displayNons nmm
    | Nothing <- (nmm ++ im) `minus` toNons deps =
        Just $ redText "dependencies not in conflict:" $$
               displayNons (toNons deps) $$
               redText "compared with deps itself:" $$
               displayPatch deps
    | otherwise =
        case allConflictsWith m im of
            (im1, []) | im1 `eqSet` im -> Nothing
            (_, imnc) -> Just $ redText ("m doesn't conflict with im in "
                                         ++ "isConsistent. unconflicting:") $$
                                displayNons imnc
    where (nmm, rmm) = geteff im mm

everyoneConflicts :: PrimPatch prim => [Non (RepoPatchV2 prim) wX] -> Bool
everyoneConflicts [] = True
everyoneConflicts (x : xs) = case allConflictsWith x xs of
                                 ([], _) -> False
                                 (_, xs') -> everyoneConflicts xs'

instance PatchDebug prim => PatchDebug (RepoPatchV2 prim)

mergeWith :: PrimPatch prim => Non (RepoPatchV2 prim) wX
          -> [Non (RepoPatchV2 prim) wX] -> Sealed (FL prim wX)
mergeWith p [] = effect `mapSeal` unNon p
mergeWith p xs =
    mergeall . map unNon . (p :) . unconflicting_of $ nonDependsOrConflictsP xs
  where
    nonDependsOrConflictsP =
        filter (\x -> not ((p `dependsUpon` x) || (p `conflictsWith` x)))
    mergeall :: PrimPatch prim => [Sealed (FL (RepoPatchV2 prim) wX)]
             -> Sealed (FL prim wX)
    mergeall [Sealed x] = Sealed $ effect x
    mergeall [] = Sealed NilFL
    mergeall (Sealed x : Sealed y : rest) =
        case merge (x :\/: y) of
            y' :/\: _ -> mergeall (Sealed (x +>+ y') : rest)
    unconflicting_of [] = []
    unconflicting_of (q : qs) = case allConflictsWith q qs of
                                    ([], _) -> q : qs
                                    (_, nc) -> unconflicting_of nc

instance Summary (RepoPatchV2 prim) where
    conflictedEffect (Duplicate (Non _ x)) = [IsC Duplicated x]
    conflictedEffect (Etacilpud _) = error "impossible case"
    conflictedEffect (Conflictor _ _ (Non _ x)) = [IsC Conflicted x]
    conflictedEffect (InvConflictor{}) = error "impossible case"
    conflictedEffect (Normal x) = [IsC Okay x]

instance PrimPatch prim => Conflict (RepoPatchV2 prim) where
    numConflicts (Conflictor ix xx _) = length ix + lengthFL xx
    numConflicts (InvConflictor ix xx _) = length ix + lengthFL xx
    numConflicts _ = 0
    resolveConflicts _ = map mangleOrFail . combineConflicts resolveOne
      where
        resolveOne :: RepoPatchV2 prim wX wY -> [[Sealed (FL prim wY)]]
        resolveOne (Conflictor ix xx x) = [unravelled]
          where
            unravelled = nubFL $ filter isCons $ map (`mergeWith` xIxNonXX) xIxNonXX
            xIxNonXX = x : ix ++ nonxx
            nonxx = nonxx_ (reverseFL $ xx2patches ix xx)
        resolveOne _ = []
        -- |nonxx_ takes an RL of patches, and returns a singleton list
        -- containing a Non, in the case where we have a Normal patch at the
        -- end of the list (using the rest of the RL as context), and an empty
        -- list otherwise.
        nonxx_ :: RL (RepoPatchV2 prim) wX wY -> [Non (RepoPatchV2 prim) wX]
        nonxx_ (qs :<: Normal q) = [Non (reverseRL qs) q]
        nonxx_ _ = []
        isCons = unseal (not . nullFL)

instance PrimPatch prim => Unwind (RepoPatchV2 prim) where
  fullUnwind (Normal p) =
    mkUnwound NilFL (p :>: NilFL) NilFL
  fullUnwind (Duplicate (Non ps p)) =
    mkUnwound (effect ps) (p :>: NilFL) (invert p :>: effect (invert ps))
  fullUnwind (Conflictor _ es (Non ps p)) =
    mkUnwound (invert es +>+ effect ps) (p :>: NilFL) (invert p :>: effect (invert ps))
  fullUnwind (Etacilpud non) =
    invert (fullUnwind (Duplicate non))
  fullUnwind (InvConflictor ix xx x) =
    invert (fullUnwind (Conflictor ix xx x))

instance PrimPatch prim => CommuteNoConflicts (RepoPatchV2 prim) where
    commuteNoConflicts (d1@(Duplicate _) :> d2@(Duplicate _)) = Just (d2 :> d1)
    commuteNoConflicts (e@(Etacilpud _) :> d@(Duplicate _)) = Just (d :> e)
    commuteNoConflicts (d@(Duplicate _) :> e@(Etacilpud _)) = Just (e :> d)
    commuteNoConflicts (e1@(Etacilpud _) :> e2@(Etacilpud _)) = Just (e2 :> e1)

    -- If the duplicate is @x@, as a 'Non', with @invert x@ as the context,
    -- then it is the patch the duplicate @d@ represents, so commuting results
    -- in the same two patches (since we'd make one a duplicate, and the other
    -- would become @x@ as it would no longer be duplicated).
    -- Otherwise, we commute past, or remove @invert x@ from the context of @d@
    -- to obtain a new Duplicate.
    commuteNoConflicts orig@(x :> Duplicate d) =
        if d == commuteOrAddToCtx (invert x) (non x)
            then Just orig
            else do d' <- commuteOrRemFromCtx (invert x) d
                    return (Duplicate d' :> x)

    -- Commuting a Duplicate and any other patch simply places @invert x@ into
    -- the context of the non @d@, by commuting past, or adding to the context.
    commuteNoConflicts (Duplicate d :> x) =
        Just (x :> Duplicate (commuteOrAddToCtx (invert x) d))

    -- handle Etacilpud cases by first inverting, then using the previous
    -- definitions.
    commuteNoConflicts c@(Etacilpud _ :> _) = invertCommuteNC c
    commuteNoConflicts c@(_ :> Etacilpud _) = invertCommuteNC c

    -- Two normal patches should be simply commuted (assuming the can).
    commuteNoConflicts (Normal x :> Normal y) = do
        y' :> x' <- commute (x :> y)
        return (Normal y' :> Normal x')

    -- Commuting a Normal patch past a Conflictor first commutes @x@ past the
    -- effect of the Conflictor, then commutes the resulting @x'@ past the
    -- conflicting patch and the already-undone patches. The commuting must be
    -- done in this order to make the contexts match up (@iy@ and @y@ are made
    -- in the context before @yy@ have their effect, so we need to commute past
    -- the effect of @yy@ first).
    commuteNoConflicts (Normal x :> Conflictor iy yy y) = do
        iyy' :> x' <- commuteFL (x :> invert yy)
        y' : iy' <- mapM (Normal x' >*) (y : iy)
        return (Conflictor iy' (invert iyy') y' :> Normal x')

    -- Handle via the previous case, using the inverting commuter.
    commuteNoConflicts c@(InvConflictor{} :> Normal _) = invertCommuteNC c

    -- Commuting a Conflictor past a Normal patch is the dual operation to
    -- commuting a Normal patch past a Conflictor.
    commuteNoConflicts (Conflictor iy yy y :> Normal x) = do
        y' : iy' <- mapM (*> Normal x) (y : iy)
        x' :> iyy' <- commuteRL (invertFL yy :> x)
        return (Normal x' :> Conflictor iy' (invertRL iyy') y')

    -- Handle via the previous case, using the inverting commuter.
    commuteNoConflicts c@(Normal _ :> InvConflictor{}) = invertCommuteNC c

    -- Commuting two Conflictors, c1 and c2, first commutes the Conflictors'
    -- effects, then commutes the effect of c1 and c2 and the other's
    -- already-undone, and conflicting patch, to bring the already-undone and
    -- conflicting patch into the context of the commuted effects.
    commuteNoConflicts (Conflictor ix xx x :> Conflictor iy yy y) = do
        xx' :> yy' <- commute (yy :> xx)
        x':ix' <- mapM (yy >>*) (x:ix)
        y':iy' <- mapM (*>> xx') (y:iy)
        False <- return $ any (conflictsWith y) (x':ix')
        False <- return $ any (conflictsWith x') iy
        return (Conflictor iy' yy' y' :> Conflictor ix' xx' x')

    -- Handle via the previous case, using the inverting commuter.
    commuteNoConflicts c@(InvConflictor{} :> InvConflictor{}) =
        invertCommuteNC c

    commuteNoConflicts (InvConflictor ix xx x :> Conflictor iy yy y) = do
        iyy' :> xx' <- commute (xx :> invert yy)
        y':iy' <- mapM (xx' >>*) (y:iy)
        x':ix' <- mapM (invertFL iyy' >>*) (x:ix)
        False <- return $ any (conflictsWith y') (x':ix')
        False <- return $ any (conflictsWith x') iy'
        return (Conflictor iy' (invert iyy') y' :> InvConflictor ix' xx' x')

    commuteNoConflicts (Conflictor iy' yy' y' :> InvConflictor ix' xx' x') = do
        xx :> iyy <- commute (invert yy' :> xx')
        y:iy <- mapM (*>> xx') (y':iy')
        x:ix <- mapM (*>> yy') (x':ix')
        False <- return $ any (conflictsWith y') (x':ix')
        False <- return $ any (conflictsWith x') iy'
        return (InvConflictor ix xx x :> Conflictor iy (invert iyy) y)

instance PrimPatch prim => Check (RepoPatchV2 prim) where
    isInconsistent = isConsistent

type instance PatchId (RepoPatchV2 prim) = ()

instance FromPrim (RepoPatchV2 prim) where
    fromAnonymousPrim = Normal

instance ToPrim (RepoPatchV2 prim) where
    toPrim (Normal p) = Just p
    toPrim _ = Nothing

instance PrimPatch prim => Eq2 (RepoPatchV2 prim) where
    (Duplicate x) =\/= (Duplicate y) | x == y = IsEq
    (Etacilpud x) =\/= (Etacilpud y) | x == y = IsEq
    (Normal x) =\/= (Normal y) = x =\/= y
    (Conflictor cx xx x) =\/= (Conflictor cy yy y)
        | map commuteOrAddIXX cx `eqSet` map commuteOrAddIYY cy
          && commuteOrAddIXX x == commuteOrAddIYY y = reverseFL xx =/~\= reverseFL yy
      where
          commuteOrAddIXX = commutePrimsOrAddToCtx (invertFL xx)
          commuteOrAddIYY = commutePrimsOrAddToCtx (invertFL yy)
    (InvConflictor cx xx x) =\/= (InvConflictor cy yy y)
        | cx `eqSet` cy && x == y = xx =\~/= yy
    _ =\/= _ = NotEq

eqSet :: Eq a => [a] -> [a] -> Bool
eqSet [] [] = True
eqSet (x:xs) xys | Just ys <- remove1 x xys = eqSet xs ys
eqSet _ _ = False

remove1 :: Eq a => a -> [a] -> Maybe [a]
remove1 x (y : ys) = if x == y then Just ys else (y :) `fmap` remove1 x ys
remove1 _ [] = Nothing

minus :: Eq a => [a] -> [a] -> Maybe [a]
minus xs [] = Just xs
minus xs (y:ys) = do xs' <- remove1 y xs
                     xs' `minus` ys

invertNon :: PrimPatch prim => Non (RepoPatchV2 prim) wX
          -> Non (RepoPatchV2 prim) wX
invertNon (Non c x)
    | Just rc' <- removeRL nix (reverseFL c) = Non (reverseRL rc') (invert x)
    | otherwise = commuteOrAddToCtxRL (reverseFL c :<: Normal x) $ non nix
  where
    nix = Normal $ invert x

nonTouches :: PatchInspect prim => Non (RepoPatchV2 prim) wX -> [AnchoredPath]
nonTouches (Non c x) = listTouchedFiles (c +>+ Normal x :>: NilFL)

nonHunkMatches :: PatchInspect prim => (BC.ByteString -> Bool)
               -> Non (RepoPatchV2 prim) wX -> Bool
nonHunkMatches f (Non c x) = hunkMatches f c || hunkMatches f x

toNons :: forall p wX wY . (Commute p, PatchListFormat p,
       Nonable p, ShowPatchBasic (PrimOf p), ShowPatchBasic p)
       => FL p wX wY -> [Non p wX]
toNons xs = map lastNon $ initsFL xs
    where lastNon :: Sealed ((p :> FL p) wX) -> Non p wX
          lastNon (Sealed xxx) =
              case lastNon_aux xxx of
                   deps :> p :> _ ->
                       case non p of
                           Non NilFL pp -> Non (reverseRL deps) pp
                           Non ds pp ->
                               error $ renderString $
                                  redText "Weird case in toNons" $$
                                  redText "please report this bug!" $$
                                  (case xxx of
                                   z :> zs -> displayPatch (z :>: zs)) $$
                                  redText "ds are" $$ displayPatch ds $$
                                  redText "pp is" $$ displayPatch pp

          reverseFoo :: (p :> FL p) wX wZ -> (RL p :> p) wX wZ
          reverseFoo (p :> ps) = rf NilRL p ps
            where
              rf :: RL p wA wB -> p wB wC -> FL p wC wD
                 -> (RL p :> p) wA wD
              rf rs l NilFL = rs :> l
              rf rs x (y :>: ys) = rf (rs :<: x) y ys

          lastNon_aux :: (p :> FL p) wX wZ -> (RL p :> p :> RL p) wX wZ
          lastNon_aux = commuteWhatWeCanRL . reverseFoo

filterConflictsFL :: PrimPatch prim => Non (RepoPatchV2 prim) wX
                  -> FL prim wX wY -> (FL prim :> FL prim) wX wY
filterConflictsFL _ NilFL = NilFL :> NilFL
filterConflictsFL n (p :>: ps)
    | Just n' <- commuteOrRemFromCtx (Normal p) n =
        case filterConflictsFL n' ps of
            p1 :> p2 -> p :>: p1 :> p2
    | otherwise = case commuteWhatWeCanFL (p :> ps) of
                      p1 :> p' :> p2 ->
                          case filterConflictsFL n p1 of
                              p1a :> p1b -> p1a :> p1b +>+ p' :>: p2

instance Invert prim => Invert (RepoPatchV2 prim) where
    invert (Duplicate d) = Etacilpud d
    invert (Etacilpud d) = Duplicate d
    invert (Normal p) = Normal (invert p)
    invert (Conflictor x c p) = InvConflictor x c p
    invert (InvConflictor x c p) = Conflictor x c p

-- | Commute conflicting patches, i.e. one of them is the result of a
-- conflicted 'merge' with the other.
commuteConflicting :: PrimPatch prim
                   => CommuteFn (RepoPatchV2 prim) (RepoPatchV2 prim)
commuteConflicting (Normal x :> Conflictor a1'nop2 n1'x p1')
    | Just rn1' <- removeRL x (reverseFL n1'x) = do
        let p2 : n1nons = reverse $ xx2nons a1'nop2 $ reverseRL (rn1' :<: x)
            a2 = p1' : a1'nop2 ++ n1nons
        case (a1'nop2, reverseRL rn1', p1') of
            ([], NilFL, Non c y) | NilFL <- joinEffects c ->
                Just (Normal y :> Conflictor a1'nop2 (y :>: NilFL) p2)
            (a1, n1, _) ->
                Just (Conflictor a1 n1 p1' :> Conflictor a2 NilFL p2)
commuteConflicting c@(InvConflictor{} :> Normal _) = invertCommuteC c
commuteConflicting (Conflictor a1 n1 p1 :> Conflictor a2 n2 p2)
    | Just a2_minus_p1 <- remove1 p1' a2
    , not (p2 `dependsUpon` p1') = do
        let n1nons = map (commutePrimsOrAddToCtx n2) $ xx2nons a1 n1
            n2nons = xx2nons a2 n2
            Just a2_minus_p1n1 = a2_minus_p1 `minus` n1nons
            n2n1 = n2 +>+ n1
            a1' = map (commutePrimsOrAddToCtx n2) a1
            p2ooo = remNons a1' p2
        n1' :> n2' <- return $ filterConflictsFL p2ooo n2n1
        let n1'n2'nons = xx2nons a2_minus_p1n1 (n1' +>+ n2')
            n1'nons = take (lengthFL n1') n1'n2'nons
            n2'nons = drop (lengthFL n1') n1'n2'nons
            Just a1'nop2 = (a2 ++ n2nons) `minus` (p1' : n1'nons)
            Just a2'o =
                fst (allConflictsWith p2 $ a2_minus_p1 ++ n2nons)
                `minus` n2'nons
            Just a2' =
                mapM (commuteOrRemFromCtxFL (xx2patches a1'nop2 n1')) a2'o
            Just p2' = commuteOrRemFromCtxFL (xx2patches a1'nop2 n1') p2
        case (a2', n2', p2') of
            ([], NilFL, Non c x) ->
                case joinEffects c of
                    NilFL -> let n1'x = n1' +>+ x :>: NilFL in
                             Just (Normal x :> Conflictor a1'nop2 n1'x p1')
                    _ -> error "impossible case"
            _ -> Just (c1 :> c2)
              where
                c1 = Conflictor a2' n2' p2'
                c2 = Conflictor (p2 : a1'nop2) n1' p1'
    where (_, rpn2) = geteff a2 n2
          p1' = commuteOrAddToCtxRL (reverseFL rpn2) p1
commuteConflicting c@(InvConflictor{} :> InvConflictor{}) = invertCommuteC c
commuteConflicting _ = Nothing

instance PrimPatch prim => Commute (RepoPatchV2 prim) where
    commute pair@(x :> y) =
      commuteNoConflicts (assertConsistent x :> assertConsistent y)
      `mplus`
      commuteConflicting pair

instance PrimPatch prim => CleanMerge (RepoPatchV2 prim) where
    cleanMerge = mergeNoConflicts

instance PrimPatch prim => Merge (RepoPatchV2 prim) where
    merge (InvConflictor{} :\/: _) = error "impossible case"
    merge (_ :\/: InvConflictor{}) = error "impossible case"
    merge (Etacilpud _ :\/: _) = error "impossible case"
    merge (_ :\/: Etacilpud _) = error "impossible case"


    merge (Duplicate a :\/: Duplicate b) = Duplicate b :/\: Duplicate a
    -- We had a FIXME comment on this case, why?
    merge (Duplicate a :\/: b) =
        b :/\: Duplicate (commuteOrAddToCtx (invert b) a)

    -- Handle using the swap merge and the previous case.
    merge m@(_ :\/: Duplicate _) = swapMerge m

    merge (x :\/: y)
        -- First try the non-conflicting merge.
        | Just (y' :/\: x') <-
            mergeNoConflicts ((assertConsistent x) :\/: (assertConsistent y))
              = assertConsistent y' :/\: assertConsistent x'
        -- If we detect equal patches, we have a duplicate.
        | IsEq <- x =\/= y
        , n <- commuteOrAddToCtx (invert x) $ non x =
            Duplicate n :/\: Duplicate n

    -- We know that these two patches conflict, and aren't Duplicates, since we
    -- failed the previous case. We therefore create basic Conflictors, which
    -- undo the other patch.
    merge (nx@(Normal x) :\/: ny@(Normal y)) = cy :/\: cx
      where
        cy = Conflictor [] (x :>: NilFL) (non ny)
        cx = Conflictor [] (y :>: NilFL) (non nx)

    -- If a Normal patch @x@ and a Conflictor @cy@ conflict, we add @x@ to the
    -- effect of @cy@ on one side, and create a Conflictor that has no effect,
    -- but has the already-undone and conflicted patch of @cy@ and some foos as
    -- the already-undone on the other side.
    --
    -- TODO: what is foo?
    -- Why do we need nyy? I think @x'@ is @x@ in the context of @yy@.
    merge (Normal x :\/: Conflictor iy yy y) =
          Conflictor iy yyx y :/\: Conflictor (y : iy ++ nyy) NilFL x'
              where yyx = yy +>+ x :>: NilFL
                    (x' : nyy) = reverse $ xx2nons iy yyx

    -- Handle using the swap merge and the previous case.
    merge m@(Conflictor{} :\/: Normal _) = swapMerge m

    -- mH see also cH
    merge (Conflictor ix xx x :\/: Conflictor iy yy y) =
        case pullCommonRL (reverseFL xx) (reverseFL yy) of
            CommonRL rxx1 ryy1 c ->
                case commuteRLFL (ryy1 :> invertRL rxx1) of
                    Just (ixx' :> ryy') ->
                        let xx' = invert ixx'
                            yy' = reverseRL ryy'
                            y' : iy' =
                                map (commutePrimsOrAddToCtx xx') (y : iy)
                            x' : ix' =
                                map (commutePrimsOrAddToCtx ryy') (x : ix)
                            nyy' = xx2nons iy' yy'
                            nxx' = xx2nons ix' xx'
                            icx = drop (lengthRL rxx1) $
                                xx2nons ix (reverseRL $ rxx1 +<+ c)
                            ic' = map (commutePrimsOrAddToCtx ryy') icx
                            -- +++ is a more efficient version of nub (iy' ++
                            -- ix') given that we know each element shows up
                            -- only once in either list.
                            ixy' = ic' ++ (iy' +++ ix')
                            c1 = Conflictor (x' : ixy' ++ nxx') yy' y'
                            c2 = Conflictor (y' : ixy' ++ nyy') xx' x' in
                            c1 :/\: c2
                    Nothing -> error "impossible case"

instance PatchInspect prim => PatchInspect (RepoPatchV2 prim) where
    listTouchedFiles (Duplicate p) = nonTouches p
    listTouchedFiles (Etacilpud p) = nonTouches p
    listTouchedFiles (Normal p) = listTouchedFiles p
    listTouchedFiles (Conflictor x c p) =
        nubSort $ concatMap nonTouches x ++ listTouchedFiles c ++ nonTouches p
    listTouchedFiles (InvConflictor x c p) =
        nubSort $ concatMap nonTouches x ++ listTouchedFiles c ++ nonTouches p

    hunkMatches f (Duplicate p) = nonHunkMatches f p
    hunkMatches f (Etacilpud p) = nonHunkMatches f p
    hunkMatches f (Normal p) = hunkMatches f p
    hunkMatches f (Conflictor x c p) =
        any (nonHunkMatches f) x || hunkMatches f c || nonHunkMatches f p
    hunkMatches f (InvConflictor x c p) =
        any (nonHunkMatches f) x || hunkMatches f c || nonHunkMatches f p

-- | Split the rhs into those that /transitively/ conflict with the
-- lhs and those that don't.
allConflictsWith :: PrimPatch prim => Non (RepoPatchV2 prim) wX
                 -> [Non (RepoPatchV2 prim) wX]
                 -> ([Non (RepoPatchV2 prim) wX], [Non (RepoPatchV2 prim) wX])
allConflictsWith x ys = acw $ partition (conflictsWith x) ys
  where
    acw ([], nc) = ([], nc)
    acw (c:cs, nc) = case allConflictsWith c nc of
                         (c1, nc1) -> case acw (cs, nc1) of
                                          (xs', nc') -> (c : c1 ++ xs', nc')

conflictsWith :: PrimPatch prim => Non (RepoPatchV2 prim) wX
              -> Non (RepoPatchV2 prim) wX -> Bool
conflictsWith x y | x `dependsUpon` y || y `dependsUpon` x = False
conflictsWith x (Non cy y) =
    case commuteOrRemFromCtxFL cy x of
        Just (Non cx' x') ->
            let iy = Normal $ invert y in
            case commuteFL (iy :> cx' +>+ Normal x' :>: NilFL) of
                Just _ -> False
                Nothing -> True
        Nothing -> True

dependsUpon :: PrimPatch prim => Non (RepoPatchV2 prim) wX
            -> Non (RepoPatchV2 prim) wX -> Bool
dependsUpon (Non xs _) (Non ys y) =
    case removeSubsequenceFL (ys +>+ Normal y :>: NilFL) xs of
        Just _ -> True
        Nothing -> False

(+++) :: Eq a => [a] -> [a] -> [a]
[] +++ x = x
x +++ [] = x
(x:xs) +++ xys | Just ys <- remove1 x xys = x : (xs +++ ys)
               | otherwise = x : (xs +++ xys)

invertCommuteC :: PrimPatch prim => CommuteFn (RepoPatchV2 prim) (RepoPatchV2 prim)
invertCommuteC = invertCommuter commuteConflicting

invertCommuteNC :: PrimPatch prim => CommuteFn (RepoPatchV2 prim) (RepoPatchV2 prim)
invertCommuteNC = invertCommuter commuteNoConflicts

-- | 'pullCommon' @xs ys@ returns the set of patches that can be commuted out
-- of both @xs@ and @ys@ along with the remnants of both lists
pullCommon :: (Commute p, Eq2 p) => FL p wO wX -> FL p wO wY -> Common p wO wX wY
pullCommon NilFL ys = Fork NilFL NilFL ys
pullCommon xs NilFL = Fork NilFL xs NilFL
pullCommon (x :>: xs) xys | Just ys <- removeFL x xys =
    case pullCommon xs ys of
        Fork c xs' ys' -> Fork (x :>: c) xs' ys'
pullCommon (x :>: xs) ys =
    case commuteWhatWeCanFL (x :> xs) of
        xs1 :> x' :> xs2 -> case pullCommon xs1 ys of
            Fork c xs1' ys' -> Fork c (xs1' +>+ x' :>: xs2) ys'

-- | 'Common' @cs xs ys@ represents two sequences of patches that have @cs@ in
-- common, in other words @cs +>+ xs@ and @cs +>+ ys@
type Common p wO wX wY = Fork (FL p) (FL p) (FL p) wO wX wY

-- | 'pullCommonRL' @xs ys@ returns the set of patches that can be commuted
--   out of both @xs@ and @ys@ along with the remnants of both lists
pullCommonRL :: (Commute p, Eq2 p) => RL p wX wO -> RL p wY wO -> CommonRL p wX wY wO
pullCommonRL NilRL ys = CommonRL NilRL ys NilRL
pullCommonRL xs NilRL = CommonRL xs NilRL NilRL
pullCommonRL (xs :<: x) xys | Just ys <- removeRL x xys =
    case pullCommonRL xs ys of
        CommonRL xs' ys' c -> CommonRL xs' ys' (c :<: x)
pullCommonRL (xs :<: x) ys =
    case commuteWhatWeCanRL (xs :> x) of
        xs1 :> x' :> xs2 ->
            case pullCommonRL xs2 ys of
                CommonRL xs2' ys' c -> CommonRL (xs1 :<: x' +<+ xs2') ys' c

-- | 'CommonRL' @xs ys cs@' represents two sequences of patches that have @cs@
-- in common, in other words @xs +<+ cs@ and @ys +<+ cs@
data CommonRL p wX wY wF where
    CommonRL :: RL p wX wI -> RL p wY wI -> RL p wI wF -> CommonRL p wX wY wF

instance PrimPatch prim => Apply (RepoPatchV2 prim) where
    type ApplyState (RepoPatchV2 prim) = ApplyState prim
    apply p = applyPrimFL (effect p)

instance PrimPatch prim => RepairToFL (RepoPatchV2 prim) where
    applyAndTryToFixFL (Normal p) =
        mapMaybeSnd (mapFL_FL Normal) `liftM` applyAndTryToFixFL p
    applyAndTryToFixFL x = do apply x; return Nothing

instance PatchListFormat (RepoPatchV2 prim) where
   -- In principle we could use ListFormatDefault when prim /= V1 Prim patches,
   -- as those are the only case where we need to support a legacy on-disk
   -- format. In practice we don't expect RepoPatchV2 to be used with any other
   -- argument anyway, so it doesn't matter.
    patchListFormat = ListFormatV2

duplicate, etacilpud, conflictor, rotcilfnoc :: String
duplicate = "duplicate"
etacilpud = "etacilpud"
conflictor = "conflictor"
rotcilfnoc = "rotcilfnoc"

instance PrimPatch prim => ShowPatchBasic (RepoPatchV2 prim) where
    showPatch f (Duplicate d) = blueText duplicate $$ showNon f d
    showPatch f (Etacilpud d) = blueText etacilpud $$ showNon f d
    showPatch f (Normal p) = showPatch f p
    showPatch f (Conflictor i NilFL p) =
        blueText conflictor <+> showNons f i <+> blueText "[]" $$ showNon f p
    showPatch f (Conflictor i cs p) =
        blueText conflictor <+> showNons f i <+> blueText "[" $$
        showFL f cs $$
        blueText "]" $$
        showNon f p
    showPatch f (InvConflictor i NilFL p) =
        blueText rotcilfnoc <+> showNons f i <+> blueText "[]" $$ showNon f p
    showPatch f (InvConflictor i cs p) =
        blueText rotcilfnoc <+> showNons f i <+> blueText "[" $$
        showFL f cs $$
        blueText "]" $$
        showNon f p

instance PrimPatch prim => ShowContextPatch (RepoPatchV2 prim) where
    showPatchWithContextAndApply f (Normal p) = showPatchWithContextAndApply f p
    showPatchWithContextAndApply f p = apply p >> return (showPatch f p)

instance PrimPatch prim => ShowPatch (RepoPatchV2 prim) where
    summary = plainSummary
    summaryFL = plainSummary
    thing _ = "change"

instance PrimPatch prim => ReadPatch (RepoPatchV2 prim) where
    readPatch' = do
        skipSpace
        let str = string . BC.pack
            readConflictorPs = do
               i <- readNons
               ps <- bracketedFL readPatch' '[' ']'
               p <- readNon
               return (i, ps, p)
        choice [ do str duplicate
                    p <- readNon
                    return $ Sealed $ Duplicate p
               , do str etacilpud
                    p <- readNon
                    return $ Sealed $ Etacilpud p
               , do str conflictor
                    (i, Sealed ps, p) <- readConflictorPs
                    return $ Sealed $ Conflictor i (unsafeCoerceP ps) p
               , do str rotcilfnoc
                    (i, Sealed ps, p) <- readConflictorPs
                    return $ Sealed $ InvConflictor i ps p
               , do Sealed p <- readPatch'
                    return $ Sealed $ Normal p
               ]

instance Show2 prim => Show (RepoPatchV2 prim wX wY) where
    showsPrec d (Normal prim) =
        showParen (d > appPrec) $ showString "Normal " . showsPrec2 (appPrec + 1) prim

    showsPrec d (Duplicate x) =
        showParen (d > appPrec) $ showString "Duplicate " . showsPrec (appPrec + 1) x

    showsPrec d (Etacilpud x) =
        showParen (d > appPrec) $ showString "Etacilpud " . showsPrec (appPrec + 1) x

    showsPrec d (Conflictor ix xx x) =
        showParen (d > appPrec) $
            showString "Conflictor " . showsPrec (appPrec + 1) ix .
            showString " " . showsPrec (appPrec + 1) xx .
            showString " " . showsPrec (appPrec + 1) x

    showsPrec d (InvConflictor ix xx x) =
        showParen (d > appPrec) $
            showString "InvConflictor " . showsPrec (appPrec + 1) ix .
            showString " " . showsPrec (appPrec + 1) xx .
            showString " " . showsPrec (appPrec + 1) x

instance Show2 prim => Show1 (RepoPatchV2 prim wX)

instance Show2 prim => Show2 (RepoPatchV2 prim)

instance PrimPatch prim => Nonable (RepoPatchV2 prim) where
    non (Duplicate d) = d
    non (Etacilpud d) = invertNon d -- FIXME !!! ???
    non (Normal p) = Non NilFL p
    non (Conflictor _ xx x) = commutePrimsOrAddToCtx (invertFL xx) x
    non (InvConflictor _ _ n) = invertNon n

instance PrimPatch prim => Effect (RepoPatchV2 prim) where
    effect (Duplicate _) = NilFL
    effect (Etacilpud _) = NilFL
    effect (Normal p) = p :>: NilFL
    effect (Conflictor _ e _) = invert e
    effect (InvConflictor _ e _) = e

instance IsHunk prim => IsHunk (RepoPatchV2 prim) where
    isHunk rp = do Normal p <- return rp
                   isHunk p

displayNons :: (PatchListFormat p, ShowPatchBasic p, PrimPatchBase p) =>
               [Non p wX] -> Doc
displayNons p = showNons ForDisplay p

showFL :: ShowPatchBasic p => ShowPatchFor -> FL p wX wY -> Doc
showFL f = vcat . mapFL (showPatch f)
