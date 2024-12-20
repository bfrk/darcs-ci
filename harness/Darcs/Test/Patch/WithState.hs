{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module Darcs.Test.Patch.WithState where

import Darcs.Prelude

import Darcs.Patch.Apply
import Darcs.Patch.Commute
import Darcs.Patch.Effect
import Darcs.Patch.FromPrim
import Darcs.Patch.Invert
import Darcs.Patch.Prim.Class
import Darcs.Patch.Witnesses.Eq
import Darcs.Patch.Witnesses.Maybe
import Darcs.Patch.Witnesses.Ordered
import Darcs.Patch.Witnesses.Sealed
import Darcs.Patch.Witnesses.Show
import Test.QuickCheck ( Gen, sized, choose )

import Darcs.Test.Patch.RepoModel
import Darcs.Test.Patch.Arbitrary.Sealed
import Darcs.Test.Patch.Arbitrary.Shrink
import Darcs.Test.Patch.Types.Pair ( Pair(..) )

import Data.Maybe

----------------------------------------------------------------------
-- * WithState

data WithState p wX wY = WithState {
                              wsStartState :: (ModelOf p) wX
                            , wsPatch      :: p wX wY
                            , wsEndState   :: (ModelOf p) wY
                            }

type instance ModelOf (WithState p) = ModelOf p

instance (Show1 (ModelOf p), Show2 p) => Show (WithState p wX wY) where
  showsPrec d (WithState s p s')
    = showParen (d > appPrec) $ showString "WithState "
                              . showsPrec1 (appPrec+1) s
                              . showString " "
                              . showsPrec2 (appPrec+1) p
                              . showString " "
                              . showsPrec1 (appPrec+1) s'

instance (Show1 (ModelOf p), Show2 p) => Show1 (WithState p wA)

instance (Show1 (ModelOf p), Show2 p) => Show2 (WithState p)

class ArbitraryWS p where
  arbitraryWS :: Gen (Sealed2 (WithState p))
  shrinkWS :: Sealed2 (WithState p) -> [Sealed2 (WithState p)]
  shrinkWS _ = []

instance ArbitraryWS p => ArbitraryS2 (WithState p) where
  arbitraryS2 = arbitraryWS
  shrinkS2 = shrinkWS

instance (ArbitraryState p1, ArbitraryState p2, ModelOf p1 ~ ModelOf p2, RepoModel (ModelOf p1)) => ArbitraryS2 (p1 :\/: p2) where
  arbitraryS2 = do
    repo <- aSmallRepo
    Sealed (WithEndState p1 _) <- arbitraryState repo
    Sealed (WithEndState p2 _) <- arbitraryState repo
    return (Sealed2 (p1 :\/: p2))

arbitraryWSPair :: (RepoModel (ModelOf p), ArbitraryState p) => Gen (Sealed2 (WithState (Pair p)))
arbitraryWSPair = do
  repo <- aSmallRepo
  Sealed (WithEndState pp repo') <- arbitraryStatePair repo
  return $ seal2 $ WithState repo pp repo'

instance (RepoModel (ModelOf p), ArbitraryState p) => ArbitraryWS (Pair p) where
  arbitraryWS = arbitraryWSPair
  shrinkWS _ = []

instance (RepoModel (ModelOf p), ArbitraryState p) => ArbitraryWS (FL p) where
  arbitraryWS = makeWS2Gen aSmallRepo

-- | This is only used for the legacy 'Tree' based test generator, where the
-- @p@ parameter gets instantiated to @'Tree' p@ (which has no definite end
-- state).
data WithStartState s p wX = WithStartState {
                                 wssStartState :: s wX
                               , wssPatch      :: p wX
                               }
    deriving Eq

instance (Show1 s, Show1 p) => Show (WithStartState s p wX) where
   showsPrec d (WithStartState s p) = showParen (d > appPrec) $ showString "WithStartState " .
                                      showsPrec1 (appPrec + 1) s . showString " " .
                                      showsPrec1 (appPrec + 1) p

instance (Show1 s, Show1 p) => Show1 (WithStartState s p)

-- |'WithStartState2' is like 'WithStartState' but for patches that have both witnesses.
data WithStartState2 p wX wY =
  WithStartState2
  { wss2StartState :: ModelOf p wX
  , wss2Patch      :: p wX wY
  }

instance (Show1 (ModelOf p), Show2 p) => Show (WithStartState2 p wX wY) where
  showsPrec d (WithStartState2 s p) =
    showParen (d > appPrec) $ showString "WithStartState2 " .
    showsPrec1 (appPrec + 1) s . showString " " .
    showsPrec2 (appPrec + 1) p

instance (Show1 (ModelOf p), Show2 p) => Show1 (WithStartState2 p wX)
instance (Show1 (ModelOf p), Show2 p) => Show2 (WithStartState2 p)

-- | A combination of a patch and its final state. The state, in this module, is
--   typically represented by a 'RepoModel' value. The @px@ type is typically a
--   patch type applied to its pre-state, e.g. @Prim x@.
data WithEndState s px wY = WithEndState {
                                wesPatch    :: px wY
                              , wesEndState :: s wY
                              }
    deriving Eq

instance (Show1 s, Show1 p) => Show (WithEndState s p wX) where
   showsPrec d (WithEndState p s) = showParen (d > appPrec) $ showString "WithEndState " .
                                    showsPrec1 (appPrec + 1) p . showString " " .
                                    showsPrec1 (appPrec + 1) s


instance (Show1 s, Show1 p) => Show1 (WithEndState s p)


----------------------------------------------------------------------
-- * ArbitraryState generators

-- | A type class to generate arbitrary values, threading a state through the
--   arbitrary calls. So this can be used to generate a patch that comes after
--   another patch. The post-state of the generated patch is hidden by the
--   'Sealed'.
class ArbitraryState p where
  arbitraryState :: ModelOf p wX -> Gen (Sealed (WithEndState (ModelOf p) (p wX)))

  -- |This member is to allow specialising generation of pairs,
  -- e.g. to increase the frequency of commutable ones.
  arbitraryStatePair :: ModelOf p wX -> Gen (Sealed (WithEndState (ModelOf p) (Pair p wX)))
  -- default implementation doesn't do anything special
  arbitraryStatePair s = do
    -- use the :> instance
    Sealed (WithEndState pair s') <- arbitraryState s
    return $ seal $ WithEndState (Pair pair) s'

instance ArbitraryState p => ArbitraryState (WithState p) where
  arbitraryState s = do Sealed (WithEndState x s') <- arbitraryState s
                        return $ seal $ WithEndState (WithState s x s') s'

-- this instance is only useful if ModelOf p ~ ModelOf q
type instance ModelOf (p :> q) = ModelOf p

instance (ArbitraryState p, ArbitraryState q, ModelOf p ~ ModelOf q) => ArbitraryState (p :> q) where
  arbitraryState s = do
    Sealed (WithEndState p1 s') <- arbitraryState s
    Sealed (WithEndState p2 s'') <- arbitraryState s'
    return $ seal $ WithEndState (p1 :> p2) s'' 

arbitraryFL ::
     ArbitraryState p
  => forall wX. Int -> ModelOf p wX -> Gen (Sealed (WithEndState (ModelOf p) (FL p wX)))
arbitraryFL 0 s = return $ seal $ WithEndState NilFL s
arbitraryFL n s = do Sealed (WithEndState x s') <- arbitraryState s
                     Sealed (WithEndState xs s'') <- arbitraryFL (n-1) s'
                     return $ seal $ WithEndState (x :>: xs) s''

instance ArbitraryState p => ArbitraryState (FL p) where
  arbitraryState s = sized $ \n -> do k <- choose (0, min 2 (n `div` 5))
                                      arbitraryFL k s


makeSGen :: ArbitraryState p => Gen (ModelOf p wX) -> Gen (Sealed (p wX))
makeSGen stGen = do s <- stGen
                    Sealed (WithEndState p _) <- arbitraryState s
                    return $ seal p

makeWS2Gen :: ArbitraryState p => Gen (ModelOf p wX) -> Gen (Sealed2 (WithState p))
makeWS2Gen stGen = do s <- stGen
                      Sealed (WithEndState wsP _) <- arbitraryState s
                      return $ seal2 wsP

makeWSGen :: ArbitraryState p => Gen (ModelOf p wX) -> Gen (Sealed (WithState p wX))
makeWSGen stGen = do s <- stGen
                     Sealed (WithEndState wsP _) <- arbitraryState s
                     return $ seal wsP

-- | A class to help with shrinking complex test cases by simplifying
-- the starting state of the test case. See also 'PropagateShrink'.
class ShrinkModel s prim where
  -- |Given a repository state, produce a patch that simplifies the
  -- repository state. The inverse of the patch can be passed as the
  -- "shrinking fixup" to 'propagateShrink'.
  --
  -- Imagine that we start with
  --
  --    s wX1 --p1 wX1 wY1--> s wY1
  --
  -- If we shrink the state to @s wX2@:
  --
  --    s wX2 <--prim wX1 wX2-- s wX1
  --
  -- then we hope that 'propagateShrink' will produce a simpler version of @p1@,
  -- @p2@, that starts from the simpler state @s wX2@:
  --
  --                        p2 wX2 wY2
  --               s wX2 ----------------> s wY2
  --                |                        |
  --                |                        |
  --    invert prim |                        | (discard)
  --                |                        |
  --                V                        V
  --               s wX1 ----------------> s wY1
  --                        p1 wX1 wY1
  shrinkModelPatch :: s wX -> [Sealed (prim wX)]

checkOK :: Fail a -> [a]
checkOK = maybe [] (\x -> [x]) . maybeFail

shrinkModel
  :: forall s prim wX
   . ( ApplyState prim ~ RepoState s
     , RepoModel s
     , ShrinkModel s prim
     , RepoApply prim
     )
  => s wX
  -> [Sealed (WithEndState s (prim wX))]
shrinkModel s = do
  Sealed prim <- shrinkModelPatch s
  endState <- checkOK $ repoApply s prim
  return $ Sealed $ WithEndState prim endState

-- | A class to help with shrinking complex test cases. The idea is that the
-- "starting state" of the test case is shrunk and this results in a "fixup"
-- primitive that goes from the shrunk starting state to the original starting
-- state. This so-called "shrinking fixup" is then propagated through the test
-- case to generate a new test case that starts at the shrunk starting state.
-- The shrinking fixup is typically generated via the 'ShrinkModel' class.
class PropagateShrink prim p where
  -- Given a test patch (of type @p@) and a shrinking fixup (of type @prim@),
  -- try to propagate the shrinking fixup past the test patch.
  -- The @Maybe2 p@ return type allows the fixup to eliminate the shrinking
  -- patch entirely, and vice versa the @FL prim@ allows the shrinking fixup
  -- to disappear (for example it might be cancelled out by something in the test
  -- patch).
  -- In the result type we use @FL prim@ for the propagated shrinking fixups
  -- (instead of the more restrictive @Maybe2 prim@) because for
  -- MergeableSequence this makes propagateShrink succeed in more cases.
  propagateShrink :: (prim :> p) wX wY -> Maybe ((Maybe2 p :> FL prim) wX wY)

propagateShrinkKeep
  :: PropagateShrink prim p
  => (prim :> p) wX wY
  -> Maybe ((p :> FL prim) wX wY)
propagateShrinkKeep inp = do
  Just2 p' :> prims' <- propagateShrink inp
  return (p' :> prims')

propagateShrinks
  :: PropagateShrink prim p
  => (FL prim :> p) wX wY
  -> Maybe ((Maybe2 p :> FL prim) wX wY)
propagateShrinks (NilFL :> p) = Just (Just2 p :> NilFL)
propagateShrinks (prim :>: prims :> p) = do
  mp' :> prims' <- propagateShrinks (prims :> p)
  case mp' of
    Nothing2 -> return (Nothing2 :> prim :>: prims')
    Just2 p' -> do
      mp'' :> prims'' <- propagateShrink (prim :> p')
      return (mp'' :> prims'' +>+ prims')

-- |Shrink a test case wrapped with 'WithStartState2' by shrinking the start state
-- of the test case with 'ShrinkModel' and then propagating the shrink through the
-- patch type of the test case.
shrinkState
  :: forall s prim p
   . ( Invert prim, RepoModel s
     , ShrinkModel s prim, PropagateShrink prim p
     , ApplyState prim ~ RepoState s
     , ModelOf p ~ s
     , RepoApply prim
     )
  => Sealed2 (WithStartState2 p)
  -> [Sealed2 (WithStartState2 p)]
shrinkState (Sealed2 (WithStartState2 s p)) = do
  Sealed (WithEndState fixup shrunkState) <- shrinkModel @s @prim s
  p' :> _ <- maybeToList $ propagateShrinkKeep (invert fixup :> p)
  return $ Sealed2 $ WithStartState2 shrunkState p'

shrinkAtStartState
  :: ( Shrinkable p, RepoModel (ModelOf p), Effect p
     , prim ~ PrimOf p, Invert prim
     , ApplyState prim ~ RepoState (ModelOf p)
     , RepoApply prim
     )
  => WithStartState2 p wX wY
  -> [FlippedSeal (WithStartState2 p) wY]
shrinkAtStartState (WithStartState2 s p) = do
  FlippedSeal p' <- shrinkAtStart p
  endState <- checkOK $ repoApply s (effect p)
  newState <- checkOK $ repoApply endState (invert (effect p'))
  return $ FlippedSeal (WithStartState2 newState p')

instance
  ( ArbitraryState p, Shrinkable p, RepoModel s
  , s ~ ModelOf p
  , Effect p
  , ApplyState prim ~ RepoState s
  , prim ~ PrimOf p, Invert prim, ShrinkModel s prim, PropagateShrink prim p
  , RepoApply prim
  )
  => ArbitraryS2 (WithStartState2 p) where
  arbitraryS2 = do
    repo <- aSmallRepo @s
    Sealed (WithEndState p _) <- arbitraryState repo
    return (Sealed2 (WithStartState2 repo p))
  shrinkS2 w@(Sealed2 (WithStartState2 repo p)) =
    map (Sealed2 . WithStartState2 repo) (shrinkInternally p) ++
    map (unseal (Sealed2 . WithStartState2 repo)) (shrinkAtEnd p) ++
    map (unsealFlipped Sealed2) (shrinkAtStartState (WithStartState2 repo p)) ++
    shrinkState @s @prim @p w

propagatePrim
  :: PrimCoalesce prim
  => (prim :> prim) wX wY -> Maybe ((Maybe2 prim :> FL prim) wX wY)
propagatePrim (p1 :> p2)
  -- The order of guards here means we prefer commutation over coalescing. In
  -- most cases coalescing prims don't commute and, vice versa, commuting prims
  -- don't coalesce, so it makes no difference. I think for Prim.V1 the only
  -- exception is adjacent hunks that both add and remove lines. In this case,
  -- commutation simplifies more, so the choice here is justified.
  | IsEq <- invert p1 =\/= p2 = Just (Nothing2 :> NilFL)
  | Just (p2' :> p1') <- commute (p1 :> p2) = Just (Just2 p2' :> p1' :>: NilFL)
  | Just p' <- primCoalesce p1 p2 = Just (Just2 p' :> NilFL)
  | otherwise = Nothing

instance (PropagateShrink prim p, PropagateShrink prim q)
  => PropagateShrink prim (p :> q) where

  propagateShrink (prim :> (p :> q)) = do
    Just2 mp' :> prims' <- propagateShrink (prim :> p)
    Just2 mq' :> prims'' <- propagateShrinks (prims' :> q)
    return (Just2 (mp' :> mq') :> prims'')

instance PropagateShrink prim p => PropagateShrink prim (FL p) where
  propagateShrink (prim :> NilFL) = return (Just2 NilFL :> prim :>: NilFL)
  propagateShrink (prim :> (p :>: ps)) = do
    mp' :> prims' <- propagateShrink (prim :> p)
    mps' :> prims'' <- propagateShrinks (prims' :> ps)
    let result = case (mp', mps') of
          (Nothing2, Nothing2) -> NilFL
          (Nothing2, Just2 ps') -> ps'
          (Just2 p', Nothing2) -> p' :>: NilFL
          (Just2 p', Just2 ps') -> p' :>: ps'
    return (Just2 result :> prims'')
