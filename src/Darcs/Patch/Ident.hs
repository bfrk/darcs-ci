module Darcs.Patch.Ident
    ( Ident(..)
    , SignedIdent
    , PatchId
    , (=\^/=)
    , (=/^\=)
    , SignedId(..)
    , StorableId(..)
    , fastRemoveFL
    , fastRemoveRL
    , fastRemoveSubsequenceRL
    , findCommonFL
    , findCommonRL
    , findCommonWithThemFL
    , findCommonWithThemRL
    , commuteToPrefix
    -- * Properties
    , prop_identInvariantUnderCommute
    , prop_sameIdentityImpliesCommutable
    , prop_equalImpliesSameIdentity
    , prop_sameIdentityImpliesEqual
    ) where

import qualified Data.Set as S

import Darcs.Prelude

import Darcs.Patch.Commute ( Commute, commute, commuteFL, commuteRL )
import Darcs.Patch.Permutations ( partitionFL', partitionRL' )
import Darcs.Patch.Witnesses.Eq ( Eq2(..), EqCheck(..), isIsEq )
import Darcs.Patch.Witnesses.Ordered
    ( (:>)(..)
    , (:\/:)(..)
    , FL(..)
    , RL(..)
    , Fork(..)
    , (+<<+)
    , (+>>+)
    , mapFL
    , mapRL
    , reverseFL
    , reverseRL
    )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoercePEnd, unsafeCoercePStart )

import Darcs.Util.Format ( Format )
import Darcs.Util.Parser ( Parser )
import Darcs.Util.Printer ( Doc )

-- | The reason this is not associated to class 'Ident' is that for technical
-- reasons we want to be able to define type instances for patches that don't
-- have an identity and therefore cannot be lawful members of class 'Ident'.
type family PatchId (p :: Type -> Type -> Type)

{- | Class of patches that have an identity/name.

Patches with an identity give rise to the notion of /nominal equality/,
expressed by the operators '=\^/=' and '=/^\='.

Laws:

[/ident-commute/]

    Patch identity must be invariant under commutation:

    prop> 'commute' (p :> _) == 'Just' (_ :> p') => 'ident' p == 'ident' p'

    and thus (via symmetry of 'commute'):

    prop> 'commute' (_ :> q) == 'Just' (q' :> _) => 'ident' q == 'ident' q'

    Conversely, patches with the same identity result from a series of
    'commute's:

    prop> 'ident' p == 'ident' p' => exists qs, qs' :: FL p. 'commuteFL' (p :> qs) == 'Just' (qs' :> p')

[/ident-compare/]

    In general, comparing patches via their identity is
    weaker than (semantic) equality:

    prop> 'unsafeCompare' p q => 'ident' p == 'ident' q

    However, if the patches have a common context, then semantic and nominal
    equality should coincide, up to internal re-ordering:

    prop> p '=\~/=' q  <=> p '=\^/=' q
    prop> p '=/~\=' q  <=> p '=/^\=' q

    (Technical note: equality up to internal re-ordering is currently only
    defined for 'FL's, but it should be obvious how to generalize it.)

Taken together, these laws express the assumption that recording a patch
gives it a universally unique identity.

Note that violations of this universal property are currently not detected
in a reliable way. Fixing this is possible but far from easy.

-}
class Ord (PatchId p) => Ident p where
  ident :: p wX wY -> PatchId p

type instance PatchId (FL p) = S.Set (PatchId p)
type instance PatchId (RL p) = S.Set (PatchId p)
type instance PatchId (p :> p) = S.Set (PatchId p)

instance Ident p => Ident (FL p) where
  ident = S.fromList . mapFL ident

instance Ident p => Ident (RL p) where
  ident = S.fromList . mapRL ident

instance Ident p => Ident (p :> p) where
  ident (p :> q) = S.fromList [ident p, ident q]

-- | Nominal equality for patches with an identity in the same context. Usually
-- quite a bit faster than structural equality.
(=\^/=) :: Ident p => p wA wB -> p wA wC -> EqCheck wB wC
p =\^/= q = if ident p == ident q then unsafeCoercePEnd IsEq else NotEq

(=/^\=) :: Ident p => p wA wC -> p wB wC -> EqCheck wA wB
p =/^\= q = if ident p == ident q then unsafeCoercePStart IsEq else NotEq


{- | Signed identities.

Like for class 'Invert', we require that 'invertId' is self-inverse:

prop> 'invertId' . 'invertId' = 'id'

We also require that inverting changes the sign:

prop> 'positiveId' . 'invertId' = 'not' . 'positiveId'

Side remark: in mathematical terms, these properties can be expressed by
stating that 'invertId' is an involution and that 'positiveId' is a
"homomorphism of sets with an involution" (there is no official term for
this) from @a@ to the simplest non-trivial set with involution, namely
'Bool' with the involution 'not'.
-}
class Ord a => SignedId a where
  positiveId :: a -> Bool
  invertId :: a -> a

{- | Constraint for patches that have an identity that is signed,
     i.e. can be positive (uninverted) or negative (inverted).

Provided that an instance 'Invert' exists, inverting a patch
inverts its identity:

prop> 'ident' ('invert' p) = 'invertId' ('ident' p)

-}
type SignedIdent p = (Ident p, SignedId (PatchId p))


{- | Storable identities.

The methods here can be used to help implement ReadPatch and ShowPatch
for a patch type containing the identity.

As with all Read/Show pairs, We expect that the output of
@showId ForStorage x@ can be parsed by 'readId' to produce @x@:

prop> 'parse' 'readId' . 'renderPS' . 'showId' 'ForStorage' == 'id'

-}
class StorableId a where
  readId :: Parser a
  showId :: a -> Doc
  formatId :: a -> Format

{-# INLINABLE fastRemoveFL #-}
-- | Remove a patch from an FL of patches with an identity. The result is
-- 'Just' whenever the patch has been found and removed and 'Nothing'
-- otherwise. If the patch is not found at the head of the sequence we must
-- first commute it to the head before we can remove it.
-- 
-- We assume that this commute always succeeds. This is justified because
-- patches are created with a (universally) unique identity, implying that if
-- two patches have the same identity, then they have originally been the same
-- patch; thus being at a different position must be due to commutation,
-- meaning we can commute it back.
--
-- For patch types that define semantic equality via nominal equality, this is
-- only faster than 'removeFL' if the patch does not occur in the sequence,
-- otherwise we have to perform the same number of commutations.
fastRemoveFL :: forall p wX wY wZ. (Commute p, Ident p)
             => p wX wY
             -> FL p wX wZ
             -> Maybe (FL p wY wZ)
fastRemoveFL a bs
  | i `notElem` mapFL ident bs = Nothing
  | otherwise = do
      _ :> bs' <- pullout NilRL bs
      Just (unsafeCoercePStart bs')
  where
    i = ident a
    pullout :: RL p wA wB -> FL p wB wC -> Maybe ((p :> FL p) wA wC)
    pullout _ NilFL = Nothing
    pullout acc (x :>: xs)
      | ident x == i = do
          x' :> acc' <- commuteRL (acc :> x)
          Just (x' :> acc' +>>+ xs)
      | otherwise = pullout (acc :<: x) xs

-- | Same as 'fastRemoveFL' only for 'RL'.
fastRemoveRL :: forall p wX wY wZ. (Commute p, Ident p)
             => p wY wZ
             -> RL p wX wZ
             -> Maybe (RL p wX wY)
fastRemoveRL a bs
  | i `notElem` mapRL ident bs = Nothing
  | otherwise = do
      bs' :> _ <- pullout bs NilFL
      Just (unsafeCoercePEnd bs')
  where
    i = ident a
    pullout :: RL p wA wB -> FL p wB wC -> Maybe ((RL p :> p) wA wC)
    pullout NilRL _ = Nothing
    pullout (xs :<: x) acc
      | ident x == i = do
          acc' :> x' <- commuteFL (x :> acc)
          Just (xs +<<+ acc' :> x')
      | otherwise = pullout xs (x :>: acc)

fastRemoveSubsequenceRL :: (Commute p, Ident p)
                        => RL p wY wZ
                        -> RL p wX wZ
                        -> Maybe (RL p wX wY)
fastRemoveSubsequenceRL NilRL ys = Just ys
fastRemoveSubsequenceRL (xs :<: x) ys =
  fastRemoveRL x ys >>= fastRemoveSubsequenceRL xs

-- | Find the common and uncommon parts of two lists that start in a common
-- context, using patch identity for comparison. Of the common patches, only
-- one is retained, the other is discarded.
findCommonFL :: (Commute p, Ident p)
             => FL p wX wY
             -> FL p wX wZ
             -> Fork (FL p) (FL p) (FL p) wX wY wZ
findCommonFL xs ys =
  case findCommonWithThemFL xs ys of
    cxs :> xs' ->
      case findCommonWithThemFL ys xs of
        cys :> ys' ->
          case cxs =\^/= cys of
            NotEq -> error "common patches aren't equal"
            IsEq -> Fork cxs xs' ys'

findCommonWithThemFL
  :: (Commute p, Ident p) => FL p wX wY -> FL p wX wZ -> (FL p :> FL p) wX wY
findCommonWithThemFL xs ys =
  case partitionFL' ((`S.member` yids) . ident) NilRL NilRL xs of
    cxs :> NilRL :> xs' -> cxs :> reverseRL xs'
    _ -> error "failed to commute common patches"
  where
    yids = S.fromList (mapFL ident ys)

findCommonRL :: (Commute p, Ident p)
             => RL p wX wY
             -> RL p wX wZ
             -> Fork (RL p) (RL p) (RL p) wX wY wZ
findCommonRL xs ys =
  case findCommonWithThemRL xs ys of
    cxs :> xs' ->
      case findCommonWithThemRL ys xs of
        cys :> ys' ->
          case cxs =\^/= cys of
            NotEq -> error "common patches aren't equal"
            IsEq -> Fork cxs xs' ys'

findCommonWithThemRL
  :: (Commute p, Ident p) => RL p wX wY -> RL p wX wZ -> (RL p :> RL p) wX wY
findCommonWithThemRL xs ys =
  case partitionRL' (not . (`S.member` yids) . ident) xs of
    cxs :> NilFL :> xs' -> reverseFL cxs :> xs'
    _ -> error "failed to commute common patches"
  where
    yids = S.fromList (mapRL ident ys)

-- | Try to commute all patches matching any of the 'PatchId's in the set to the
-- head of an 'FL', i.e. backwards in history.
commuteToPrefix :: (Commute p, Ident p)
                => S.Set (PatchId p) -> FL p wX wY -> Maybe ((FL p :> RL p) wX wY)
commuteToPrefix is ps
  | prefix :> NilRL :> rest <-
      partitionFL' ((`S.member` is) . ident) NilRL NilRL ps = Just (prefix :> rest)
  | otherwise = Nothing

prop_identInvariantUnderCommute :: (Commute p, Ident p)
                                => (p :> p) wX wY -> Maybe Bool
prop_identInvariantUnderCommute (p :> q) =
  case commute (p :> q) of
    Just (q' :> p') -> Just $ ident p == ident p' && ident q == ident q'
    Nothing -> Nothing

prop_sameIdentityImpliesCommutable :: (Commute p, Eq2 p, Ident p)
                                   => (p :\/: (RL p :> p)) wX wY -> Maybe Bool
prop_sameIdentityImpliesCommutable (p :\/: (ps :> q))
  | ident p == ident q =
      case commuteRL (ps :> q) of
        Just (p' :> _) -> Just $ isIsEq (p =\/= p')
        Nothing -> Just False
  | otherwise = Nothing

prop_equalImpliesSameIdentity :: (Eq2 p, Ident p)
                              => p wA wB -> p wC wD -> Maybe Bool
prop_equalImpliesSameIdentity p q
  | p `unsafeCompare` q = Just $ ident p == ident q
  | otherwise = Nothing

-- Note the assumption of coinciding start states here!
prop_sameIdentityImpliesEqual :: (Eq2 p, Ident p)
                              => (p :\/: p) wX wY -> Maybe Bool
prop_sameIdentityImpliesEqual (p :\/: q)
  | ident p == ident q = Just $ isIsEq $ p =\/= q
  | otherwise = Nothing
