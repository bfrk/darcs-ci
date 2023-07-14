{- | 'Conflictor's a la camp.

Similar to the camp paper, but with a few differences:

* no reverse conflictors and no Invert instance

* instead we directly implement cleanMerge

* minor details of merge and commute due to bug fixes

The proofs in this module assume that whenever we create a conflictor we
maintain the following invariants:

(1) A conflictor reverts a patch in its context iff it is the first patch
    that conflicts with it. This implies that any patch a conflictor reverts
    exists in its context as an unconflicted Prim.

(2) If v depends on u and p conflicts with u then it also conflicts with v.

-}

{-# LANGUAGE ViewPatterns, PatternSynonyms #-}
module Darcs.Patch.V3.Core
    ( RepoPatchV3(..)
    , pattern PrimP
    , pattern ConflictorP
    , (+|)
    , (-|)
    ) where

import Control.Applicative ( Alternative(..) )
import Control.Monad ( guard )
import qualified Data.ByteString.Char8 as BC
import Data.List.Ordered ( nubSort )
import qualified Data.Set as S

import Darcs.Prelude

import Darcs.Patch.Commute ( commuteFL, commuteRL, commuteRLFL )
import Darcs.Patch.CommuteFn ( CommuteFn )
import Darcs.Patch.CommuteNoConflicts ( CommuteNoConflicts(..) )
import Darcs.Patch.Debug ( PatchDebug(..) )
import Darcs.Patch.FileHunk ( IsHunk(..) )
import Darcs.Patch.Format ( ListFormat(ListFormatV3) )
import Darcs.Patch.FromPrim ( ToPrim(..) )
import Darcs.Patch.Ident
    ( Ident(..)
    , PatchId
    , SignedId(..)
    , StorableId(..)
    , commuteToPrefix
    , fastRemoveFL
    , findCommonFL
    , (=\^/=)
    )
import Darcs.Patch.Invert ( Invert, invert )
import Darcs.Patch.Merge
    ( CleanMerge(..)
    , Merge(..)
    , cleanMergeFL
    , swapCleanMerge
    , swapMerge
    )
import Darcs.Patch.Prim ( PrimPatch, applyPrimFL, sortCoalesceFL )
import Darcs.Patch.Prim.WithName ( PrimWithName, wnPatch )
import Darcs.Patch.Read ( bracketedFL )
import Darcs.Patch.Repair (RepairToFL(..), Check(..) )
import Darcs.Patch.RepoPatch
    ( Apply(..)
    , Commute(..)
    , Effect(..)
    , Eq2(..)
    , PatchInspect(..)
    , PatchListFormat(..)
    , PrimPatchBase(..)
    , ReadPatch(..)
    , Summary(..)
    )
import Darcs.Patch.Show hiding ( displayPatch )
import Darcs.Patch.Summary
    ( ConflictState(..)
    , IsConflictedPrim(..)
    , plainSummary
    , plainSummaryFL
    )
import Darcs.Patch.Unwind ( Unwind(..), mkUnwound )
import Darcs.Patch.V3.Contexted
    ( Contexted
    , ctxId
    , ctxView
    , ctxNoConflict
    , ctx
    , ctxAddRL
    , ctxAddInvFL
    , ctxAddFL
    , commutePast
    , ctxToFL
    , ctxTouches
    , ctxHunkMatches
    , showCtx
    , readCtx
    )
import Darcs.Patch.Witnesses.Eq ( EqCheck(..) )
import Darcs.Patch.Witnesses.Ordered
    ( (:/\:)(..)
    , (:>)(..)
    , (:\/:)(..)
    , FL(..)
    , Fork(..)
    , (+>+)
    , mapFL
    , mapFL_FL
    , reverseFL
    , reverseRL
    )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..), mapSeal, unseal )
import Darcs.Patch.Witnesses.Show ( Show1, Show2, appPrec, showsPrec2 )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP1 )

import Darcs.Test.TestOnly

import Darcs.Util.Parser ( string, lexString, choice, skipSpace )
import Darcs.Util.Printer
    ( Doc
    , ($$)
    , (<+>)
    , blueText
    , redText
    , renderString
    , vcat
    )

data RepoPatchV3 name prim wX wY where
  Prim :: PrimWithName name prim wX wY -> RepoPatchV3 name prim wX wY
  Conflictor :: FL (PrimWithName name prim) wX wY             -- ^ effect
             -> S.Set (Contexted (PrimWithName name prim) wY) -- ^ conflicts
             -> Contexted (PrimWithName name prim) wY         -- ^ identity
             -> RepoPatchV3 name prim wX wY

{- Naming convention: If we don't examine the contents of a RepoPatchV3, we
use @p@ (on the lhs) and @q@ (on the rhs), otherwise these names refer to
the (uncontexted) prims they represent (regardless of whether they are
conflicted or not). The components of Conflictors are named as follows: On
the lhs we use @Conflictor r x cp@, on the rhs @Conflictor s y cq@, execpt
when we have two conflictors that may have common prims in their effects. In
that case we use @com_r@ and @com_s@ for the effects and use @r@ and @s@ for
the uncommon parts (and @com@ for the common part). Primed versions always
refer to things with the same ident/name i.e. they are commuted versions of
the un-primed ones. -}

-- TODO now that we export the constructors of RepoPatchV3 these
-- pattern synonyms could probably be removed
pattern PrimP :: TestOnly => PrimWithName name prim wX wY -> RepoPatchV3 name prim wX wY
pattern PrimP prim <- Prim prim

pattern ConflictorP
  :: TestOnly
  => FL (PrimWithName name prim) wX wY
  -> S.Set (Contexted (PrimWithName name prim) wY)
  -> Contexted (PrimWithName name prim) wY
  -> RepoPatchV3 name prim wX wY
pattern ConflictorP r x cp <- Conflictor r x cp

-- * Effect

instance Effect (RepoPatchV3 name prim) where
  effect (Prim p) = wnPatch p :>: NilFL
  effect (Conflictor r _ _) = mapFL_FL wnPatch r

-- * Ident

type instance PatchId (RepoPatchV3 name prim) = name

instance SignedId name => Ident (RepoPatchV3 name prim) where
  ident (Prim p) = ident p
  ident (Conflictor _ _ cp) = ctxId cp

-- * Merge

-- We only use displayPatch for error messages here, so it makes sense
-- to use the storage format that contains the patch names.
displayPatch :: ShowPatchBasic p => p wX wY -> Doc
displayPatch p = showPatch ForStorage p

instance (SignedId name, StorableId name, PrimPatch prim) =>
         CleanMerge (RepoPatchV3 name prim) where
  cleanMerge (p :\/: q)
    | ident p == ident q = error "merging identical patches is undefined"
  cleanMerge (Prim p :\/: Prim q) = do
    q' :/\: p' <- cleanMerge (p :\/: q)
    return $ Prim q' :/\: Prim p'
  cleanMerge (Prim p :\/: Conflictor s y cq) = do
    -- note: p cannot occur in y, because every element of y already
    -- exists in the history /before/ the rhs, and PatchIds must be
    -- unique in a repo
    s' :/\: p' <- cleanMergeFL (p :\/: s)
    let ip' = invert p'
    cq' <- commutePast ip' cq
    y' <- S.fromList <$> mapM (commutePast ip') (S.toList y)
    return $ Conflictor s' y' cq' :/\: Prim p'
  cleanMerge pair@(Conflictor {} :\/: Prim {}) = swapCleanMerge pair
  cleanMerge (Conflictor com_r x cp :\/: Conflictor com_s y cq) =
    case findCommonFL com_r com_s of
      Fork _ rev_r rev_s -> do
        s' :/\: r' <- cleanMerge (rev_r :\/: rev_s)
        -- the paper uses commutePast to calculate cp' and cq', but this must
        -- succeed (and then give the same result as adding to the context)
        -- because of the ctxNoConflict guards below
        let cp' = ctxAddInvFL s' cp
        let cq' = ctxAddInvFL r' cq
        let x' = S.map (ctxAddInvFL s') x
        let y' = S.map (ctxAddInvFL r') y
        guard (ctxNoConflict cq' cp')
        guard $ all (ctxNoConflict cq') (S.difference x' y')
        guard $ all (ctxNoConflict cp') (S.difference y' x')
        return $ Conflictor s' y' cq' :/\: Conflictor r' x' cp'

instance (SignedId name, StorableId name, PrimPatch prim) =>
         Merge (RepoPatchV3 name prim) where
  -- * no conflict
  merge pq | Just r <- cleanMerge pq = r
  -- * conflicting prim patches:
  -- If we have p and pull conflicting q, we make a conflictor
  -- that inverts p, conflicts with p, and represents q.
  merge (Prim p :\/: Prim q) =
    Conflictor (invert p :>: NilFL) (S.singleton (ctx p)) (ctx q)
    :/\:
    Conflictor (invert q :>: NilFL) (S.singleton (ctx q)) (ctx p)
  -- * prim patch p conflicting with conflictor on the rhs:
  -- The rhs is the first to conflict with p, so we must add invert p
  -- to its effect, and to its conflicts (adding invert r as context for p).
  -- For the other branch, we add a new conflictor representing p. It
  -- conflicts with q and has no effect, since q is already conflicted.
  merge (Prim p :\/: Conflictor r x cq) =
    Conflictor (invert p :>: r) (ctxAddInvFL r (ctx p) +| x) cq
    :/\:
    Conflictor NilFL (S.singleton cq) (ctxAddInvFL r (ctx p))
  -- same as previous case with both sides swapped
  merge pair@(Conflictor {} :\/: Prim {}) = swapMerge pair
  -- * conflictor c1 conflicts with conflictor c2:
  -- If we pull c2 onto c1, we remove everything common to both effects
  -- from the effect of c2 (but still remember that we conflict with them).
  -- We also record that we now conflict with c1, too, and as before keep
  -- our identity unchanged. The rest consists of adapting contexts.
  merge (lhs@(Conflictor com_r x cp) :\/: rhs@(Conflictor com_s y cq)) =
    case findCommonFL com_r com_s of
      Fork _ r s ->
        case cleanMerge (r :\/: s) of
          Just (s' :/\: r') ->
            let cp' = ctxAddInvFL s' cp
                cq' = ctxAddInvFL r' cq
                x' = cq' +| S.map (ctxAddInvFL s') x
                y' = cp' +| S.map (ctxAddInvFL r') y
            in Conflictor s' y' cq' :/\: Conflictor r' x' cp'
          Nothing ->
            -- Proof that this is impossible:
            --
            -- A conflictor reverts another patch only if it is the first that
            -- conflicts with it. Thus every patch it reverts is contained in
            -- its context as an unconflicted Prim patch. This holds for both
            -- lhs and rhs, which share the same context. Thus there can be no
            -- conflict between the effects of lhs and rhs. QED
            error $ renderString $ redText "uncommon effects can't be merged cleanly:"
              $$ redText "lhs:" $$ displayPatch lhs
              $$ redText "rhs:" $$ displayPatch rhs
              $$ redText "r:" $$ displayPatch r
              $$ redText "s:" $$ displayPatch s

-- * CommuteNoConflicts

instance (SignedId name, StorableId name, PrimPatch prim)
  => CommuteNoConflicts (RepoPatchV3 name prim) where

  -- two prim patches that commute
  commuteNoConflicts (Prim p :> Prim q) = do
    q' :> p' <- commute (p :> q)
    return $ Prim q' :> Prim p'
  -- commute a conflictor past a prim patch where everything goes smoothly
  -- i.e. everything in the lhs commutes past the rhs
  commuteNoConflicts (Conflictor r x cp :> Prim q) = do
    q' :> r' <- commuteRL (reverseFL r :> q)
    let iq = invert q
    cp' <- commutePast iq cp
    x' <- S.fromList <$> mapM (commutePast iq) (S.toList x)
    return $ Prim q' :> Conflictor (reverseRL r') x' cp'
  -- commute a prim patch past a conflictor where everything goes smoothly
  -- (completely symmetric to the previous case)
  commuteNoConflicts (Prim p :> Conflictor s y cq) = do
    s' :> p' <- commuteFL (p :> s)
    cq' <- commutePast p' cq
    y' <- S.fromList <$> mapM (commutePast p') (S.toList y)
    return $ Conflictor s' y' cq' :> Prim p'
  -- commuting a conflictor past another one where everything goes smoothly
  commuteNoConflicts (Conflictor com_r x cp :> Conflictor s y cq) = do
    -- com = prims in the effect of the lhs that the rhs also conflicts with;
    -- these remain on the lhs
    com :> rr <- commuteToPrefix (S.map (invertId . ctxId) y) com_r
    s' :> rr' <- commuteRLFL (rr :> s)
    let cp' = ctxAddInvFL s cp
        cq' = ctxAddRL rr' cq
    -- obviously p and q must not conflict, nor depend on each other
    guard (ctxNoConflict cq cp')
    let x' = S.map (ctxAddInvFL s) x
        y' = S.map (ctxAddRL rr') y
    -- somewhat less obviously, p must not conflict with the patches that only
    -- q conflicts with, nor depend on them, and vice versa
    guard $ all (ctxNoConflict cp') (S.difference y x')
    guard $ all (ctxNoConflict cq) (S.difference x' y)
    return $ Conflictor (com +>+ s') y' cq' :> Conflictor (reverseRL rr') x' cp'

-- * Commute

-- | Commute conflicting patches. These cases follow directly from merge.
commuteConflicting
  :: (SignedId name, StorableId name, PrimPatch prim)
  => CommuteFn (RepoPatchV3 name prim) (RepoPatchV3 name prim)
-- if we have a prim and a conflictor that only conflicts with that prim,
-- they trade places
-- [p] :> [p^, {:p}, :q] <-> [q] :> [q^, {:q}, :p]
commuteConflicting (Prim p :> Conflictor (ip:>:NilFL) ys cq@(ctxView -> Sealed (NilFL :> q)))
  | [ctxView -> Sealed (NilFL :> p')] <- S.toList ys
  , IsEq <- invert p =\/= ip
  , IsEq <- p =\/= p' =
      Just (Prim q :> Conflictor (invert q :>: NilFL) (S.singleton cq) (ctx p))
-- similar to above case: a prim and a conflictor that conflicts with the prim
-- but also conflicts with other patches
-- [p] :> [p^ s, {s^:p} U Y, cq] <-> [s, Y, cq] :> [, {cq}, s^:p]
commuteConflicting (Prim p :> Conflictor s y cq)
  | ident p `S.member` S.map ctxId y =
      case fastRemoveFL (invert p) s of
        Nothing ->
          -- Proof that this is impossible:
          --
          -- The case assumption (that p is in conflict with q) together
          -- with the fact that the rhs is obviously the first patch that
          -- conflicts with the lhs, imply that p^ is contained in s. It
          -- remains to be shown that p^ does not depend on any prim contained
          -- in s. Suppose there were such a prim, then p would be in conflict
          -- with it, which means p would have to be a conflictor. QED
          error $ renderString
            $ redText "commuteConflicting: cannot remove (invert lhs):"
            $$ displayPatch (invert p)
            $$ redText "from effect of rhs:"
            $$ displayPatch s
        Just r ->
          let cp = ctxAddInvFL r (ctx p)
          in Just (Conflictor r (cp -| y) cq :> Conflictor NilFL (S.singleton cq) cp)
-- if we have two conflictors where the rhs conflicts /only/ with the lhs,
-- the latter becomes a prim patch
-- [r, X, cp] [, {cp}, r^:q] <-> [q] [q^r, {r^:q} U X, cp]
commuteConflicting (lhs@(Conflictor r x cp) :> rhs@(Conflictor NilFL y cq))
  | y == S.singleton cp =
      case ctxView (ctxAddFL r cq) of
        Sealed (NilFL :> q') ->
          Just $ Prim q' :> Conflictor (invert q' :>: r) (cq +| x) cp
        Sealed (c' :> _) ->
          -- Proof that this is impossible:
          --
          -- First, it must be true that commutePastFL r cq = Just cq'. For if
          -- not, then there would be a conflict between the rhs and one of the
          -- prims that the lhs reverts, in contradiction to our case
          -- assumption that the rhs conflicts only with the lhs.
          --
          -- Second, suppose that cq' has residual nonempty context. That means
          -- there is a patch x in the history that the rhs depends on, and
          -- which is in conflict with at least one other patch y in our
          -- history (the history being the patches that precede the lhs);
          -- because otherwise cq' appended to the history would be a sequence
          -- that contains x twice without an intermediate revert. But then the
          -- rhs would also have to conflict with the patch x, again in
          -- contradiction to our case assumption. QED
          error $ renderString $ redText "remaining context in commute:"
            $$ displayPatch c'
            $$ redText "lhs:" $$ displayPatch lhs
            $$ redText "rhs:" $$ displayPatch rhs
-- conflicting conflictors where the rhs conflicts with lhs but
-- also conflicts with other patches
-- [com r, X, cp] [s, y=({s^cp} U Y'), cq] <-> [com s', r'Y', r'cq] [r', {cq} U s^X, s^cp]
commuteConflicting (Conflictor com_r x cp :> Conflictor s y cq)
  | let is_cp = ctxAddInvFL s cp
  , is_cp `S.member` y
  , let y' = is_cp -| y =
      case commuteToPrefix (S.map (invertId . ctxId) y') com_r of
        Nothing ->
          -- Proof that the above commute must suceed:
          --
          -- Let u and v be prims that the lhs reverts, and suppose v also
          -- conflicts with the rhs. If v^ depends on u^, then u depends on v
          -- and thus u also conflicts with the rhs. Thus any v^ in com_r such
          -- that v conflicts with the rhs can depend only on other elements of
          -- com_r that also conflict with the rhs. QED
          error "commuteConflicting: cannot commute common effects"
        Just (com :> rr) ->
          case commuteRLFL (rr :> s) of
            Nothing ->
              -- Proof that the above commute must succeed:
              --
              -- This is equivalent to the statement: a prim v that conflicts
              -- only with the lhs cannot depend on another prim u that
              -- conflicts only with the rhs. Again, this is a consequence of
              -- the fact that if v depends on u and u conflicts with q, then v
              -- must also conflict with q.
              error "commuteConflicting: cannot commute uncommon effects"
            Just (s' :> rr') ->
              Just $
                Conflictor (com +>+ s') (S.map (ctxAddRL rr') y') (ctxAddRL rr' cq)
                :>
                Conflictor (reverseRL rr') (cq +| S.map (ctxAddInvFL s) x) is_cp
commuteConflicting _ = Nothing

instance (SignedId name, StorableId name, PrimPatch prim) =>
         Commute (RepoPatchV3 name prim) where
  commute pair = commuteConflicting pair <|> commuteNoConflicts pair

-- * PatchInspect

-- Note: in contrast to RepoPatchV2 we do not look at the list of conflicts
-- here. I see no reason why we should: the conflicts are only needed for the
-- instance Commute. We do however look at the patches that we undo.
instance PatchInspect prim => PatchInspect (RepoPatchV3 name prim) where
  listTouchedFiles (Prim p) = listTouchedFiles p
  listTouchedFiles (Conflictor r _ cp) =
    nubSort $ concat (mapFL listTouchedFiles r) ++ ctxTouches cp
  hunkMatches f (Prim p) = hunkMatches f p
  hunkMatches f (Conflictor r _ cp) = hunkMatches f r || ctxHunkMatches f cp

-- * Boilerplate instances

instance (SignedId name, Eq2 prim, Commute prim) => Eq2 (RepoPatchV3 name prim) where
    (Prim p) =\/= (Prim q) = p =\/= q
    (Conflictor r x cp) =\/= (Conflictor s y cq)
        | IsEq <- r =\^/= s -- more efficient than IsEq <- r =\/= s
        , x == y
        , cp == cq = IsEq
    _ =\/= _ = NotEq

instance (Show name, Show2 prim) => Show (RepoPatchV3 name prim wX wY) where
  showsPrec d rp = showParen (d > appPrec) $
    case rp of
      Prim prim ->
        showString "Prim " . showsPrec2 (appPrec + 1) prim
      Conflictor r x cp -> showString "Conflictor " . showContent r x cp
    where
      showContent r x cp =
        showsPrec (appPrec + 1) r .
          showString " " . showsPrec (appPrec + 1) x .
          showString " " . showsPrec (appPrec + 1) cp

instance (Show name, Show2 prim) => Show1 (RepoPatchV3 name prim wX)

instance (Show name, Show2 prim) => Show2 (RepoPatchV3 name prim)

instance PrimPatch prim => PrimPatchBase (RepoPatchV3 name prim) where
  type PrimOf (RepoPatchV3 name prim) = prim

instance ToPrim (RepoPatchV3 name prim) where
  toPrim (Conflictor {}) = Nothing
  toPrim (Prim p) = Just (wnPatch p)

instance PatchDebug prim => PatchDebug (RepoPatchV3 name prim)

instance PrimPatch prim => Apply (RepoPatchV3 name prim) where
  type ApplyState (RepoPatchV3 name prim) = ApplyState prim
  apply = applyPrimFL . effect
  unapply = applyPrimFL . invert . effect

instance PatchListFormat (RepoPatchV3 name prim) where
  patchListFormat = ListFormatV3

instance IsHunk prim => IsHunk (RepoPatchV3 name prim) where
  isHunk rp = do
    Prim p <- return rp
    isHunk p

instance Summary (RepoPatchV3 name prim) where
  conflictedEffect (Conflictor _ _ (ctxView -> Sealed (_ :> p))) = [IsC Conflicted (wnPatch p)]
  conflictedEffect (Prim p) = [IsC Okay (wnPatch p)]

instance (Invert prim, Commute prim, Eq2 prim) => Unwind (RepoPatchV3 name prim) where
  fullUnwind (Prim p)
    = mkUnwound NilFL (wnPatch p :>: NilFL) NilFL
  fullUnwind
    (Conflictor
      (mapFL_FL wnPatch -> es)
      _
      (ctxView -> Sealed ((mapFL_FL wnPatch -> cs) :> (wnPatch -> i)))
    ) =
    mkUnwound
      (es +>+ cs)
      (i :>: NilFL)
      (invert i :>: invert cs +>+ NilFL)

-- * More boilerplate instances

instance PrimPatch prim => Check (RepoPatchV3 name prim)
  -- use the default implementation for method isInconsistent

instance PrimPatch prim => RepairToFL (RepoPatchV3 name prim)
  -- use the default implementation for method applyAndTryToFixFL

instance (SignedId name, StorableId name, PrimPatch prim)
  => ShowPatch (RepoPatchV3 name prim) where

  summary = plainSummary
  summaryFL = plainSummaryFL
  thing _ = "change"

instance (SignedId name, StorableId name, PrimPatch prim)
  => ShowContextPatch (RepoPatchV3 name prim) where

  showPatchWithContextAndApply f (Prim p) = showPatchWithContextAndApply f p
  showPatchWithContextAndApply f p = apply p >> return (showPatch f p)

-- * Read and Write

instance (SignedId name, StorableId name, PrimPatch prim)
  => ReadPatch (RepoPatchV3 name prim) where

  readPatch' = do
    skipSpace
    choice
      [ do string (BC.pack "conflictor")
           (Sealed r, x, p) <- readContent
           return (Sealed (Conflictor r (S.map unsafeCoerceP1 x) (unsafeCoerceP1 p)))
      , do mapSeal Prim <$> readPatch'
      ]
    where
      readContent = do
        r <- bracketedFL readPatch' '[' ']'
        x <- readCtxSet
        p <- readCtx
        return (r, x, p)
      readCtxSet = (lexString (BC.pack "{{") >> go) <|> pure S.empty
        where
          go = (lexString (BC.pack "}}") >> pure S.empty) <|> S.insert <$> readCtx <*> go

instance (SignedId name, StorableId name, PrimPatch prim)
  => ShowPatchBasic (RepoPatchV3 name prim) where

  showPatch fmt rp =
    case rp of
      Prim p -> showPatch fmt p
      Conflictor r x cp ->
        case fmt of
          ForStorage -> blueText "conflictor" <+> showContent r x cp
          ForDisplay ->
            vcat
            [ blueText "conflictor"
            , vcat (mapFL displayPatch r)
            , redText "v v v v v v v"
            , vcat [ displayCtx p $$ redText "*************" | p <- S.toList x ]
            , displayCtx cp
            , redText "^ ^ ^ ^ ^ ^ ^"
            ]
    where
      showContent r x cp = showEffect r <+> showCtxSet x $$ showCtx fmt cp
      showEffect NilFL = blueText "[]"
      showEffect ps = blueText "[" $$ vcat (mapFL (showPatch fmt) ps) $$ blueText "]"
      showCtxSet xs =
        case S.minView xs of
          Nothing -> mempty
          Just _ ->
            blueText "{{"
              $$ vcat (map (showCtx fmt) (S.toAscList xs))
              $$ blueText "}}"
      displayCtx c =
        -- need to use ForStorage to see the prim patch IDs
        showId ForStorage (ctxId c) $$
        unseal (showPatch ForDisplay . sortCoalesceFL . mapFL_FL wnPatch) (ctxToFL c)

-- * Local helper functions

infixr +|, -|

-- | A handy synonym for 'S.insert'.
(+|) :: Ord a => a -> S.Set a -> S.Set a
c +| cs = S.insert c cs

-- | A handy synonym for 'S.delete'.
(-|) :: Ord a => a -> S.Set a -> S.Set a
c -| cs = S.delete c cs
