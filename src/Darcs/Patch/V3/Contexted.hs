-- | 'Contexted' patches.

{-# LANGUAGE ViewPatterns #-}
module Darcs.Patch.V3.Contexted
    ( -- * Contexted patches
      Contexted
      -- * Query
    , ctxId
    , ctxView
    , ctxNoConflict
    , ctxToFL
    , ctxDepends
      -- * Construct / Modify
    , ctx
    , ctxAdd
    , ctxAddRL
    , ctxAddInvFL
    , ctxAddFL
    , commutePast
    , commutePastRL
      -- * 'PatchInspect' helpers
    , ctxTouches
    , ctxHunkMatches
      -- * 'ReadPatch' and 'ShowPatch' helpers
    , showCtx
    , readCtx
    , formatCtx
      -- * Properties
    , prop_ctxInvariants
    , prop_ctxEq
    , prop_ctxPositive
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC ( pack )
import Data.Maybe ( isNothing, isJust )

import Darcs.Prelude

import Darcs.Patch.Commute
import Darcs.Patch.Format ( FormatPatch(..) )
import Darcs.Patch.Ident
import Darcs.Patch.Invert
import Darcs.Patch.Inspect
import Darcs.Patch.Merge ( CleanMerge(..) )
import Darcs.Patch.Read ( ReadPatch(..), ReadPatches(..) )
import Darcs.Patch.Permutations ( (=\~/=) )
import Darcs.Util.Parser ( Parser, lexString )
import Darcs.Patch.Show ( ShowPatchBasic(..) )
import Darcs.Patch.Viewing ()
import Darcs.Patch.Witnesses.Eq
import Darcs.Patch.Witnesses.Ordered
import Darcs.Patch.Witnesses.Sealed
import Darcs.Patch.Witnesses.Show

import Darcs.Util.Path ( AnchoredPath )
import Darcs.Util.Printer
import qualified Darcs.Util.Format as F

{-
| (Definition 10.1) A 'Contexted' patch is a patch transferred to, or viewed
from, a different context.

More precisely we make the following definitions:

* A /context/ for a patch @p@ is a sequence of patches that @p@ depends on,
  and such that it never contains a patch and its inverse.

* A 'Contexted' patch is a patch @p@ together with a context for @p@, such
  that the end state of the patch and its context is hidden (existentially
  quantified).

The definition of context above is chosen so that this sequence is minimal.
-}
data Contexted p wX where
  Contexted :: FL p wX wY -> p wY wZ -> Contexted p wX

-- | Equality between 'Contexted' patches reduces to equality of the
-- identifiers of the patches referred to /if/ we look at them from the same
-- context. (This assumes witnesses aren't coerced in an unsafe manner.)
instance Ident p => Eq (Contexted p wX) where
  c1 == c2 = ctxId c1 == ctxId c2

instance Ident p => Ord (Contexted p wX) where
  cp `compare` cq = ctxId cp `compare` ctxId cq

instance Show2 p => Show (Contexted p wX) where
  showsPrec d (Contexted ps p) =
    showParen (d > appPrec) $ showString "Contexted " .
    showsPrec2 (appPrec + 1) ps . showString " " .
    showsPrec2 (appPrec + 1) p

instance Show2 p => Show1 (Contexted p)

-- | This property states that no prefix of the context commutes with the rest
-- of the 'Contexted' patch and that the context never contains a patch
-- and its inverse.
prop_ctxInvariants :: (Commute p, Invert p, SignedIdent p) => Contexted p wX -> Bool
prop_ctxInvariants (Contexted NilFL _) = True
prop_ctxInvariants c@(Contexted (_ :>: ps) q) =
  prop_ctxInvariants (Contexted ps q) && prop_ctxNotCom c && prop_ctxNotInv c

-- | This property states that the first patch in the context must not
-- commute with the rest of the 'Contexted' patch.
prop_ctxNotCom :: Commute p => Contexted p wX -> Bool
prop_ctxNotCom (Contexted NilFL _) = True
prop_ctxNotCom (Contexted (p :>: ps) q) =
  isNothing $ commuteFL (p :> ps +>+ q :>: NilFL)

-- | This property states that patches in the context of a 'Contexted' patch as
-- well as the patch itself are positive. It does /not/ necessarily hold for all
-- 'Contexted' patches.
prop_ctxPositive :: SignedIdent p => Contexted p wX -> Bool
prop_ctxPositive (Contexted ps p) =
  allFL (positiveId . ident) ps && positiveId (ident p)

-- | This property states that the inverse of the first patch in the context
-- is not contained in the rest of the context.
prop_ctxNotInv :: SignedIdent p => Contexted p wX -> Bool
prop_ctxNotInv (Contexted NilFL _) = True
prop_ctxNotInv (Contexted (p :>: ps) _) =
  invertId (ident p) `notElem` mapFL ident ps

-- | This property states that equal 'Contexted' patches have equal content
-- up to reorderings of the context patches.
prop_ctxEq :: (Commute p, Eq2 p, Ident p) => Contexted p wX -> Contexted p wX -> Bool
prop_ctxEq cp@(Contexted ps p) cq@(Contexted qs q)
  | cp == cq =
      case ps =\~/= qs of
        IsEq -> isIsEq (p =\/= q)
        NotEq -> False
prop_ctxEq _ _ = True

-- * Query

-- | Identity of a contexted patch.
{-# INLINE ctxId #-}
ctxId :: Ident p => Contexted p wX -> PatchId p
ctxId (Contexted _ p) = ident p

-- | Wether the first argument is contained (identity-wise) in the context of
-- the second, in other words, the second depends on the first. This does not
-- include equality, only proper dependency.
ctxDepends :: Ident p => Contexted p wX -> Contexted p wX -> Bool
ctxDepends (Contexted _ p1) (Contexted c2 _) = ident p1 `elem` mapFL ident c2

-- | 'Contexted' patches conflict with each other if the identity of one is in
-- the context of the other or they cannot be merged cleanly.
ctxNoConflict :: (CleanMerge p, Commute p, Ident p)
              => Contexted p wX -> Contexted p wX -> Bool
ctxNoConflict cp cq | cp == cq = True
ctxNoConflict (Contexted ps p) (Contexted qs q)
  | ident p `elem` mapFL ident qs || ident q `elem` mapFL ident ps = False
  | otherwise =
      case findCommonFL ps qs of
        Fork _ ps' qs' ->
          isJust $ cleanMerge (ps' +>+ p :>: NilFL :\/: qs' +>+ q :>: NilFL)

{-
-- This is (Definition 10.4) of the paper.
-- It misses a case for equal contexted patches and is also quite slow.
ctxNoConflict (Contexted cs p) cq =
  isJust $ commutePast (invert p) (ctxAddInvFL cs cq)
-}

-- | We sometimes want to pattern match on a 'Contexted' patch but still guard
-- against violation of the invariants. So we export a view that is isomorphic
-- to the 'Contexted' type but doesn't allow to manipulate the internals.
ctxView :: Contexted p wX -> Sealed ((FL p :> p) wX)
ctxView (Contexted cs p) = Sealed (cs :> p)

-- | Convert a 'Contexted' patch into a plain 'FL' with the patch at the end.
ctxToFL :: Contexted p wX -> Sealed (FL p wX)
ctxToFL (ctxView -> Sealed (ps :> p)) = Sealed (ps +>+ p :>: NilFL)

-- * Construct

-- | A 'Contexted' patch with empty context.
ctx :: p wX wY -> Contexted p wX
ctx p = Contexted NilFL p

-- | Add a patch to the context of a 'Contexted' patch. This is
-- the place where we take care of the invariants.
ctxAdd :: (Commute p, Invert p, Ident p)
       => p wX wY -> Contexted p wY -> Contexted p wX
ctxAdd p (Contexted ps q)
  | Just ps' <- fastRemoveFL (invert p) ps = Contexted ps' q
ctxAdd p c@(Contexted ps q) =
  case commutePast p c of
    Just c' -> c'
    Nothing -> Contexted (p :>: ps) q

-- | Add an 'RL' of patches to the context.
ctxAddRL :: (Commute p, Invert p, Ident p)
         => RL p wX wY -> Contexted p wY -> Contexted p wX
ctxAddRL NilRL cp = cp
ctxAddRL (ps :<: p) cp = ctxAddRL ps (ctxAdd p cp)

-- | Add an 'FL' of patches to the context but invert it first.
ctxAddInvFL :: (Commute p, Invert p, Ident p)
            => FL p wX wY -> Contexted p wX -> Contexted p wY
ctxAddInvFL = ctxAddRL . invertFL

-- | Add an 'FL' of patches to the context.
ctxAddFL :: (Commute p, Invert p, Ident p)
         => FL p wX wY -> Contexted p wY -> Contexted p wX
ctxAddFL NilFL t = t
ctxAddFL (p :>: ps) t = ctxAdd p (ctxAddFL ps t)

-- | (Definition 10.2) Commute a patch past a 'Contexted' patch. This
-- commutes it past the context and then past the patch itself. If it
-- succeeds, the patch that we commuted past gets dropped.
-- Note that this does /not/ succeed if the inverted patch is in the
-- 'Contexted' patch.
commutePast :: Commute p
            => p wX wY -> Contexted p wY -> Maybe (Contexted p wX)
commutePast q (Contexted ps p) = do
  ps' :> q' <- commuteFL (q :> ps)
  p' :> _ <- commute (q' :> p)
  return (Contexted ps' p')

-- | Not defined in the paper but used in the commute algorithm.
commutePastRL :: Commute p
              => RL p wX wY -> Contexted p wY -> Maybe (Contexted p wX)
commutePastRL = foldRL_M commutePast

-- * 'PatchInspect' helpers

ctxTouches :: PatchInspect p => Contexted p wX -> [AnchoredPath]
ctxTouches (Contexted ps p) =
  concat $ listTouchedFiles p : mapFL listTouchedFiles ps

ctxHunkMatches :: PatchInspect p => (B.ByteString -> Bool)
               -> Contexted p wX -> Bool
ctxHunkMatches f (Contexted ps p) = hunkMatches f ps || hunkMatches f p

-- * 'ReadPatch' and 'ShowPatch' helpers

-- For storage it would be enough to read/write the patch identifiers in the
-- context. But this means that we need access to the patches preceding us.
-- So these functions would no longer be independent of context.

showCtx :: ShowPatchBasic p => Contexted p wX -> Doc
showCtx (Contexted c p) =
  hiddenPrefix "|" (showPatch c) $$ hiddenPrefix "|" (blueText ":") $$ showPatch p

formatCtx :: FormatPatch p => Contexted p wX -> F.Format
formatCtx (Contexted c p) = F.vcat [formatPatchFL c, F.ascii ":", formatPatch p]

readCtx :: ReadPatches p => Parser (Contexted p wX)
readCtx = do
  Sealed ps <- readPatchFL'
  lexString (BC.pack ":")
  Sealed p <- readPatch'
  return $ Contexted ps p
