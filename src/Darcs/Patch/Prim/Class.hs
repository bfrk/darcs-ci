module Darcs.Patch.Prim.Class
    ( PrimConstruct(..)
    , PrimCoalesce(..)
    , PrimDetails(..)
    , PrimSift(..)
    , PrimShow(..)
    , PrimRead(..)
    , PrimApply(..)
    , PrimPatch
    , PrimMangleUnravelled(..)
    , Mangled
    , Unravelled
    , showPrimCtx
    , primCleanMerge
    )
    where

import Darcs.Prelude

import Darcs.Patch.ApplyMonad ( ApplyMonad )
import Darcs.Patch.FileHunk ( IsHunk(..) )
import Darcs.Patch.Format ( FileNameFormat, PatchListFormat )
import Darcs.Patch.Inspect ( PatchInspect )
import Darcs.Patch.Apply ( Apply(..), ObjectIdOfPatch )
import Darcs.Patch.Commute ( Commute(..) )
import Darcs.Patch.CommuteFn ( PartialMergeFn )
import Darcs.Patch.Invert ( Invert(..) )
import Darcs.Patch.Merge ( CleanMerge(..) )
import Darcs.Patch.Object ( ObjectId )
import Darcs.Patch.Read ( ReadPatch )
import Darcs.Patch.Repair ( RepairToFL )
import Darcs.Patch.Show ( ShowPatch, ShowContextPatch )
import Darcs.Patch.SummaryData ( SummDetail )
import Darcs.Patch.Viewing ( showContextHunk )
import Darcs.Patch.Witnesses.Eq ( Eq2(..), EqCheck )
import Darcs.Patch.Witnesses.Ordered ( (:/\:)(..), (:>)(..), (:\/:)(..), FL )
import Darcs.Patch.Witnesses.Show ( Show2 )
import Darcs.Patch.Witnesses.Sealed ( Sealed )

import Darcs.Util.Parser ( Parser )
import Darcs.Util.Path ( AnchoredPath )
import Darcs.Util.Printer ( Doc )

import qualified Data.ByteString as B ( ByteString )


type PrimPatch prim =
    ( Apply prim
    , CleanMerge prim
    , Commute prim
    , Invert prim
    , Eq2 prim
    , IsHunk prim
    , PatchInspect prim
    , RepairToFL prim
    , Show2 prim
    , PrimConstruct prim
    , PrimCoalesce prim
    , PrimDetails prim
    , PrimApply prim
    , PrimSift prim
    , PrimMangleUnravelled prim
    , ReadPatch prim
    , ShowPatch prim
    , ShowContextPatch prim
    , PatchListFormat prim
    )

class PrimConstruct prim where
   addfile :: AnchoredPath -> prim wX wY
   rmfile :: AnchoredPath -> prim wX wY
   adddir :: AnchoredPath -> prim wX wY
   rmdir :: AnchoredPath -> prim wX wY
   move :: AnchoredPath -> AnchoredPath -> prim wX wY
   changepref :: String -> String -> String -> prim wX wY
   hunk :: AnchoredPath -> Int -> [B.ByteString] -> [B.ByteString] -> prim wX wY
   tokreplace :: AnchoredPath -> String -> String -> String -> prim wX wY
   binary :: AnchoredPath -> B.ByteString -> B.ByteString -> prim wX wY

class (Commute prim, Eq2 prim, Invert prim) => PrimCoalesce prim where
   -- | Try to shrink the input sequence by getting rid of self-cancellations
   -- and identity patches or by coalescing patches. Also sort patches
   -- according to some internally defined order (specific to the patch type)
   -- as far as possible while respecting dependencies.
   -- A result of 'Nothing' means that we could not shrink the input.
   --
   -- This method is included in the class for optimization. Instances are free
   -- to use 'Darcs.Patch.Prim.Coalesce.defaultTryToShrink'.
   tryToShrink :: FL prim wX wY -> Maybe (FL prim wX wY)

   -- | This is similar to 'tryToShrink' but always gives back a result: if the
   -- sequence could not be shrunk we merely give back a sorted version.
   --
   -- This method is included in the class for optimization. Instances are free
   -- to use 'Darcs.Patch.Prim.Coalesce.defaultSortCoalesceFL'.
   sortCoalesceFL :: FL prim wX wY -> FL prim wX wY

   -- | Coalesce adjacent patches to one with the same effect.
   --
   -- prop> apply (primCoalesce p q) == apply p >> apply q
   primCoalesce :: prim wX wY -> prim wY wZ -> Maybe (prim wX wZ)

   -- | Whether prim patch has no effect at all and thus can be eliminated
   -- as far as coalescing is concerned.
   isIdentity :: prim wX wY -> EqCheck wX wY

   -- | Provide a total order between arbitrary patches that is consistent
   -- with 'Eq2':
   --
   -- prop> unsafeCompare p q == IsEq  <=>  comparePrim p q == EQ
   comparePrim :: prim wA wB -> prim wC wD -> Ordering

-- | Prim patches that support "sifting". This is the process of eliminating
-- changes from a sequence of prims that can be recovered by comparing states
-- (normally the pristine and working states), except those that other changes
-- depend on. In other words, changes to the content of (tracked) files. The
-- implementation is allowed and expected to shrink and coalesce changes in the
-- process.
class PrimSift prim where
  -- | Whether a prim is a candidate for sifting
  primIsSiftable :: prim wX wY -> Bool

class PrimDetails prim where
   summarizePrim :: prim wX wY -> [SummDetail]

class PrimShow prim where
   showPrim :: FileNameFormat -> prim wA wB -> Doc

showPrimCtx
  :: ( PrimShow prim
     , ApplyMonad (ApplyState prim) m
     , IsHunk prim
     , ObjectId (ObjectIdOfPatch prim)
     , Apply prim
     )
  => FileNameFormat
  -> prim wA wB
  -> m Doc
showPrimCtx fmt p =
  case isHunk p of
    Just fh -> do
      r <- showContextHunk fmt fh
      apply p
      return r
    Nothing -> do
      apply p
      return $ showPrim fmt p

class PrimRead prim where
   readPrim :: FileNameFormat -> Parser (Sealed (prim wX))

class PrimApply prim where
   applyPrimFL :: ApplyMonad (ApplyState prim) m => FL prim wX wY -> m ()

-- | A list of conflicting alternatives. They form a connected
-- component of the conflict graph i.e. one transitive conflict.
type Unravelled prim wX = [Sealed (FL prim wX)]

-- | Result of mangling a single Unravelled.
type Mangled prim wX = Sealed (prim wX)

class PrimMangleUnravelled prim where
  -- | Mangle conflicting alternatives if possible.
  mangleUnravelled :: Unravelled prim wX -> Maybe (Mangled prim wX)

primCleanMerge :: (Commute prim, Invert prim) => PartialMergeFn prim prim
primCleanMerge (p :\/: q) = do
  q' :> ip' <- commute (invert p :> q)
  return $ q' :/\: invert ip'
