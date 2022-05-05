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
    , primCleanMerge
    )
    where

import Darcs.Prelude

import Darcs.Patch.ApplyMonad ( ApplyMonad )
import Darcs.Patch.FileHunk ( FileHunk, IsHunk )
import Darcs.Patch.Format ( FileNameFormat, PatchListFormat )
import Darcs.Patch.Inspect ( PatchInspect )
import Darcs.Patch.Apply ( Apply(..) )
import Darcs.Patch.Commute ( Commute(..) )
import Darcs.Patch.CommuteFn ( PartialMergeFn )
import Darcs.Patch.Invert ( Invert(..) )
import Darcs.Patch.Merge ( CleanMerge(..) )
import Darcs.Patch.Read ( ReadPatch )
import Darcs.Patch.Repair ( RepairToFL )
import Darcs.Patch.Show ( ShowPatch, ShowContextPatch )
import Darcs.Patch.SummaryData ( SummDetail )
import Darcs.Patch.Witnesses.Eq ( Eq2(..) )
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
   primFromHunk :: FileHunk wX wY -> prim wX wY

class PrimCoalesce prim where
   -- | Try to shrink the input sequence by getting rid of self-cancellations
   -- and identity patches or by coalescing patches. Also sort patches
   -- according to some internally defined order (specific to the patch type)
   -- as far as possible while respecting dependencies.
   -- A result of 'Nothing' means that we could not shrink the input.
   tryToShrink :: FL prim wX wY -> Maybe (FL prim wX wY)

   -- | This is similar to 'tryToShrink' but always gives back a result: if the
   -- sequence could not be shrunk we merely give back a sorted version.
   sortCoalesceFL :: FL prim wX wY -> FL prim wX wY

   -- | Either 'primCoalesce' or cancel inverses.
   --
   -- prop> primCoalesce (p :> q) == Just r => apply r = apply p >> apply q
   -- prop> primCoalesce (p :> q) == Just r => lengthFL r < 2
   coalesce :: (prim :> prim) wX wY -> Maybe (FL prim wX wY)

   -- | Coalesce adjacent patches to one with the same effect.
   --
   -- prop> apply (primCoalesce p q) == apply p >> apply q
   primCoalesce :: prim wX wY -> prim wY wZ -> Maybe (prim wX wZ)

   -- | If 'primCoalesce' is addition, then this is subtraction.
   --
   -- prop> Just r == primCoalesce p q => primDecoalesce r p == Just q
   primDecoalesce :: prim wX wZ -> prim wX wY -> Maybe (prim wY wZ)

-- | Prim patches that support "sifting". This is the process of eliminating
-- changes from a sequence of prims that can be recovered by comparing states
-- (normally the pristine and working states), except those that other changes
-- depend on. In other words, changes to the content of (tracked) files. The
-- implementation is allowed and expected to shrink and coalesce changes in the
-- process.
class PrimSift prim where
  -- | Simplify the candidate pending patch through a combination of looking
  -- for self-cancellations (sequences of patches followed by their inverses),
  -- coalescing, and getting rid of any hunk or binary patches we can commute
  -- out the back.
  --
  -- More abstractly, for an argument @p@, pristine state @R@, and working
  -- state @U@, define
  --
  -- > unrecorded p = p +>+ diff (pureApply p R) U
  --
  -- Then the resulting sequence @p'@ must maintain that equality, i.e.
  --
  -- > unrecorded p = unrecorded (siftForPending p)
  --
  -- while trying to "minimize" @p@.
  siftForPending :: FL prim wX wY -> Sealed (FL prim wX)

class PrimDetails prim where
   summarizePrim :: prim wX wY -> [SummDetail]

class PrimShow prim where
   showPrim :: FileNameFormat -> prim wA wB -> Doc
   showPrimCtx :: ApplyMonad  (ApplyState prim) m => FileNameFormat -> prim wA wB -> m Doc

class PrimRead prim where
   readPrim :: FileNameFormat -> Parser (Sealed (prim wX))

class PrimApply prim where
   applyPrimFL :: ApplyMonad (ApplyState prim) m => FL prim wX wY -> m ()

-- | A list of conflicting alternatives. They form a connected
-- component of the conflict graph i.e. one transitive conflict.
type Unravelled prim wX = [Sealed (FL prim wX)]

-- | Result of mangling a single Unravelled.
type Mangled prim wX = Sealed (FL prim wX)

class PrimMangleUnravelled prim where
  -- | Mangle conflicting alternatives if possible.
  mangleUnravelled :: Unravelled prim wX -> Maybe (Mangled prim wX)

primCleanMerge :: (Commute prim, Invert prim) => PartialMergeFn prim prim
primCleanMerge (p :\/: q) = do
  q' :> ip' <- commute (invert p :> q)
  return $ q' :/\: invert ip'
