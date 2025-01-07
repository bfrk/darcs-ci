{-# LANGUAGE UndecidableInstances #-}
module Darcs.Patch.Rebase.Suspended
    ( Suspended(..)
    , countToEdit, simplifyPush, simplifyPushes
    , addFixupsToSuspended, removeFixupsFromSuspended
    , addToEditsToSuspended
    , readSuspended
    , showSuspended
    ) where

import Darcs.Prelude

import Darcs.Patch.Effect ( Effect(..) )
import Darcs.Patch.Invert ( invert )
import Darcs.Patch.Named ( Named(..) )
import Darcs.Patch.Info ( replaceJunk )
import Darcs.Patch.Read ( ReadPatch(..) )
import Darcs.Patch.FromPrim ( PrimPatchBase(..) )
import Darcs.Patch.Read ( bracketedFL )
import Darcs.Patch.Rebase.Fixup ( RebaseFixup(..), namedToFixups )
import Darcs.Patch.Rebase.Name ( RebaseName(..) )
import Darcs.Patch.RepoPatch ( RepoPatch )
import qualified Darcs.Patch.Rebase.Change as Change ( simplifyPush, simplifyPushes )
import Darcs.Patch.Rebase.Change ( RebaseChange(..), addNamedToRebase )
import Darcs.Patch.Rebase.Legacy.Item as Item ( toRebaseChanges )
import Darcs.Patch.Show ( ShowPatchBasic(..), ShowPatchFor )
import Darcs.Util.Parser ( Parser, lexString, lexWord )
import Darcs.Patch.Witnesses.Ordered
import Darcs.Patch.Witnesses.Sealed
import Darcs.Patch.Witnesses.Show ( Show2 )
import Darcs.Util.Printer ( Doc, vcat, text, blueText, ($$), (<+>) )
import qualified Darcs.Util.Diff as D ( DiffAlgorithm(MyersDiff) )

import Control.Applicative ( (<|>) )
import qualified Data.ByteString.Char8 as BC ( pack )

-- | A @Suspended@ patch contains the entire rebase state, in the form
-- of 'RebaseChange's. The end state is existientially quantified and
-- thus hidden.
data Suspended p wX where
    Items :: FL (RebaseChange (PrimOf p)) wX wY -> Suspended p wX

deriving instance (Show2 p, Show2 (PrimOf p)) => Show (Suspended p wX)

showSuspended :: PrimPatchBase p
              => ShowPatchFor -> Suspended p wX -> Doc
showSuspended f (Items ps)
       = blueText "rebase" <+> text "0.2" <+> blueText "{"
         $$ vcat (mapFL (showPatch f) ps)
         $$ blueText "}"

readSuspended :: forall p wX. RepoPatch p => Parser (Suspended p wX)
readSuspended =
    do lexString (BC.pack "rebase")
       version <- lexWord
       case () of
         _ | version == BC.pack "0.2" ->
              (lexString (BC.pack "{}") >> return (Items NilFL))
              <|>
              (unseal Items <$> bracketedFL readPatch' '{' '}')
           -- version 0.1 was a very temporary intermediate state on the way to 0.2
           -- and we don't offer an upgrade path for it.
           | version == BC.pack "0.0" ->
               -- Note that if we have an "old-style" rebase, i.e. the first
               -- rebase implementation in darcs, characterised by the format
               -- string "rebase-in-progress", then only version 0.0 is
               -- possible here. On the other hand, the more recent
               -- implementation could use any version including 0.0.
               -- Unlike version 0.2, version 0.0 rebase patches on disk can
               -- contain conflicts. These are removed when reading by
               -- Item.toRebaseChanges, which ultimately calls 'fullUnwind',
               -- the same machinery that is used when version 0.2 patches are
               -- created from scratch.
               (lexString (BC.pack "{}") >> return (Items NilFL))
               <|>
               (unseal Items . unseal (Item.toRebaseChanges @p) <$>
                bracketedFL readPatch' '{' '}')
           | otherwise -> error $ "can't handle rebase version " ++ show version

countToEdit :: Suspended p wX -> Int
countToEdit (Items ps) = lengthFL ps

-- |add fixups for the name and effect of a patch to a 'Suspended'
addFixupsToSuspended
  :: (PrimPatchBase p, Effect p)
  => Named p wX wY
  -> Suspended p wY
  -> Suspended p wX
addFixupsToSuspended p = simplifyPushes D.MyersDiff (namedToFixups p)

-- | Remove fixups (actually, add their inverse) for the name and effect of
-- a patch to a 'Suspended'.
removeFixupsFromSuspended
  :: (PrimPatchBase p, Effect p)
  => Named p wX wY
  -> Suspended p wX
  -> Suspended p wY
removeFixupsFromSuspended p =
  simplifyPushes D.MyersDiff (invert (namedToFixups p))

-- | Add 'Named' patches for editing to a 'Suspended'. The patches to be
-- suspended are renamed by replacing the junk in their 'Patchinfo'.
--
-- The reason we rename patches immediately when suspending them is that
-- the user may pull an identical copy from a clone, Which means we have
-- the same patch name twice, once in the normal repo and once suspended.
-- Furthermore, they can again suspend that copy, leaving us with multiple
-- copies of the same patch in the rebase state. This is bad because it
-- invalidates most of the invariants for RebaseName fixups. See issue2445
-- and tests/rebase-repull.sh for examples which lead to crashes when we
-- don't do the renaming here.
addToEditsToSuspended
  :: RepoPatch p
  => D.DiffAlgorithm
  -> FL (Named p) wX wY
  -> Suspended p wY
  -> IO (Suspended p wX)
addToEditsToSuspended _ NilFL items = return items
addToEditsToSuspended da (NamedP old ds ps :>: qs) items = do
  Items items' <- addToEditsToSuspended da qs items
  new <- replaceJunk old
  return $
    unseal Items $
    unseal (addNamedToRebase da (NamedP new ds ps)) $
    Change.simplifyPush da (NameFixup (Rename new old)) items'

simplifyPush
  :: PrimPatchBase p
  => D.DiffAlgorithm
  -> RebaseFixup (PrimOf p) wX wY
  -> Suspended p wY
  -> Suspended p wX
simplifyPush da fixups (Items ps) =
  unseal Items (Change.simplifyPush da fixups ps)

simplifyPushes
  :: PrimPatchBase p
  => D.DiffAlgorithm
  -> FL (RebaseFixup (PrimOf p)) wX wY
  -> Suspended p wY
  -> Suspended p wX
simplifyPushes da fixups (Items ps) =
  unseal Items (Change.simplifyPushes da fixups ps)
