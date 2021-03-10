{-# LANGUAGE UndecidableInstances #-}
module Darcs.Patch.Rebase.Legacy.Wrapped
  ( WrappedNamed(..)
  , fromRebasing
  ) where

import Darcs.Prelude

import Control.Applicative ( (<|>) )
import Data.Coerce ( coerce )

import Darcs.Patch.Effect ( Effect(..) )
import Darcs.Patch.Format ( PatchListFormat(..), ListFormat )
import Darcs.Patch.Info ( PatchInfo )
import Darcs.Patch.FromPrim ( FromPrim, PrimPatchBase(..) )
import Darcs.Patch.Named ( Named(..) )
import Darcs.Patch.Read ( ReadPatch(..) )
import Darcs.Patch.Rebase.Suspended ( Suspended, readSuspended )
import Darcs.Patch.RepoPatch ( RepoPatch )
import Darcs.Patch.RepoType
  ( RepoType(..), IsRepoType(..), SRepoType(..)
  , RebaseType(..), SRebaseType(..)
  )

import Darcs.Patch.Witnesses.Sealed ( Sealed(..), mapSeal )
import Darcs.Patch.Witnesses.Ordered ( FL(..), mapFL_FL )

-- |A patch that lives in a repository where an old-style rebase is in
-- progress. Such a repository will consist of @Normal@ patches
-- along with exactly one @Suspended@ patch.
--
-- It is here only so that we can upgrade an old-style rebase.
--
-- @NormalP@ represents a normal patch within a respository where a
-- rebase is in progress. @NormalP p@ is given the same on-disk
-- representation as @p@, so a repository can be switched into
-- and out of rebasing mode simply by adding or removing a
-- @RebaseP@ patch and setting the appropriate format flag.
--
-- Note that the witnesses are such that the @RebaseP@
-- patch has no effect on the context of the rest of the
-- repository; in a sense the patches within it are
-- dangling off to one side from the main repository.
data WrappedNamed (rt :: RepoType) p wX wY where
  NormalP :: !(Named p wX wY) -> WrappedNamed rt p wX wY
  RebaseP
    :: (PrimPatchBase p, FromPrim p, Effect p)
    => !PatchInfo
    -> !(Suspended p wX)
    -> WrappedNamed ('RepoType 'IsRebase) p wX wX

fromRebasing :: WrappedNamed rt p wX wY -> Named p wX wY
fromRebasing (NormalP n) = n
fromRebasing (RebaseP {}) = error "internal error: found rebasing internal patch"

-- This is a local hack to maintain backwards compatibility with
-- the on-disk format for rebases. Previously the rebase container
-- was internally represented via a 'Rebasing' type that sat *inside*
-- a 'Named', and so the rebase container patch had the structure
-- 'NamedP i [] (Suspendended s :>: NilFL)'. This structure was reflected
-- in the way it was saved on disk.
-- The easiest to read this structure is to use an intermediate type
-- that reflects the old structure.
-- Cleaning this up is obsolete since this module is only here for upgrading
-- the legacy rebase format where the rebase patch was mixed in with regular
-- patches.
data ReadRebasing p wX wY where
  ReadNormal    :: p wX wY -> ReadRebasing p wX wY
  ReadSuspended :: Suspended p wX -> ReadRebasing p wX wX

instance (RepoPatch p, IsRepoType rt) => ReadPatch (WrappedNamed rt p) where
  readPatch' =
    case singletonRepoType :: SRepoType rt of
      SRepoType SIsRebase ->
        let wrapNamed :: Named (ReadRebasing p) wX wY -> WrappedNamed rt p wX wY
            wrapNamed (NamedP i [] (ReadSuspended s :>: NilFL)) = RebaseP i s
            wrapNamed (NamedP i deps ps) =
              NormalP (NamedP i deps (mapFL_FL unRead ps))

            unRead (ReadNormal p) = p
            unRead (ReadSuspended _) = error "unexpected suspended patch"

        in fmap (mapSeal wrapNamed) readPatch'

      _ -> fmap (mapSeal NormalP) readPatch'

instance PatchListFormat p => PatchListFormat (ReadRebasing p) where
  patchListFormat = coerce (patchListFormat :: ListFormat p)

instance RepoPatch p => ReadPatch (ReadRebasing p) where
  readPatch' =
    Sealed . ReadSuspended <$> readSuspended <|> mapSeal ReadNormal <$> readPatch'
