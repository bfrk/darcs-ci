-- Copyright (C) 2006-2007 David Roundy
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
-- along with this program; if not, write to the Free Software Foundation,
-- Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
module Darcs.Repository.InternalTypes
    ( Repository
    , PristineType(..)
    , AccessType(..)
    , SAccessType(..)
    , repoAccessType
    , repoCache
    , modifyCache
    , repoFormat
    , repoLocation
    , withRepoDir
    , repoPristineType
    , unsafeCoerceRepoType
    , unsafeCoercePatchType
    , unsafeCoerceR
    , unsafeCoerceU
    , unsafeEndTransaction
    , unsafeStartTransaction
    , mkRepo
    ) where

import Darcs.Prelude

import Darcs.Util.Cache ( Cache )
import Darcs.Repository.Format ( RepoFormat )
import Darcs.Util.Path ( AbsoluteOrRemotePath, toPath )
import System.Directory ( withCurrentDirectory )
import Unsafe.Coerce ( unsafeCoerce )

data PristineType
  = NoPristine
  | PlainPristine
  | HashedPristine
    deriving ( Show, Eq )

data AccessType = RO | RW deriving (Eq)

data SAccessType (rt :: AccessType) where
  SRO :: SAccessType 'RO
  SRW :: SAccessType 'RW

-- |A @Repository@ is a token representing the state of a repository on disk.
-- It is parameterized by
--
-- [@rt@] the access type (whether we are in a transaction or not),
-- [@p@]  the patch type,
-- [@wU@] the witness for the unrecorded state (what's in the working tree now).
-- [@wR@] the witness for
--
--        * the recorded state when outside a transaction, or
--        * the tentative state when inside a transaction.
data Repository (rt :: AccessType) (p :: * -> * -> *) wU wR =
  Repo !String !RepoFormat !PristineType Cache (SAccessType rt)

type role Repository nominal nominal nominal nominal

repoLocation :: Repository rt p wU wR -> String
repoLocation (Repo loc _ _ _ _) = loc

-- | Perform an action with the current working directory set to the
-- 'repoLocation'.
withRepoDir :: Repository rt p wU wR -> IO a -> IO a
withRepoDir repo = withCurrentDirectory (repoLocation repo)

repoFormat :: Repository rt p wU wR -> RepoFormat
repoFormat (Repo _ fmt _ _ _) = fmt

repoPristineType :: Repository rt p wU wR -> PristineType
repoPristineType (Repo _ _ pr _ _) = pr

repoCache :: Repository rt p wU wR -> Cache
repoCache (Repo _ _ _ c _) = c

modifyCache :: (Cache -> Cache) -> Repository rt p wU wR -> Repository rt p wU wR
modifyCache g (Repo l f p c a) = Repo l f p (g c) a

repoAccessType :: Repository rt p wU wR -> SAccessType rt
repoAccessType (Repo _ _ _ _ s) = s

unsafeCoerceRepoType :: Repository rt p wU wR -> Repository rt' p wU wR
unsafeCoerceRepoType = unsafeCoerce

unsafeCoercePatchType :: Repository rt p wU wR -> Repository rt p' wU wR
unsafeCoercePatchType = unsafeCoerce

unsafeCoerceR :: Repository rt p wU wR -> Repository rt p wU wR'
unsafeCoerceR = unsafeCoerce

unsafeCoerceU :: Repository rt p wU wR -> Repository rt p wU' wR
unsafeCoerceU = unsafeCoerce

-- | Both 'unsafeStartTransaction' and 'unsafeEndTransaction' are "unsafe" in
-- the sense that they merely "coerce" the type but do not actually perform the
-- steps ('IO' actions) required to start or end a transaction (this is done by
-- 'revertRepositoryChanges' and 'finalizeRepositoryChanges'). Technically this
-- is not an actual coercion like with e.g. 'unsafeCoerceR', due to the
-- singleton typed member, but in practical terms it is no less unsafe, because
-- 'RO' vs. 'RW' changes whether @wR@ refers to the recorded or the tentative
-- state, respectively. In particular, you will get different results if you
-- are inside a transaction and read the patchset with a "coerced" Repository
-- of access type 'RO. The same holds for other state that is modified in a
-- transaction, like the pending patch or the rebase state.
unsafeStartTransaction :: Repository 'RO p wU wR -> Repository 'RW p wU wR
unsafeStartTransaction (Repo l f p c SRO) = Repo l f p c SRW

unsafeEndTransaction :: Repository 'RW p wU wR -> Repository 'RO p wU wR
unsafeEndTransaction (Repo l f p c SRW) = Repo l f p c SRO

mkRepo :: AbsoluteOrRemotePath -> RepoFormat -> PristineType -> Cache -> Repository 'RO p wU wR
mkRepo p f pr c = Repo (toPath p) f pr c SRO
