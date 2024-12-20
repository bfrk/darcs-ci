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
    , AccessType(..)
    , SAccessType(..)
    , repoAccessType
    , repoCache
    , modifyCache
    , repoFormat
    , modifyRepoFormat
    , repoLocation
    , unsafeCoerceRepoType
    , unsafeCoercePatchType
    , unsafeCoerceR
    , unsafeCoerceU
    , unsafeEndTransaction
    , unsafeStartTransaction
    , mkRepo
    ) where

import Darcs.Prelude

import Darcs.Util.Cache ( Cache, WritableOrNot(..), setThisRepo )
import Darcs.Repository.Format ( RepoFormat, unsafeWriteRepoFormat )
import Darcs.Repository.Paths ( formatPath )
import Darcs.Util.Path ( AbsoluteOrRemotePath, toPath )
import Unsafe.Coerce ( unsafeCoerce )

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
--
-- Note that none of the accessors are exported.
data Repository (rt :: AccessType) (p :: Type -> Type -> Type) wU wR = Repo
  { location :: !String
  , format :: !RepoFormat
  , cache :: Cache
  , access :: (SAccessType rt)
  }

type role Repository nominal nominal nominal nominal

repoLocation :: Repository rt p wU wR -> String
repoLocation = location

repoFormat :: Repository rt p wU wR -> RepoFormat
repoFormat = format

repoCache :: Repository rt p wU wR -> Cache
repoCache = cache

modifyCache :: (Cache -> Cache) -> Repository rt p wU wR -> Repository rt p wU wR
modifyCache g r@(Repo {cache = c}) = r { cache = g c }

repoAccessType :: Repository rt p wU wR -> SAccessType rt
repoAccessType = access

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
unsafeStartTransaction Repo {access = SRO, ..} =
  Repo {access = SRW, cache = setThisRepo location Writable cache, ..}

unsafeEndTransaction :: Repository 'RW p wU wR -> Repository 'RO p wU wR
unsafeEndTransaction Repo {access = SRW, ..} =
  Repo {access = SRO, cache = setThisRepo location NotWritable cache, ..}

mkRepo :: AbsoluteOrRemotePath -> RepoFormat -> Cache -> Repository 'RO p wU wR
mkRepo p f c = Repo {location = toPath p, format = f, cache = c, access = SRO}

modifyRepoFormat
  :: (RepoFormat -> RepoFormat)
  -> Repository 'RW p wU wR
  -> IO (Repository 'RW p wU wR)
modifyRepoFormat f r@(Repo {format = fmt}) = do
  let fmt' = f fmt
  unsafeWriteRepoFormat fmt' formatPath
  return r {format = fmt'}
