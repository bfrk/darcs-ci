--  Copyright (C) 2004-2005 David Roundy
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2, or (at your option)
--  any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; see the file COPYING.  If not, write to
--  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
--  Boston, MA 02110-1301, USA.

module Darcs.Repository.Match
    ( getPristineUpToMatch
    ) where

import Darcs.Prelude

import Darcs.Patch ( RepoPatch )
import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch.Match ( PatchSetMatch, rollbackToPatchSetMatch )

import Darcs.Repository.Hashed ( readPatches )
import Darcs.Repository.InternalTypes ( Repository )
import Darcs.Repository.Pristine ( readPristine )

import Darcs.Util.Tree ( Tree )
import Darcs.Util.Tree.Monad ( virtualTreeIO )

-- | Return the pristine tree up to the given 'PatchSetMatch'.
-- In the typical case where the match is closer to the end of the repo than
-- its beginning, this is (a lot) more efficient than applying the result of
-- 'getOnePatchset' to an empty tree.
getPristineUpToMatch :: (RepoPatch p, ApplyState p ~ Tree)
                     => Repository rt p wU wR
                     -> PatchSetMatch
                     -> IO (Tree IO)
getPristineUpToMatch r psm = do
  ps <- readPatches r
  tree <- readPristine r
  snd <$> virtualTreeIO (rollbackToPatchSetMatch psm ps) tree
