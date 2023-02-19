--  Copyright (C) 2002-2005 David Roundy
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

module Darcs.Patch.Show
     ( ShowPatchBasic(..)
     , displayPatch
     , ShowPatchFor(..)
     , ShowPatch(..)
     , ShowContextPatch(..)
     , formatFileName
     ) where

import Darcs.Prelude

import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch.ApplyMonad ( ApplyMonad )
import Darcs.Patch.Object ( formatFileName )
import Darcs.Patch.Witnesses.Ordered ( FL, mapFL )

import Darcs.Util.English ( plural, Noun(Noun) )
import Darcs.Util.Printer ( Doc, vcat )

data ShowPatchFor = ForDisplay | ForStorage

displayPatch :: ShowPatchBasic p => p wX wY -> Doc
displayPatch p = showPatch ForDisplay p

class ShowPatchBasic p where
    showPatch :: ShowPatchFor -> p wX wY -> Doc

class ShowPatchBasic p => ShowContextPatch p where
    -- | Show a patch with context lines added, as diff -u does. Thus, it
    -- differs from showPatch only for hunks. It is used for instance before
    -- putting it into a bundle. As this unified context is not included in
    -- patch representation, this requires access to the 'ApplyState'.
    --
    -- Note that this applies the patch in the 'ApplyMonad' given by the
    -- context. This is done in order to simplify showing multiple patches in a
    -- series, since each patch may change the context lines for later changes.
    showContextPatch :: (ApplyMonad (ApplyState p) m)
                     => ShowPatchFor -> p wX wY -> m Doc

-- | This class is used only for user interaction, not for storage. The default
-- implementations for 'description' and 'content' are suitable only for
-- 'PrimPatch' and 'RepoPatch' types. Logically, 'description' should default
-- to 'mempty' while 'content' should default to 'displayPatch'. We define them
-- the other way around so that 'Darcs.UI.PrintPatch.showFriendly' gives
-- reasonable results for all patch types.
class ShowPatchBasic p => ShowPatch p where
    content :: p wX wY -> Doc
    content = mempty

    description :: p wX wY -> Doc
    description = displayPatch

    summary :: p wX wY -> Doc

    summaryFL :: FL p wX wY -> Doc
    summaryFL = vcat . mapFL summary

    thing :: p wX wY -> String
    thing _ = "patch"

    things :: p wX wY -> String
    things x = plural (Noun $ thing x) ""
