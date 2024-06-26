--  Copyright (C) 2002-2003 David Roundy
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

{-# OPTIONS_GHC -Wno-orphans #-}

module Darcs.Patch
    ( PrimPatchBase(..)
    , Named
    , ApplyState
    , rmfile
    , addfile
    , rmdir
    , adddir
    , move
    , hunk
    , tokreplace
    , anonymous
    , binary
    , description
    , showPatchWithContext
    , ShowPatchFor(..)
    , showPatch
    , displayPatch
    , content
    , infopatch
    , changepref
    , thing
    , things
    , merge
    , commute
    , listTouchedFiles
    , hunkMatches
    , forceTokReplace
    , PrimPatch
      -- * for PatchTest
    , resolveConflicts
    , Effect
    , effect
    , invert
    , invertFL
    , invertRL
    , commuteFL
    , commuteRL
    , readPatch
    , readPatchPartial
    , canonizeFL
    , sortCoalesceFL
    , tryToShrink
    , patchname
    , patchcontents
    , apply
    , applyToTree
    , maybeApplyToTree
    , effectOnPaths
    , patch2patchinfo
    , summary
    , summaryFL
    , plainSummary
    , xmlSummary
    , plainSummaryPrims
    , adddeps
    , getdeps
    , listConflictedFiles
    , isInconsistent
    , module Darcs.Patch.RepoPatch
    , module Darcs.Patch.PatchInfoAnd
    ) where


import Darcs.Patch.Apply ( apply, effectOnPaths, applyToTree,
                           maybeApplyToTree, ApplyState )
import Darcs.Patch.Commute ( commute, commuteFL, commuteRL )
import Darcs.Patch.Conflict ( resolveConflicts )
import Darcs.Patch.Effect ( Effect(effect) )
import Darcs.Patch.Invert ( invert, invertRL, invertFL )
import Darcs.Patch.Inspect ( listTouchedFiles, hunkMatches )
import Darcs.Patch.Merge ( merge )
import Darcs.Patch.Named ( Named,
                           adddeps,
                           anonymous,
                           getdeps,
                           infopatch,
                           patch2patchinfo, patchname, patchcontents )
import Darcs.Patch.FromPrim ( PrimPatchBase(..) )
import Darcs.Patch.Prim ( canonizeFL,
                          sortCoalesceFL,
                          rmdir, rmfile, tokreplace, adddir, addfile,
                          binary, changepref, hunk, move,
                          tryToShrink,
                          PrimPatch )
import Darcs.Patch.Read ( readPatch, readPatchPartial )
import Darcs.Patch.Repair ( isInconsistent )
import Darcs.Patch.RepoPatch ( RepoPatch )
import Darcs.Patch.Show ( description, showPatch, content, displayPatch
                        , summary, summaryFL, thing, things, ShowPatchFor(..)
                        , showPatchWithContext )
import Darcs.Patch.Summary
    ( listConflictedFiles
    , xmlSummary
    , plainSummary
    , plainSummaryPrims
    )
import Darcs.Patch.TokenReplace ( forceTokReplace )
import Darcs.Patch.PatchInfoAnd
    ( PatchInfoAnd
    , hopefully
    , info
    )
