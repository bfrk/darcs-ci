--  Copyright (C) 2002-2003,2007 David Roundy
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

module Darcs.Patch.Prim.V1.Core
    ( Prim(..)
    , DirPatchType(..)
    , FilePatchType(..)
    ) where

import Darcs.Prelude

import qualified Data.ByteString as B (ByteString)

import Darcs.Util.Path ( AnchoredPath )
import Darcs.Patch.Witnesses.Eq ( Eq2(..) )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP )
import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch.Debug ( PatchDebug(..) )
import Darcs.Patch.FileHunk ( FileHunk(..), IsHunk(..) )
import Darcs.Patch.Invert ( Invert(..) )
import Darcs.Patch.Inspect ( PatchInspect(..) )
import Darcs.Patch.Object ( ObjectIdOf )
import Darcs.Patch.Permutations () -- for Invert instance of FL
import Darcs.Patch.Prim.Class ( PrimConstruct(..), PrimSift(..) )

data Prim wX wY where
    Move :: !AnchoredPath -> !AnchoredPath -> Prim wX wY
    DP :: !AnchoredPath -> !(DirPatchType wX wY) -> Prim wX wY
    FP :: !AnchoredPath -> !(FilePatchType wX wY) -> Prim wX wY
    ChangePref :: !String -> !String -> !String -> Prim wX wY

data FilePatchType wX wY
    = RmFile
    | AddFile
    | Hunk !Int [B.ByteString] [B.ByteString]
    | TokReplace !String !String !String
    | Binary B.ByteString B.ByteString
    deriving (Eq,Ord)

type role FilePatchType nominal nominal

data DirPatchType wX wY = RmDir | AddDir
                           deriving (Eq,Ord)

type role DirPatchType nominal nominal

instance Eq2 FilePatchType where
    unsafeCompare a b = a == unsafeCoerceP b

instance Invert FilePatchType where
    invert RmFile = AddFile
    invert AddFile = RmFile
    invert (Hunk line old new) = Hunk line new old
    invert (TokReplace t o n) = TokReplace t n o
    invert (Binary o n) = Binary n o

instance Eq2 DirPatchType where
    unsafeCompare a b = a == unsafeCoerceP b

instance Invert DirPatchType where
    invert RmDir = AddDir
    invert AddDir = RmDir

instance ObjectIdOf (ApplyState Prim) ~ AnchoredPath => PrimConstruct Prim where
    addfile f = FP f AddFile
    rmfile f = FP f RmFile
    adddir d = DP d AddDir
    rmdir d = DP d RmDir
    move old new = Move old new
    changepref p f t = ChangePref p f t
    hunk f line old new = FP f (Hunk line old new)
    tokreplace f tokchars old new = FP f (TokReplace tokchars old new)
    binary f old new = FP f $ Binary old new
    primFromHunk (FileHunk f line before after) = FP f (Hunk line before after)

instance ObjectIdOf (ApplyState Prim) ~ AnchoredPath => IsHunk Prim where
    isHunk (FP f (Hunk line before after)) = Just (FileHunk f line before after)
    isHunk _ = Nothing

instance Invert Prim where
    invert (FP f p)  = FP f (invert p)
    invert (DP d p) = DP d (invert p)
    invert (Move f f') = Move f' f
    invert (ChangePref p f t) = ChangePref p t f

instance PatchInspect Prim where
    -- Recurse on everything, these are potentially spoofed patches
    listTouchedFiles (Move f1 f2) = [f1, f2]
    listTouchedFiles (FP f _) = [f]
    listTouchedFiles (DP d _) = [d]
    listTouchedFiles (ChangePref _ _ _) = []

    hunkMatches f (FP _ (Hunk _ remove add)) = anyMatches remove || anyMatches add
        where anyMatches = foldr ((||) . f) False
    hunkMatches _ (FP _ _) = False
    hunkMatches _ (DP _ _) = False
    hunkMatches _ (ChangePref _ _ _) = False
    hunkMatches _ (Move _ _) = False

instance PatchDebug Prim

instance Eq2 Prim where
    unsafeCompare (Move a b) (Move c d) = a == c && b == d
    unsafeCompare (DP d1 p1) (DP d2 p2)
        = d1 == d2 && p1 `unsafeCompare` p2
    unsafeCompare (FP f1 fp1) (FP f2 fp2)
        = f1 == f2 && fp1 `unsafeCompare` fp2
    unsafeCompare (ChangePref a1 b1 c1) (ChangePref a2 b2 c2)
        = c1 == c2 && b1 == b2 && a1 == a2
    unsafeCompare _ _ = False

instance Eq (Prim wX wY) where
    (==) = unsafeCompare

instance PrimSift Prim where
  primIsSiftable (FP _ (Binary _ _)) = True
  primIsSiftable (FP _ (Hunk _ _ _)) = True
  primIsSiftable _ = False
