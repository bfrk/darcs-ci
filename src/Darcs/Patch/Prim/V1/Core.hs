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

{-# LANGUAGE OverloadedStrings #-}
module Darcs.Patch.Prim.V1.Core
    ( Prim(..)
    , TreePatchType(..)
    , FilePatchType(..)
    , With(..)
    ) where

import Darcs.Prelude
import qualified Data.ByteString.Char8 as BC

import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch.Commute ( Commute(..), invertCommute, trivialCommute )
import Darcs.Patch.Debug ( PatchDebug(..) )
import Darcs.Patch.FileHunk ( FileHunk(..), IsHunk(..) )
import Darcs.Patch.Inspect ( PatchInspect(..) )
import Darcs.Patch.Invert ( Invert(..) )
import Darcs.Patch.Merge ( CleanMerge(..) )
import Darcs.Patch.Object ( ObjectIdOf )
import Darcs.Patch.Permutations ()
import Darcs.Patch.Prim.Class
    ( PrimConstruct(..)
    , PrimDetails(..)
    , PrimRead(..)
    , PrimShow(..)
    , PrimSift(..)
    , primCleanMerge
    )
import Darcs.Patch.Prim.V1.FilePatch ( FilePatchType(..), With(..) )
import Darcs.Patch.Prim.V1.TreePatch ( TreePatchType(..), orIfEq )
import Darcs.Patch.SummaryData ( SummDetail(..) )
import Darcs.Patch.Witnesses.Eq ( Eq2(..) )
import Darcs.Patch.Witnesses.Ordered ( (:>)(..) )
import Darcs.Patch.Witnesses.Sealed ( mapSeal, seal )
import Darcs.Patch.Witnesses.Show ( Show1, Show2 )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP )
import Darcs.Util.Parser
    ( anyChar
    , choice
    , lexWord
    , skipSpace
    , skipWhile
    , string
    , takeTillChar
    )
import Darcs.Util.Path ( AnchoredPath, movedirfilename )
import Darcs.Util.Printer ( blueText, text, userchunk, ($$), (<+>) )


data Prim wX wY where
    TP :: !(TreePatchType wX wY) -> Prim wX wY
    FP :: !AnchoredPath -> !(FilePatchType wX wY) -> Prim wX wY
    CP :: !String -> !String -> !String -> Prim wX wY
    deriving (Eq)

-- | The reason the 'Ord' instances are manually defined is that we have to
-- keep the ordering exactly as it was, because, unfortunately, RepoPatchV2
-- internally uses sortCoalesceFL when commuting conflictors.
instance Ord (Prim wX wY) where
    compare (TP p) (TP q) = compare p q
    compare (TP (RmFile f)) (FP g _) = compare f g `orIfEq` LT
    compare (TP (AddFile f)) (FP g _) = compare f g `orIfEq` LT
    compare (FP f _) (TP (RmFile g)) = compare f g `orIfEq` GT
    compare (FP f _) (TP (AddFile g)) = compare f g `orIfEq` GT
    compare (TP _) _ = LT
    compare _ (TP _) = GT
    compare (FP f p) (FP g q) = compare f g `orIfEq` compare p q
    compare (FP _ _) _ = LT
    compare _ (FP _ _) = GT
    compare (CP a b c) (CP a' b' c') = compare (a,b,c) (a',b',c')

instance Show2 Prim
instance Show1 (Prim wX)
deriving instance Show (Prim wX wY)

instance PrimShow Prim where
    showPrim fmt (TP tp) = showPrim fmt tp
    showPrim fmt (FP f p) = showPrim fmt (With f p)
    showPrim _ (CP p f t) =
      blueText "changepref" <+> text p $$ userchunk f $$ userchunk t

instance PrimRead Prim where
    readPrim fmt =
      skipSpace >>
      choice
        [ mapSeal fromFP <$> readPrim fmt
        , mapSeal TP <$> readPrim fmt
        , seal <$> readChangePref
        ]
      where
        readChangePref = do
          string "changepref"
          p <- lexWord
          skipWhile (== ' ')
          _ <- anyChar -- skip newline
          f <- takeTillChar '\n'
          _ <- anyChar -- skip newline
          t <- takeTillChar '\n'
          return $ CP (BC.unpack p) (BC.unpack f) (BC.unpack t)
        fromFP (With f p) = FP f p

instance ObjectIdOf (ApplyState Prim) ~ AnchoredPath => PrimConstruct Prim where
    addfile p = TP (AddFile p)
    rmfile p = TP (RmFile p)
    adddir p = TP (AddDir p)
    rmdir p = TP (RmDir p)
    move old new = TP (Move old new)
    changepref p f t = CP p f t
    hunk f line old new = FP f (Hunk line old new)
    tokreplace f tokchars old new =
      FP f (TokReplace tokchars (BC.pack old) (BC.pack new))
    binary f old new = FP f $ Binary old new

instance ObjectIdOf (ApplyState Prim) ~ AnchoredPath => IsHunk Prim where
    type ExtraData Prim = ()
    isHunk (FP f (Hunk line before after)) = Just (FileHunk () f line before after)
    isHunk _ = Nothing
    fromHunk (FileHunk () f line before after) = FP f (Hunk line before after)

instance Invert Prim where
    invert (FP f fp)  = FP f (invert fp)
    invert (TP tp) = TP (invert tp)
    invert (CP p f t) = CP p t f

instance PatchInspect Prim where
    -- Recurse on everything, these are potentially spoofed patches
    listTouchedFiles (FP f _) = [f]
    listTouchedFiles (TP tp) = listTouchedFiles tp
    listTouchedFiles (CP _ _ _) = []

    hunkMatches f (FP _ (Hunk _ remove add)) = anyMatches remove || anyMatches add
        where anyMatches = foldr ((||) . f) False
    hunkMatches _ (FP _ _) = False
    hunkMatches f (TP tp) = hunkMatches f tp
    hunkMatches _ (CP _ _ _) = False

instance PrimDetails Prim where
    summarizePrim (TP p) = summarizePrim p
    summarizePrim (FP f p) = summarizePrim (With f p)
    summarizePrim (CP{}) = [SummNone]

instance PatchDebug Prim

instance Eq2 Prim where
    unsafeCompare p q = p == unsafeCoerceP q

instance PrimSift Prim where
    primIsSiftable (FP _ (Binary _ _)) = True
    primIsSiftable (FP _ (Hunk _ _ _)) = True
    primIsSiftable _ = False

instance Commute Prim where
  commute pair =
    case pair of
      FP f1 p1 :> FP f2 p2
        | f1 == f2 -> do
          p2' :> p1' <- commute (p1 :> p2)
          return (FP f2 p2' :> FP f1 p1')
        | otherwise -> do
          p2' :> p1' <- trivialCommute (p1 :> p2)
          return (FP f2 p2' :> FP f1 p1')
      TP tp1 :> TP tp2 -> do
        tp2' :> tp1' <- commute (tp1 :> tp2)
        return (TP tp2' :> TP tp1')
      FP f fp :> TP tp -> do
        tp' :> With f' fp' <- commuteFPTP (With f fp :> tp)
        return (TP tp' :> FP f' fp')
      TP tp :> FP f fp -> do
        With f' fp' :> tp' <- invertCommute commuteFPTP (tp :> With f fp)
        return (FP f' fp' :> TP tp')
      -- the wildcard matches here mean we only have to consider TP and FP above
      _ :> CP {} -> trivialCommute pair
      CP {} :> _ -> trivialCommute pair
    where
      commuteFPTP (With f fp :> p2@(Move o n)) =
        return
          (unsafeCoerceP p2 :> With (movedirfilename o n f) (unsafeCoerceP fp))
      commuteFPTP fptp@(With f1 _ :> RmFile f2)
        | f1 == f2 = Nothing
        | otherwise = trivialCommute fptp
      -- the rest (AddFile, RmDir, AddDir) is trivial
      commuteFPTP fptp@(_ :> _) = trivialCommute fptp

instance CleanMerge Prim where
  cleanMerge = primCleanMerge
