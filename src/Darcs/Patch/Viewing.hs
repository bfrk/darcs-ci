-- Copyright (C) 2002-2004 David Roundy
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
-- along with this program; see the file COPYING.  If not, write to
-- the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
-- Boston, MA 02110-1301, USA.

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module Darcs.Patch.Viewing
    ( showContextHunk
    ) where

import Darcs.Prelude

import qualified Data.ByteString as B ( null )

import Darcs.Patch.Apply ( Apply(..), ObjectIdOfPatch )
import Darcs.Patch.ApplyMonad ( ApplyMonad(..) )
import Darcs.Patch.FileHunk ( FileHunk(..), IsHunk(..), showContextFileHunk )
import Darcs.Patch.Format ( FileNameFormat(..), ListFormat(..), PatchListFormat(..) )
import Darcs.Patch.Object ( ObjectId(..), ObjectIdOf )
import Darcs.Patch.Show
    ( ShowContextPatch(..)
    , ShowPatch(..)
    , ShowPatchBasic(..)
    , ShowPatchFor(..)
    )
import Darcs.Patch.Witnesses.Ordered
    ( FL(..)
    , RL(..)
    , concatFL
    , mapFL
    , mapFL_FL
    , reverseRL
    )
import Darcs.Util.ByteString ( linesPS )
import Darcs.Util.Printer ( Doc, blueText, empty, vcat, ($$) )

showContextSeries
  :: forall p m wX wY
   . ( Apply p
     , ShowContextPatch p
     , IsHunk p
     , ApplyMonad (ApplyState p) m
     , ObjectId (ObjectIdOfPatch p)
     )
  => ShowPatchFor
  -> FileNameFormat
  -> FL p wX wY
  -> m Doc
showContextSeries use fmt = scs Nothing
  where
    scs :: Maybe (FileHunk (ObjectIdOfPatch p) wA wB) -> FL p wB wC -> m Doc
    scs pold (p :>: ps) = do
        case isHunk p of
            Nothing -> do
                a <- showPatchWithContextAndApply use p
                b <- scs Nothing ps
                return $ a $$ b
            Just fh -> case ps of
                NilFL -> do
                  r <- coolContextHunk fmt pold fh Nothing
                  apply p
                  return r
                (p2 :>: _) -> do
                    a <- coolContextHunk fmt pold fh (isHunk p2)
                    apply p
                    b <- scs (Just fh) ps
                    return $ a $$ b
    scs _ NilFL = return empty

showContextHunk
  :: (ApplyMonad state m, oid ~ ObjectIdOf state, ObjectId oid)
  => FileNameFormat
  -> FileHunk oid wX wY
  -> m Doc
showContextHunk fmt h = coolContextHunk fmt Nothing h Nothing

coolContextHunk :: (ApplyMonad state m, oid ~ ObjectIdOf state, ObjectId oid)
                => FileNameFormat
                -> Maybe (FileHunk oid wA wB) -> FileHunk oid wB wC
                -> Maybe (FileHunk oid wC wD) -> m Doc
coolContextHunk fmt prev fh@(FileHunk f l o n) next = do
    ls <- linesPS <$> readFilePS f
    let pre = take numpre $ drop (l - numpre - 1) ls
        -- This removes the last line if that is empty. This is because if a
        -- file ends with a newline, this would add an unintuitive "empty last
        -- line"; in other words, we regard the newline as a terminator, not a
        -- separator. See also the long comment in Darcs.Repository.Diff.
        cleanedls = case reverse ls of
            (x : xs)
                | B.null x -> reverse xs
            _ -> ls
        post = take numpost $ drop (max 0 $ l + length o - 1) cleanedls
    return $ showContextFileHunk fmt pre fh post
  where
    numpre = case prev of
        Just (FileHunk f' lprev _ nprev)
            | f' == f && l - (lprev + length nprev + 3) < 3 && lprev < l
            -> max 0 $ l - (lprev + length nprev + 3)
        _ -> if l >= 4 then 3 else l - 1
    numpost = case next of
        Just (FileHunk f' lnext _ _)
            | f' == f && lnext < l + length n + 4 && lnext > l
            -> lnext - (l + length n)
        _ -> 3

instance (PatchListFormat p, ShowPatchBasic p) => ShowPatchBasic (FL p) where
    showPatch ForDisplay = vcat . mapFL (showPatch ForDisplay)
    showPatch ForStorage = showPatchInternal patchListFormat
      where
        showPatchInternal :: ListFormat p -> FL p wX wY -> Doc
        showPatchInternal ListFormatV1 (p :>: NilFL) = (showPatch ForStorage) p
        showPatchInternal ListFormatV1 NilFL = blueText "{" $$ blueText "}"
        showPatchInternal ListFormatV1 ps = blueText "{"
                                            $$ vcat (mapFL (showPatch ForStorage) ps)
                                            $$ blueText "}"
        showPatchInternal ListFormatV2 ps = vcat (mapFL (showPatch ForStorage) ps)
        showPatchInternal ListFormatDefault ps = vcat (mapFL (showPatch ForStorage) ps)
        showPatchInternal ListFormatV3 ps = vcat (mapFL (showPatch ForStorage) ps)

instance ( Apply p
         , IsHunk p
         , PatchListFormat p
         , ShowContextPatch p
         , ObjectId (ObjectIdOfPatch p)
         ) =>
         ShowContextPatch (FL p) where
    showPatchWithContextAndApply ForDisplay = showContextSeries ForDisplay FileNameFormatDisplay
    showPatchWithContextAndApply ForStorage = showContextPatchInternal patchListFormat
      where
        showContextPatchInternal :: (ApplyMonad (ApplyState (FL p)) m)
                                 => ListFormat p -> FL p wX wY -> m Doc
        showContextPatchInternal ListFormatV1 (p :>: NilFL) =
            showPatchWithContextAndApply ForStorage p
        showContextPatchInternal ListFormatV1 NilFL =
            return $ blueText "{" $$ blueText "}"
        showContextPatchInternal ListFormatV1 ps = do
            x <- showContextSeries ForStorage FileNameFormatV1 ps
            return $ blueText "{" $$ x $$ blueText "}"
        showContextPatchInternal ListFormatV2 ps = showContextSeries ForStorage FileNameFormatV2 ps
        showContextPatchInternal ListFormatDefault ps = showContextSeries ForStorage FileNameFormatV2 ps
        showContextPatchInternal ListFormatV3 ps = return $ showPatch ForStorage ps

instance (PatchListFormat p, ShowPatch p) => ShowPatch (FL p) where
    content = vcat . mapFL content

    description = vcat . mapFL description

    summary = summaryFL

    summaryFL = summaryFL . concatFL

    thing x = thing (helperx x) ++ "s"
      where
        helperx :: FL a wX wY -> a wX wY
        helperx _ = undefined

    things = thing

instance (PatchListFormat p, ShowPatchBasic p) => ShowPatchBasic (RL p) where
    showPatch f = showPatch f . reverseRL

instance (ShowContextPatch p, Apply p, IsHunk p, PatchListFormat p, ObjectId (ObjectIdOfPatch p))
        => ShowContextPatch (RL p) where
    showPatchWithContextAndApply use = showPatchWithContextAndApply use . reverseRL

instance (PatchListFormat p, ShowPatch p) => ShowPatch (RL p) where
    content = content . reverseRL

    description = description . reverseRL

    summary = summary . reverseRL

    summaryFL = summaryFL . mapFL_FL reverseRL

    thing = thing . reverseRL

    things = things . reverseRL
