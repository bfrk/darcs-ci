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

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module Darcs.Patch.Viewing
    ( showContextHunk
    ) where

import Darcs.Prelude

import qualified Data.ByteString as B ( null )

import Darcs.Patch.Apply ( Apply(..), ObjectIdOfPatch )
import Darcs.Patch.ApplyMonad ( ApplyMonad(..) )
import Darcs.Patch.FileHunk ( FileHunk(..), IsHunk(..), showContextFileHunk )
import Darcs.Patch.Object ( ObjectId(..), ObjectIdOf )
import Darcs.Patch.Show
    ( ShowContextPatch(..)
    , ShowPatch(..)
    , ShowPatchBasic(..)
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
import Darcs.Util.Printer ( Print(..), Doc, empty, vcat, ($$) )

showContextSeries
  :: forall p m wX wY
   . ( Apply p
     , ShowContextPatch p
     , IsHunk p
     , ApplyMonad (ApplyState p) m
     , ObjectId (ObjectIdOfPatch p)
     )
  => FL p wX wY
  -> m Doc
showContextSeries = scs Nothing
  where
    scs :: Maybe (FileHunk (ExtraData p) (ObjectIdOfPatch p) wA wB) -> FL p wB wC -> m Doc
    scs pold (p :>: ps) = do
        case isHunk p of
            Nothing -> do
                a <- showPatchWithContextAndApply p
                b <- scs Nothing ps
                return $ a $$ b
            Just fh -> case ps of
                NilFL -> do
                  r <- coolContextHunk pold fh Nothing
                  apply p
                  return r
                (p2 :>: _) -> do
                    a <- coolContextHunk pold fh (isHunk p2)
                    apply p
                    b <- scs (Just fh) ps
                    return $ a $$ b
    scs _ NilFL = return empty

showContextHunk
  :: (ApplyMonad state m, oid ~ ObjectIdOf state, ObjectId oid, Print xd)
  => FileHunk xd oid wX wY
  -> m Doc
showContextHunk h = coolContextHunk Nothing h Nothing

coolContextHunk :: (ApplyMonad state m, oid ~ ObjectIdOf state, ObjectId oid, Print xd)
                => Maybe (FileHunk xd oid wA wB) -> FileHunk xd oid wB wC
                -> Maybe (FileHunk xd oid wC wD) -> m Doc
coolContextHunk prev fh@(FileHunk _ f l o n) next = do
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
    return $ showContextFileHunk pre fh post
  where
    numpre = case prev of
        Just (FileHunk _ f' lprev _ nprev)
            | f' == f && l - (lprev + length nprev + 3) < 3 && lprev < l
            -> max 0 $ l - (lprev + length nprev + 3)
        _ -> if l >= 4 then 3 else l - 1
    numpost = case next of
        Just (FileHunk _ f' lnext _ _)
            | f' == f && lnext < l + length n + 4 && lnext > l
            -> lnext - (l + length n)
        _ -> 3

instance ShowPatchBasic p => ShowPatchBasic (FL p) where
    showPatch = vcat . mapFL showPatch

instance ( Apply p
         , IsHunk p
         , ShowContextPatch p
         , ObjectId (ObjectIdOfPatch p)
         ) =>
         ShowContextPatch (FL p) where
    showPatchWithContextAndApply = showContextSeries

instance ShowPatch p => ShowPatch (FL p) where
    content = vcat . mapFL content

    description = vcat . mapFL description

    summary = summaryFL

    summaryFL = summaryFL . concatFL

    thing x = thing (helperx x) ++ "s"
      where
        helperx :: FL a wX wY -> a wX wY
        helperx _ = undefined

    things = thing

instance ShowPatchBasic p => ShowPatchBasic (RL p) where
    showPatch = showPatch . reverseRL

instance (ShowContextPatch p, Apply p, IsHunk p, ObjectId (ObjectIdOfPatch p))
        => ShowContextPatch (RL p) where
    showPatchWithContextAndApply = showPatchWithContextAndApply . reverseRL

instance ShowPatch p => ShowPatch (RL p) where
    content = content . reverseRL

    description = description . reverseRL

    summary = summary . reverseRL

    summaryFL = summaryFL . mapFL_FL reverseRL

    thing = thing . reverseRL

    things = things . reverseRL
