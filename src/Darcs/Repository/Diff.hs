-- Copyright (C) 2009 Petr Rockai
--
-- Permission is hereby granted, free of charge, to any person
-- obtaining a copy of this software and associated documentation
-- files (the "Software"), to deal in the Software without
-- restriction, including without limitation the rights to use, copy,
-- modify, merge, publish, distribute, sublicense, and/or sell copies
-- of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
-- NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
-- BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
-- ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
-- CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

-- |
-- Module      : Darcs.Repository.Diff
-- Copyright   : 2009 Petr Rockai
-- License     : MIT
-- Maintainer  : darcs-devel@darcs.net
-- Stability   : experimental
-- Portability : portable

{-# LANGUAGE PatternSynonyms #-}
module Darcs.Repository.Diff
    (
      treeDiff
    ) where

import Darcs.Prelude

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.These ( These(..) )

import Darcs.Patch
    ( PrimPatch
    , adddir
    , addfile
    , binary
    , canonizeFL
    , hunk
    , invert
    , rmdir
    , rmfile
    )
import Darcs.Patch.Witnesses.Ordered ( FL(..), concatGapsFL, consGapFL, (+>+) )
import Darcs.Patch.Witnesses.Sealed ( Gap(..) )
import Darcs.Repository.Flags ( DiffAlgorithm(..) )
import Darcs.Repository.Prefs ( FileType(..) )
import Darcs.Util.ByteString ( isFunky )
import Darcs.Util.Path ( AnchoredPath, anchorPath )
import Darcs.Util.Tree ( Tree, TreeItem(..), diffTrees', emptyBlob, readBlob )

type Diff m = These (TreeItem m) (TreeItem m)

pattern Removed :: TreeItem m -> Diff m
pattern Removed i = This i

pattern Added :: TreeItem m -> Diff m
pattern Added j = That j

pattern Changed :: TreeItem m -> TreeItem m -> Diff m
pattern Changed i j = These i j

treeDiff :: forall m w prim . (Monad m, Gap w, PrimPatch prim)
         => DiffAlgorithm
         -> (FilePath -> FileType)
         -> Tree m
         -> Tree m
         -> m (w (FL prim))
treeDiff da ft t1 t2 = concatGapsFL <$> diffTrees' diff t1 t2
  where
    diff :: AnchoredPath -> Diff m -> m (w (FL prim))
    diff _ (Changed (SubTree _) (SubTree _)) = return (emptyGap NilFL)
    diff p (Removed (SubTree _)) =
        -- Note: With files we first make the file empty before removing it.
        -- But for subtrees this has already been done in previous recursive calls.
        return $ freeGap (rmdir p :>: NilFL)
    diff p (Added (SubTree _)) =
        return $ freeGap (adddir p :>: NilFL)
    diff p (Added b'@(File _)) =
        do diff' <- diff p (Changed (File emptyBlob) b')
           return $ consGapFL (addfile p) diff'
    diff p (Removed a'@(File _)) =
        do diff' <- diff p (Changed a' (File emptyBlob))
           return $ joinGap (+>+) diff' (freeGap (rmfile p :>: NilFL))
    diff p (Changed (File a') (File b')) =
        do a <- readBlob a'
           b <- readBlob b'
           case ft (anchorPath "" p) of
             TextFile | no_bin a && no_bin b ->
                          return $ text_diff p a b
             _ -> return $ if a /= b
                              then freeGap (binary p (strict a) (strict b) :>: NilFL)
                              else emptyGap NilFL
    diff p (Changed a'@(File _) subtree@(SubTree _)) =
        do rmFileP <- diff p (Removed a')
           addDirP <- diff p (Added subtree)
           return $ joinGap (+>+) rmFileP addDirP
    diff p (Changed subtree@(SubTree _) b'@(File _)) =
        do rmDirP <- diff p (Removed subtree)
           addFileP <- diff p (Added b')
           return $ joinGap (+>+) rmDirP addFileP
    diff p _ = error $ "Missing case at path " ++ show (anchorPath "" p)

    text_diff p a b
        | BL.null a && BL.null b = emptyGap NilFL
        | BL.null a = freeGap (diff_from_empty p b)
        | BL.null b = freeGap (diff_to_empty p a)

        -- What is 'a line'? One view is that a line is something that is
        -- /terminated/ by either a newline or end of file. Another view is
        -- that lines are /separated/ by newline symbols.
        --
        -- The first view is the more "intuitive" one. The second is more
        -- "technical", it has the simpler definition and the highly desirable
        -- property that splitting a text into lines and joining them with
        -- newline symbols are inverse operations. The last point is the reason
        -- we never use the standard versions of 'unlines' for ByteString
        -- anywhere in darcs.
        --
        -- The two views differ mostly when enumerating the lines of a file
        -- that ends with a newline symbol: here, the technical view counts one
        -- more (empty) line. This leads to un-intuitive (though technically
        -- not incorrect) results when calculating the diff for a change that
        -- appends an empty line to a file that already has a newline at the
        -- end. For instance, for a file with a single, newline-terminated line
        -- of text, the LCS algorithm would tell us that a *third* (empty) line
        -- is being added.
        --
        -- To avoid this, we add a special case here: we strip off common
        -- newline symbols at the end. When we later split the result into
        -- lines for the diff algorithm, it never gets to see the empty
        -- last lines in both files and thus gives us the more intuitive result.

        | BLC.last a == '\n' && BLC.last b == '\n'
                    = freeGap (line_diff p (linesB $ BLC.init a) (linesB $ BLC.init b))
        | otherwise = freeGap (line_diff p (linesB a) (linesB b))

    line_diff p a b = canonizeFL da (hunk p 1 a b :>: NilFL)

    diff_to_empty p x | BLC.last x == '\n' = line_diff p (init $ linesB x) []
                      | otherwise = line_diff p (linesB x) [B.empty]

    diff_from_empty p x = invert (diff_to_empty p x)

    no_bin = not . isFunky . strict . BL.take 4096

    linesB = map strict . BLC.split '\n'

    strict = B.concat . BL.toChunks
