module Darcs.Patch.Prim.V1.Format (formatPrim) where

import Darcs.Prelude

import qualified Data.ByteString as B (ByteString, length, take, drop)

import Darcs.Patch.Permutations ()
import Darcs.Patch.Prim.V1.Core ( DirPatchType(..), FilePatchType(..), Prim(..) )

import Darcs.Util.Format
import Darcs.Util.Path ( AnchoredPath )


formatPrim :: (AnchoredPath -> Format) -> Prim wX wY -> Format
formatPrim fmt (FP f AddFile) = ascii "addfile" <+> fmt f
formatPrim fmt (FP f RmFile) = ascii "rmfile" <+> fmt f
formatPrim fmt (FP f (Hunk line old new)) = formatHunk fmt f line old new
formatPrim fmt (FP f (TokReplace t old new)) = formatTok fmt f t old new
formatPrim fmt (FP f (Binary old new)) = formatBinary fmt f old new
formatPrim fmt (DP d AddDir) = ascii "adddir" <+> fmt d
formatPrim fmt (DP d RmDir) = ascii "rmdir" <+> fmt d
formatPrim fmt (Move f f') = ascii "move" <+> fmt f <+> fmt f'
formatPrim _ (ChangePref p f t) =
  ascii "changepref" <+> ascii p <>
  -- Note that '$$' is not correct here, since read expects both values to
  -- be terminated by a newline (or EOF). For a similar situation see
  -- Darcs.Patch.Prim.FileUUID.Format.formatFileContent
  newline <> userchunk f <> newline <> userchunk t

formatHunk
  :: (AnchoredPath -> Format)
  -> AnchoredPath
  -> Int
  -> [B.ByteString]
  -> [B.ByteString]
  -> Format
formatHunk fmt f line old new =
  vcat
    [ ascii "hunk" <+> fmt f <+> intDec line
    , vcat (map (format_line "-") old)
    , vcat (map (format_line "+") new)
    ]
  where
    format_line pre bs = ascii pre <> byteString bs

formatTok
  :: (AnchoredPath -> Format)
  -> AnchoredPath
  -> String
  -> String
  -> String
  -> Format
formatTok fmt f t o n =
  ascii "replace"
    <+> fmt f
    <+> ascii "[" <> userchunk t <> ascii "]"
    <+> userchunk o
    <+> userchunk n

formatBinary
  :: (AnchoredPath -> Format)
  -> AnchoredPath
  -> B.ByteString
  -> B.ByteString
  -> Format
formatBinary fmt f old new =
  vcat
    [ ascii "binary" <+> fmt f
    , ascii "oldhex"
    , vcat (map format_chunk $ breakEvery 39 old)
    , ascii "newhex"
    , vcat (map format_chunk $ breakEvery 39 new)
    ]
  where
    format_chunk bs = ascii "*" <> byteStringHex bs
    breakEvery n ps
      | B.length ps < n = [ps]
      | otherwise = B.take n ps : breakEvery n (B.drop n ps)
