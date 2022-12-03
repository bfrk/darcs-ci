module Darcs.Patch.FileHunk
    ( FileHunk(..), IsHunk(..), showFileHunk, showContextFileHunk
    )
    where

import Darcs.Prelude

import Darcs.Patch.Apply ( ObjectIdOfPatch )
import Darcs.Patch.Format ( FileNameFormat )
import Darcs.Patch.Invert ( Invert(..) )
import Darcs.Patch.Object ( ObjectId(..) )

import Darcs.Util.Printer
    ( Print(..), Doc, blueText, text, lineColor, vcat, userchunkPS
    , prefix, ($$), (<+>), Color(Cyan, Magenta) )

import qualified Data.ByteString as B ( ByteString )


data FileHunk xd oid wX wY = FileHunk xd oid !Int [B.ByteString] [B.ByteString]

type role FileHunk nominal nominal nominal nominal

class Print (ExtraData p) => IsHunk p where
  type ExtraData p
  isHunk :: p wX wY -> Maybe (FileHunk (ExtraData p) (ObjectIdOfPatch p) wX wY)
  fromHunk :: FileHunk (ExtraData p) (ObjectIdOfPatch p) wX wY -> p wX wY

showFileHunkHeader
  :: (ObjectId oid, Print xd) => FileNameFormat -> xd -> oid -> Int -> Doc
showFileHunkHeader fmt xd oid line =
  print xd $$
  blueText "hunk" <+> formatObjectId fmt oid <+> text (show line)

showFileHunkBody :: [B.ByteString] -> [B.ByteString] -> Doc
showFileHunkBody old new =
  lineColor Magenta (prefix "-" (vcat $ map userchunkPS old)) $$
  lineColor Cyan (prefix "+" (vcat $ map userchunkPS new))

showFileHunk
  :: (ObjectId oid, Print xd) => FileNameFormat -> FileHunk xd oid wX wY -> Doc
showFileHunk fmt (FileHunk xd oid line old new) =
  showFileHunkHeader fmt xd oid line $$
  showFileHunkBody old new

showContextFileHunk
  :: (ObjectId oid, Print xd)
  => FileNameFormat
  -> [B.ByteString]
  -> FileHunk xd oid wB wC
  -> [B.ByteString]
  -> Doc
showContextFileHunk fmt pre (FileHunk xd oid line old new) post =
  showFileHunkHeader fmt xd oid line $$
  prefix " " (vcat $ map userchunkPS pre) $$
  showFileHunkBody old new $$
  prefix " " (vcat $ map userchunkPS post)

-- NOTE This instance is for low-level prim patch manipulation, such as done in
-- Darcs.Patch.Prim.V1.Mangle or Darcs.Patch.Split, which is why we retain the
-- ExtraData as is and don't require a capability to invert it.
instance Invert (FileHunk xd oid) where
  invert (FileHunk xd path line old new) = FileHunk xd path line new old
