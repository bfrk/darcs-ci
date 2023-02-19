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
    ( Doc, blueText, text, lineColor, vcat, userchunkPS
    , prefix, ($$), (<+>), Color(Cyan, Magenta) )

import qualified Data.ByteString as B ( ByteString )


data FileHunk oid wX wY = FileHunk oid !Int [B.ByteString] [B.ByteString]

type role FileHunk nominal nominal nominal

class IsHunk p where
    isHunk :: p wX wY -> Maybe (FileHunk (ObjectIdOfPatch p) wX wY)

showFileHunk :: ObjectId oid => FileNameFormat -> FileHunk oid wX wY -> Doc
showFileHunk x (FileHunk f line old new) =
           blueText "hunk" <+> formatObjectId x f <+> text (show line)
        $$ lineColor Magenta (prefix "-" (vcat $ map userchunkPS old))
        $$ lineColor Cyan    (prefix "+" (vcat $ map userchunkPS new))

showContextFileHunk
  :: ObjectId oid
  => FileNameFormat
  -> [B.ByteString]
  -> FileHunk oid wB wC
  -> [B.ByteString]
  -> Doc
showContextFileHunk fmt pre (FileHunk f l o n) post =
  blueText "hunk" <+> formatObjectId fmt f <+> text (show l) $$
  prefix " " (vcat $ map userchunkPS pre) $$
  lineColor Magenta (prefix "-" (vcat $ map userchunkPS o)) $$
  lineColor Cyan (prefix "+" (vcat $ map userchunkPS n)) $$
  prefix " " (vcat $ map userchunkPS post)

instance Invert (FileHunk oid) where
    invert (FileHunk path line old new) = FileHunk path line new old
