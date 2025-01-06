{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings, UndecidableInstances #-}
module Darcs.Patch.Prim.FileUUID.Show
    ( displayHunk, showUUID )
    where

import Darcs.Prelude

import qualified Data.ByteString as B

import Darcs.Patch.Apply ( Apply(..), ObjectIdOfPatch )
import Darcs.Patch.Object ( ObjectId(..) )
import Darcs.Patch.Show
    ( ShowPatchBasic(..), ShowPatch(..)
    , ShowContextPatch(..) )
import Darcs.Patch.Summary ( plainSummaryPrim, plainSummaryPrims )
import Darcs.Patch.Prim.Class ( showPrimWithContextAndApply )
import Darcs.Patch.Prim.FileUUID.Core
    ( Prim(..), Hunk(..), UUID(..), Location(..), FileContent )
import Darcs.Patch.Prim.FileUUID.Details ()
import Darcs.Patch.Prim.FileUUID.ObjectMap ()
import Darcs.Util.ByteString ( linesPS )
import Darcs.Util.Path ( Name, encodeWhiteName )
import Darcs.Util.Printer
    ( text, packedString, blueText, prefix
    , (<+>), ($$), Doc, vcat
    )

instance ShowPatchBasic Prim where
  showPatch (Hunk u h) = displayHunk (Just u) h
  showPatch (Manifest f (L d p)) = showManifest "manifest" d f p
  showPatch (Demanifest f (L d p)) = showManifest "demanifest" d f p
  showPatch Identity = blueText "identity"

instance (Apply Prim, ObjectId UUID, ObjectIdOfPatch Prim ~ UUID) => ShowContextPatch Prim where
  showPatchWithContextAndApply = showPrimWithContextAndApply

instance ShowPatch Prim where
  summary = plainSummaryPrim
  summaryFL = plainSummaryPrims False
  thing _ = "change"

showManifest :: String -> UUID -> UUID -> Name -> Doc
showManifest txt dir file name =
  blueText txt <+>
  showUUID file <+>
  showUUID dir <+>
  packedString (encodeWhiteName name)

displayHunk :: Maybe UUID -> Hunk wX wY -> Doc
displayHunk uid (H off old new) =
  blueText "hunk" <+>
  maybe (text "<nil>") showUUID uid <+>
  text (show off) $$
  displayFileContent "-" old $$
  displayFileContent "+" new

-- TODO add some heuristics to recognize binary content
displayFileContent :: String -> FileContent -> Doc
displayFileContent pre = vcat . map (prefix pre) . showLines . linesPS
  where
    context = blueText "[...]"
    showLines [] = []
    showLines [x]
      | B.null x = []
      | otherwise = [context <> packedString x <> context]
    showLines (x:xs) =
      [context <> packedString x] ++
      map packedString (init xs) ++
      [packedString (last xs) <> context]

showUUID :: UUID -> Doc
showUUID Root = "root"
showUUID (Recorded x) = "r" <+> packedString x
showUUID (Unrecorded x) = "u" <+> text (show x)
