{-# OPTIONS_GHC -Wno-orphans #-}
module Darcs.Patch.Prim.FileUUID.Format ( formatUUID ) where

import Darcs.Prelude

import qualified Data.ByteString as B

import Darcs.Patch.Format ( FormatPatch(..) )
import Darcs.Patch.Prim.FileUUID.Core
    ( FileContent
    , Hunk(..)
    , Location(..)
    , Prim(..)
    , UUID(..)
    )
import Darcs.Patch.Prim.FileUUID.ObjectMap ( unFileID )
import Darcs.Util.Format
    ( Format
    , byteString
    , intDec
    , ascii
    , newline
    , vcat
    , word64Dec
    , (<+>)
    )
import Darcs.Util.Path ( Name, encodeWhiteName )

instance FormatPatch Prim where
  formatPatch (Hunk u h) = formatHunk u h
  formatPatch (Manifest f (L d p)) = formatManifest "manifest" d f p
  formatPatch (Demanifest f (L d p)) = formatManifest "demanifest" d f p
  formatPatch Identity = ascii "identity"

formatManifest :: String -> UUID -> UUID -> Name -> Format
formatManifest txt dir file name =
  ascii txt <+>
  formatUUID file <+>
  formatUUID dir <+>
  byteString (encodeWhiteName name)

formatHunk :: UUID -> Hunk wX wY -> Format
formatHunk uid (H off old new) =
  vcat
    [ ascii "hunk" <+> formatUUID uid <+> intDec off
    , formatFileContent old
    , formatFileContent new
    ]

formatFileContent :: FileContent -> Format
formatFileContent c =
  -- NOTE readPatch wants a '\n' after the length and then starts reading
  -- content bytes; so clearly using '$$' here would be wrong; but then why
  -- does it work in the Show module? Apparently, Darcs.Util.Printer is not
  -- quite as consistent as Darcs.Util.Format when it comes to mempty being
  -- a unit for $$.
  ascii "content" <+> intDec (B.length c) <> newline <> byteString c

formatUUID :: UUID -> Format
formatUUID Root = ascii "root"
formatUUID (Recorded x) = ascii "r" <+> byteString x
formatUUID (Unrecorded x) = ascii "u" <+> word64Dec (unFileID x)
