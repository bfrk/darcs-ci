module Darcs.Patch.Object where

import Darcs.Prelude

import qualified Data.ByteString.Char8 as BC ( unpack )

import Darcs.Patch.Format ( FileNameFormat(..) )
import Darcs.Util.ByteString ( packStringToUTF8, encodeLocale )
import Darcs.Util.Path ( AnchoredPath, encodeWhite, anchorPath )
import Darcs.Util.Printer ( Doc, text, packedString )
import Darcs.Util.Tree ( Tree )

-- | Given a state type (parameterized over a monad m :: * -> *), this gives us
-- the type of the key with which we can lookup an item (or object) in the
-- state.
type family ObjectIdOf (state :: (* -> *) -> *)

-- | We require from such a key (an 'ObjectId') that it has a canonical way
-- to format itself to a 'Doc'. For historical reasons, this takes a parameter
-- of type 'FileNameFormat'.
class Eq oid => ObjectId oid where
  formatObjectId :: FileNameFormat -> oid -> Doc

type instance ObjectIdOf Tree = AnchoredPath

-- formatFileName is defined here only to avoid an import cycle

-- | Format a 'AnchoredPath' to a 'Doc' according to the given 'FileNameFormat'.
--
-- NOTE: This is not only used for display but also to format patch files. This is
--       why we have to do the white space encoding here.
--       See 'Darcs.Repository.Hashed.writePatchIfNecessary'.
--
-- Besides white space encoding, for 'FileNameFormatV2' we just pack it into a 'Doc'. For
-- 'FileNameFormatV1' we must emulate the non-standard darcs-1 encoding of file paths: it
-- is an UTF8 encoding of the raw byte stream, interpreted as code points.
--
-- See also 'Darcs.Patch.Show.readFileName'.
formatFileName :: FileNameFormat -> AnchoredPath -> Doc
formatFileName FileNameFormatV1 =
  packedString . packStringToUTF8 . BC.unpack . encodeLocale . encodeWhite . ap2fp
formatFileName FileNameFormatV2 = text . encodeWhite . ap2fp
formatFileName FileNameFormatDisplay = text . ap2fp

instance ObjectId AnchoredPath where
  formatObjectId = formatFileName

ap2fp :: AnchoredPath -> FilePath
ap2fp ap = "./" ++ anchorPath "" ap
