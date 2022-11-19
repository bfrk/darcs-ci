-- Copyright (C) 2002-2003 David Roundy
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

module Darcs.Patch.Info
    ( PatchInfo(..) -- constructor and fields exported *only for tests*
    , rawPatchInfo  -- exported *only for tests*
    , patchinfo
    , replaceJunk
    , makePatchname
    , readPatchInfo
    , justName
    , justAuthor
    , justLog
    , displayPatchInfo
    , toXml
    , toXmlShort
    , piDate
    , piDateString
    , piName
    , piRename
    , piAuthor
    , piTag
    , piLog
    , showPatchInfo
    , isTag
    , escapeXML
    , validDate
    , validLog
    , validAuthor
    , validDatePS
    , validLogPS
    , validAuthorPS
    ) where

import Darcs.Prelude

import Control.Monad ( unless, void, when )
import Crypto.Random ( seedNew, seedToInteger )
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Short as BS
import Data.Char ( isAscii )
import Data.List ( isPrefixOf, lookup )
import Data.String ( fromString )
import Numeric ( showHex )
import System.IO.Unsafe ( unsafePerformIO )
import System.Time
    ( CalendarTime
    , calendarTimeToString
    , toCalendarTime
    , toClockTime
    )

import Darcs.Patch.Show ( ShowPatchFor(..) )
import Darcs.Test.TestOnly ( TestOnly )
import Darcs.Util.ByteString
    ( decodeLocale
    , packStringToUTF8
    , unlinesBS
    , unpackPSFromUTF8
    )
import Darcs.Util.Hash ( SHA1, sha1PS )
import Darcs.Util.IsoDate ( readUTCDate )
import Darcs.Util.Parser as RM
    ( Parser
    , anyChar
    , char
    , linesStartingWithEndingWith
    , option
    , skipSpace
    , takeTill
    , takeTillChar
    )
import qualified Darcs.Util.Parser as RM ( take )
import Darcs.Util.Printer
    ( Doc
    , blueText
    , cyanText
    , empty
    , packedString
    , prefix
    , text
    , vcat
    , ($$)
    , (<+>)
    )
import Darcs.Util.Prompt ( promptYorn )

{- |
A PatchInfo value contains the metadata of a patch. The date, name, author
and log fields are UTF-8 encoded text in darcs 2.4 and later, and just
sequences of bytes (decoded with whatever is the locale when displayed) in
earlier darcs.

The members with names that start with '_' are not supposed to be used
directly in code that does not care how the patch info is stored.

@_piLegacyIsInverted@:

Historically, the @isInverted@ flag was used to indicate that a Named patch
was inverted.

We no longer support direct inversion of 'Darcs.Patch.Named.Named' patches,
except sometimes via the 'Darcs.Patch.Invertible.Invertible' wrapper which
tracks inversion in the wrapper.

However, going even further back in time, inverted patches could be written
out by @darcs rollback@. This was changed in 2008 so any patches on disk
with this flag set would have been written by a darcs from prior to then.
As they still exist, including in the darcs repository itself, we need
to support them.

As far as current darcs is concerned, the flag should be treated like any
other field in 'PatchInfo' apart from never being set freshly:

 - There is no semantic relationship between a 'PatchInfo' with
   @piLegacyIsInverted = False@ and the same 'PatchInfo' with
   @piLegacyIsInverted = True@. For example they are not inverses of each
   other.

- New or amended patches should never be written out with
  @_piLegacyIsInverted = True@.

 - We do need to maintain backwards compatibility so we take care to
   preserve things like the hash, on-disk format etc.

- A patch with @_piLegacyIsInverted = True@ should work with all the
  normal darcs operations.

The flag is completely separate and orthogonal to the tracking of
explicit inversion in the 'Darcs.Patch.Invertible.Invertible' wrapper.
The 'Darcs.Patch.Invertible.Invertible' wrapper
is only used in memory and never stored to disk so there should be no
confusion when reading a patch from disk. Within the codebase they
serve completely different purposes and should not interact at all.
-}
data PatchInfo =
  PatchInfo { _piDate    :: !BS.ShortByteString
            , _piName    :: !BS.ShortByteString
            , _piAuthor  :: !BS.ShortByteString
            , _piLog     :: ![BS.ShortByteString]
              -- | See the long description of this field in the
              -- docs above.
            , _piLegacyIsInverted :: !Bool
            }
  deriving (Eq,Ord,Show)

-- Validation

-- We need these functions to ensure that we can parse the
-- result of showPatchInfo.

validDate :: String -> Bool
validDate = all validCharForDate

validDatePS :: BS.ShortByteString -> Bool
validDatePS = BC.all validCharForDate . BS.fromShort

-- | The isAscii limitation is due to the use of BC.pack below.
validCharForDate :: Char -> Bool
validCharForDate c = isAscii c && c /= '\n' && c /= ']'

validLog :: String -> Bool
validLog = notElem '\n'

validLogPS :: BS.ShortByteString -> Bool
validLogPS = BC.notElem '\n' . BS.fromShort

validAuthor :: String -> Bool
validAuthor = notElem '*'

validAuthorPS :: BS.ShortByteString -> Bool
validAuthorPS = BC.notElem '*' . BS.fromShort

rawPatchInfo
  :: TestOnly
  => String -> String -> String -> [String] -> Bool -> PatchInfo
rawPatchInfo = rawPatchInfoInternal

rawPatchInfoInternal :: String -> String -> String -> [String] -> Bool -> PatchInfo
rawPatchInfoInternal date name author log inverted =
    PatchInfo { _piDate     = fromString $ validateDate date
              , _piName     = BS.toShort $ packStringToUTF8 $ validateName name
              , _piAuthor   = BS.toShort $ packStringToUTF8 $ validateAuthor author
              , _piLog      = map (BS.toShort . packStringToUTF8 . validateLog) log
              , _piLegacyIsInverted  = inverted
              }
  where
    validateAuthor = validate validAuthor "author"
    validateName = validate validLog "patch name"
    validateLog = validate validLog "log line"
    validateDate = validate validDate "date"
    validate test meta x =
      if test x then x else error (unwords ["invalid",meta,show x])

-- | @patchinfo date name author log@ constructs a new 'PatchInfo' value
-- with the given details, automatically assigning an Ignore-this header
-- to guarantee the patch is unique.  The function does not verify
-- the date string's sanity.
patchinfo :: String -> String -> String -> [String] -> IO PatchInfo
patchinfo date name author log =
    addJunk $ rawPatchInfoInternal date name author log False

-- | addJunk adds a line that contains a random number to make the patch
--   unique.
addJunk :: PatchInfo -> IO PatchInfo
addJunk pinf =
    do x <- seedToInteger <$> seedNew
       -- Note: this is now 40 bytes long compare to the 32 we had before
       when (_piLog pinf /= ignoreJunk (_piLog pinf)) $
            do putStrLn $ "Lines beginning with 'Ignore-this: ' " ++
                          "will not be shown when displaying a patch."
               confirmed <- promptYorn "Proceed? "
               unless confirmed $ fail "User cancelled because of Ignore-this."
       return $ pinf { _piLog = head ignored <> (fromString $ showHex x "") : _piLog pinf }

replaceJunk :: PatchInfo -> IO PatchInfo
replaceJunk pi@(PatchInfo {_piLog=log}) = addJunk $ pi{_piLog = ignoreJunk log}

-- this is a list so we can change the junk header
ignored :: [BS.ShortByteString]
ignored = map fromString ["Ignore-this: "]

ignoreJunk :: [BS.ShortByteString] -> [BS.ShortByteString]
ignoreJunk = filter isnt_ignored
    where isnt_ignored x = doesnt_start_with x ignored
          doesnt_start_with x ys = not $ any (`BS.isPrefixOf` x) ys


-- * Patch info formatting

-- | Get the name, including an "UNDO: " prefix if the patch is
-- a legacy inverted patch.
justName :: PatchInfo -> String
justName pinf =
  if _piLegacyIsInverted pinf
    then "UNDO: " ++ nameString
    else nameString
  where nameString = metadataToString (_piName pinf)

-- | Returns the author of a patch.
justAuthor :: PatchInfo -> String
justAuthor =  metadataToString . _piAuthor

justLog :: PatchInfo -> String
justLog = unlines . map (BC.unpack . BS.fromShort) . _piLog

displayPatchInfo :: PatchInfo -> Doc
displayPatchInfo pi =
    cyanText "patch " <> cyanText (show $ makePatchname pi)
 $$ text "Author: " <> text (piAuthor pi)
 $$ text "Date:   " <> text (friendlyD $ _piDate pi)
 $$ hfn (piName pi)
 $$ vcat (map ((text "  " <>) . text) (piLog pi))
  where hfn x = case piTag pi of
                Nothing -> inverted <+> text x
                Just t -> text "  tagged" <+> text t
        inverted = if _piLegacyIsInverted pi then text "  UNDO:" else text "  *"

-- | Returns the name of the patch. Unlike 'justName', it does not preprend
--   "UNDO: " to the name if the patch has the legacy inverted flag set.
piName :: PatchInfo -> String
piName = metadataToString . _piName

piRename :: PatchInfo -> String -> PatchInfo
piRename x n = x { _piName = BS.toShort $ packStringToUTF8 n }

-- | Returns the author of a patch.
piAuthor :: PatchInfo -> String
piAuthor = metadataToString . _piAuthor

isTag :: PatchInfo -> Bool
isTag pinfo = "TAG " `isPrefixOf` justName pinfo

-- | Read the date from raw patch (meta) data and convert it to UTC.
-- The raw data may contain timezone info. This is for compatibiltity
-- with patches that were created before 2003-11, when darcs still
-- created patches that contained localized date strings.
readPatchDate :: BS.ShortByteString -> CalendarTime
readPatchDate = readUTCDate . BC.unpack . BS.fromShort

piDate :: PatchInfo -> CalendarTime
piDate = readPatchDate . _piDate

piDateString :: PatchInfo -> String
piDateString = BC.unpack . BS.fromShort . _piDate

-- | Get the log message of a patch.
piLog :: PatchInfo -> [String]
piLog = map metadataToString . ignoreJunk . _piLog

-- | Get the tag name, if the patch is a tag patch.
piTag :: PatchInfo -> Maybe String
piTag pinf =
    if l == t
      then Just $ metadataToString r
      else Nothing
    where (l, r) = BS.splitAt (BS.length t) (_piName pinf)
          t = fromString "TAG "

-- | Convert a metadata ByteString to a string. It first tries to convert
--   using UTF-8, and if that fails, tries the locale encoding.
--   We try UTF-8 first because UTF-8 is clearly recognizable, widely used,
--   and people may have UTF-8 patches even when UTF-8 is not their locale.
metadataToString :: BS.ShortByteString -> String
metadataToString bs | '\xfffd' `notElem` bsUtf8 = bsUtf8
                    | otherwise                 = decodeLocale (BS.fromShort bs)
  where bsUtf8 = unpackPSFromUTF8 (BS.fromShort bs)

friendlyD :: BS.ShortByteString -> String
friendlyD d = unsafePerformIO $ do
    ct <- toCalendarTime $ toClockTime $ readPatchDate d
    return $ calendarTimeToString ct

toXml :: PatchInfo -> Doc -> Doc
toXml = toXml' True

toXmlShort :: PatchInfo -> Doc -> Doc
toXmlShort = toXml' False

toXml' :: Bool -> PatchInfo -> Doc -> Doc
toXml' includeComments pi summary =
        text "<patch"
    <+> text "author='" <> escapeXMLByteString (_piAuthor pi) <> text "'"
    <+> text "date='" <> escapeXMLByteString (_piDate pi) <> text "'"
    <+> text "local_date='" <> escapeXML (friendlyD $ _piDate pi) <> text "'"
    <+> text "inverted='" <> text (show $ _piLegacyIsInverted pi) <> text "'"
    <+> text "hash='" <> text (show $ makePatchname pi) <> text "'>"
    $$  indent abstract
    $$  indent summary
    $$  text "</patch>"
      where
        indent = prefix "    "
        name = text "<name>" <> escapeXMLByteString (_piName pi) <> text "</name>"
        abstract | includeComments = name $$ commentsAsXml (_piLog pi)
                 | otherwise = name

commentsAsXml :: [BS.ShortByteString] -> Doc
commentsAsXml comments
  | BS.length comments' > 0 = text "<comment>"
                          <> escapeXMLByteString comments'
                          <> text "</comment>"
  | otherwise = empty
    where comments' = unlinesBS comments

-- escapeXML is duplicated in Patch.lhs and Annotate.lhs
-- It should probably be refactored to exist in one place.
escapeXML :: String -> Doc
escapeXML = text . strReplace '\'' "&apos;" . strReplace '"' "&quot;" .
  strReplace '>' "&gt;" . strReplace '<' "&lt;" . strReplace '&' "&amp;"

-- Escape XML characters in a UTF-8 encoded ByteString, and turn it into a Doc.
-- The data will be in the Doc as a bytestring.
escapeXMLByteString :: BS.ShortByteString -> Doc
escapeXMLByteString =
  packedString .
  bstrReplace
    [ ('\'', "&apos;")
    , ('"', "&quot;")
    , ('>', "&gt;")
    , ('<', "&lt;")
    , ('&', "&amp;")
    ] .
  BS.fromShort

strReplace :: Char -> String -> String -> String
strReplace _ _ [] = []
strReplace x y (z:zs)
  | x == z    = y ++ strReplace x y zs
  | otherwise = z : strReplace x y zs

bstrReplace :: [(Char,String)] -> BC.ByteString -> BC.ByteString
bstrReplace tbl = BC.concat . go where
  go bs
    | BC.null bs = []
    | otherwise =
        case lookup (BC.head bs) tbl of
          Just s -> BC.pack s : go (BC.tail bs)
          Nothing -> BC.singleton (BC.head bs) : go (BC.tail bs)

-- | Hash on patch metadata (patch name, author, date, log, and the legacy
-- \"inverted\" flag.
-- Robust against context changes but does not guarantee patch contents.
-- Usually used as matcher or patch identifier (see Darcs.Patch.Match).
makePatchname :: PatchInfo -> SHA1
makePatchname pi = sha1PS sha1_me
        where b2ps True = BC.pack "t"
              b2ps False = BC.pack "f"
              sha1_me = BC.concat [BS.fromShort $ _piName pi,
                                  BS.fromShort $ _piAuthor pi,
                                  BS.fromShort $ _piDate pi,
                                  BC.concat $ map BS.fromShort $ _piLog pi,
                                  b2ps $ _piLegacyIsInverted pi]


showPatchInfo :: ShowPatchFor -> PatchInfo -> Doc
showPatchInfo ForDisplay = displayPatchInfo
showPatchInfo ForStorage = storePatchInfo

-- |Patch is stored between square brackets.
--
-- > [ <patch name>
-- > <patch author>*<patch date>
-- >  <patch log (may be empty)> (indented one)
-- >  <can have multiple lines in patch log,>
-- >  <as long as they're preceded by a space>
-- >  <and don't end with a square bracket.>
-- > ]
--
-- note that below I assume the name has no newline in it.
-- See 'readPatchInfo' for the inverse operation.
-- There are more assumptions, see validation functions above.
storePatchInfo :: PatchInfo -> Doc
storePatchInfo pi =
    blueText "[" <> bsToDoc (_piName pi)
 $$ bsToDoc (_piAuthor pi) <> text inverted <> bsToDoc (_piDate pi)
                                 <> myunlines (_piLog pi) <> blueText "] "
    where inverted = if _piLegacyIsInverted pi then "*-" else "**"
          myunlines [] = empty
          myunlines xs =
              foldr (\s -> ((text "\n " <> bsToDoc s) <>)) (text "\n") xs
          bsToDoc = packedString . BS.fromShort

-- |Parser for 'PatchInfo' as stored in patch bundles and inventory files,
-- for example:
--
-- > [Document the foo interface
-- > John Doe <john.doe@example.com>**20110615084241
-- >  Ignore-this: 85b94f67d377c4ab671101266ef9c229
-- >  Nobody knows what a 'foo' is, so describe it.
-- > ]
--
-- See 'showPatchInfo' for the inverse operation.
readPatchInfo :: Parser PatchInfo
readPatchInfo = do
  skipSpace
  char '['
  name <- takeTillChar '\n'
  _ <- anyChar
  author <- takeTillChar '*'
  s2 <- RM.take 2
  ct <- takeTill (\c->c==']'||c=='\n')
  option () (void (char '\n')) -- consume newline char, if present
  log <- linesStartingWithEndingWith ' ' ']'
  return PatchInfo { _piDate = BS.toShort ct
                   , _piName = BS.toShort name
                   , _piAuthor = BS.toShort author
                   , _piLog = map BS.toShort log
                   , _piLegacyIsInverted = BC.index s2 1 /= '*'
                   }
