--  Copyright (C) 2002-2005,2007 David Roundy
--  Copyright (C) 2009 Reinier Lamers
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2, or (at your option)
--  any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; see the file COPYING.  If not, write to
--  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
--  Boston, MA 02110-1301, USA.

-- | This module contains unit tests of the code in 'Darcs.Email'
--
-- These tests check whether the emails generated by darcs meet a few criteria.
-- We check for line length and non-ASCII characters. We apparently do not have
-- to check for CR-LF newlines because that's handled by sendmail.

module Darcs.Test.Email ( testSuite ) where

import Darcs.Prelude

import Data.Char ( isPrint )
import qualified Data.ByteString as B ( length, unpack, null, head,
                                        cons, empty, foldr, ByteString )
import qualified Data.ByteString.Char8 as BC ( unlines )
import Test.Framework ( Test, testGroup )
import Test.Framework.Providers.QuickCheck2 ( testProperty )
import Test.QuickCheck.Instances.ByteString ()

import Darcs.Util.Printer ( text, renderPS, packedString )
import Darcs.UI.Email ( makeEmail, readEmail, formatHeader, prop_qp_roundtrip )
import Safe ( tailErr )

testSuite :: Test
testSuite = testGroup "Darcs.Email"
  [ emailParsing
  , emailHeaderNoLongLines
  , emailHeaderAsciiChars
  , emailHeaderLinesStart
  , emailHeaderNoEmptyLines
  , emailCodecRoundtrip
  ]

-- | Checks that darcs can read the emails it generates
emailParsing :: Test
emailParsing = testProperty "Checking that email can be parsed" $ \bs ->
    BC.unlines (B.empty:bs++[B.empty,B.empty]) ==
              readEmail (renderPS
                    $ makeEmail "reponame" [] (Just (text "contents\n"))
                                 Nothing
                                 (packedString $ BC.unlines bs) (Just "filename"))

-- | Check that formatHeader never creates lines longer than 78 characters
-- (excluding the carriage return and line feed)
emailHeaderNoLongLines :: Test
emailHeaderNoLongLines =
    testProperty "Checking email header line length" $ \field value ->
      let cleanField = cleanFieldString field
      in not $ any (>78) $ map B.length $ bsLines $ formatHeader cleanField value

-- Check that an email header does not contain non-ASCII characters
-- formatHeader doesn't escape field names, there is no such thing as non-ascii
-- field names afaik
emailHeaderAsciiChars :: Test
emailHeaderAsciiChars =
    testProperty "Checking email for illegal characters" $ \field value ->
      let cleanField = cleanFieldString field
      in not (any (>127) (B.unpack (formatHeader cleanField value)))

-- Check that header the second and later lines of a header start with a space
emailHeaderLinesStart :: Test
emailHeaderLinesStart =
    testProperty "Checking for spaces at start of folded email header lines" $ \field value ->
      let headerLines = bsLines (formatHeader cleanField value)
          cleanField  = cleanFieldString field
      in all (\l -> B.null l || B.head l == 32) (tailErr headerLines)

-- Checks that there are no lines in email headers with only whitespace
emailHeaderNoEmptyLines :: Test
emailHeaderNoEmptyLines =
    testProperty "Checking that there are no empty lines in email headers" $ \field value ->
      let headerLines = bsLines (formatHeader cleanField value)
          cleanField  = cleanFieldString field
          in all (not . B.null) headerLines --(not . B.null . B.filter (not . (`elem` [10, 32, 9]))) headerLines

emailCodecRoundtrip :: Test
emailCodecRoundtrip =
    testProperty "Checking that quoted printable en- then decoding is id" $ prop_qp_roundtrip

bsLines :: B.ByteString -> [B.ByteString]
bsLines = finalizeFold . B.foldr splitAtLines (B.empty, [])
  where splitAtLines 10 (thisLine, prevLines) = (B.empty, thisLine:prevLines)
        splitAtLines c  (thisLine, prevLines) = (B.cons c thisLine, prevLines)
        finalizeFold (lastLine, otherLines) = lastLine : otherLines

cleanFieldString :: String -> String
cleanFieldString = filter (\c -> isPrint c && c < '\x80' && c /= ':')

