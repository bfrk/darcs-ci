module Darcs.Test.Misc.Encoding ( testSuite ) where

import Darcs.Prelude

import qualified Data.ByteString as B
import Control.Monad
import Data.Word
import System.IO.Unsafe

import Darcs.Util.Encoding

import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.QuickCheck ( testProperty )
import Test.QuickCheck

decodeThenEncode :: B.ByteString -> B.ByteString
decodeThenEncode = unsafePerformIO . (decode >=> encode)

testSuite :: TestTree
testSuite = testGroup "Darcs.Util.Encoding"
 [ testProperty "decode then encode roundtrips" propDecodeThenEncodeRoundTrip
 ]

-- could use the bytestring-arbitrary package,
-- but the shrinking isn't as effective as 'shrinkList shrink'
newtype MyByteString = MBS { _mbsBytes :: [Word8] }
 deriving Show

instance Arbitrary MyByteString where
  arbitrary =
    MBS <$> frequency
      -- make sure we test some very long ByteStrings
      [ (1, sized (\n -> vectorOf (100*n) arbitrary))
      , (9, sized (\n -> vectorOf n arbitrary))
      ]
  shrink (MBS ws) = MBS <$> shrinkList shrink ws

toBS :: MyByteString -> B.ByteString
toBS (MBS ws) = B.pack ws

propDecodeThenEncodeRoundTrip :: MyByteString -> Bool
propDecodeThenEncodeRoundTrip mbs =
  let bstr = toBS mbs in decodeThenEncode bstr == bstr
