module Darcs.Util.Format
  ( Format
  -- * Primitive constructors
  , ascii
  , stringUtf8
  , byteString
  , shortByteString
  , byteStringHex
  , userchunk
  , intDec
  , word64Dec
  , listWord8Hex
  , newline
  -- * Combinators
  , ($$)
  , (<+>)
  , hsep
  , vcat
  , vsep
  , protect
  -- * Output
  , toLazyByteString
  , toStrictByteString -- temporary, should eventually go away
  , toDoc -- for email rendering
  , hPutFormat
  , putFormat
  ) where

import Darcs.Prelude

import qualified Data.ByteString as B ( ByteString, null )
import qualified Data.ByteString.Builder as B
import Data.ByteString.Builder.Prim ( primMapListFixed, word8HexFixed )
import qualified Data.ByteString.Lazy as BL ( ByteString, toStrict )
import qualified Data.ByteString.Short as BS ( ShortByteString, null )
import Data.Word ( Word8, Word64 )
import System.IO ( Handle, stdout )

import Darcs.Util.ByteString ( encodeLocale )
import Darcs.Util.Printer ( Doc, packedString )

-- | Simple wrapper type for 'B.Builder'. It is needed only because there is no
-- way to inspect a 'B.Builder' to check if it is empty; which is needed for
-- '$$' and '<+>' to have a unit.
data Format = Empty | Nonempty B.Builder

instance Semigroup Format where
  Empty <> x = x
  x <> Empty = x
  Nonempty x <> Nonempty y = Nonempty (x <> y)

instance Monoid Format where
  mempty = Empty

infixr 6 <+>
(<+>) :: Format -> Format -> Format
Empty <+> y = y
x <+> Empty = x
Nonempty x <+> Nonempty y = Nonempty (x <> B.char7 ' ' <> y)

infixr 5 $$
($$) :: Format -> Format -> Format
Empty $$ y = y
x $$ Empty = x
x $$ y = x <> newline <> y

infixr 5 $+$
($+$) :: Format -> Format -> Format
Empty $+$ y = y
x $+$ Empty = x
x $+$ y = x <> newline <> newline <> y

newline :: Format
newline = ascii "\n"

vcat :: [Format] -> Format
vcat = foldr ($$) mempty

hsep :: [Format] -> Format
hsep = foldr (<+>) mempty

vsep :: [Format] -> Format
vsep = foldr ($+$) mempty

-- | Declare a 'Format' nonempty, regardless of its content. This 'protect's it
-- from being eliminated by combinators (like '$$' or 'mconcat'), thus the name.
protect :: Format -> Format
protect Empty = Nonempty mempty
protect f = f

nonempty :: (t -> Bool) -> (t -> B.Builder) -> t -> Format
nonempty test build arg
  | test arg = Empty
  | otherwise = Nonempty (build arg)

ascii :: String -> Format
ascii = nonempty null B.string7

stringUtf8 :: String -> Format
stringUtf8 = nonempty null B.stringUtf8

byteString :: B.ByteString -> Format
byteString = nonempty B.null B.byteString

shortByteString :: BS.ShortByteString -> Format
shortByteString = nonempty BS.null B.shortByteString

byteStringHex :: B.ByteString -> Format
byteStringHex = nonempty B.null B.byteStringHex

intDec :: Int -> Format
intDec = Nonempty . B.intDec

word64Dec :: Word64 -> Format
word64Dec = Nonempty . B.word64Dec

listWord8Hex :: [Word8] -> Format
listWord8Hex = nonempty null (primMapListFixed word8HexFixed)

-- | User-provided (potentially) non-ascii 'String's are rendered
-- using 'encodeLocale'. Should ideally go away eventually.
userchunk :: String -> Format
userchunk = byteString . encodeLocale

unFormat :: Format -> B.Builder
unFormat Empty = mempty
unFormat (Nonempty x) = x

toLazyByteString :: Format -> BL.ByteString
toLazyByteString = B.toLazyByteString . unFormat

toStrictByteString :: Format -> B.ByteString
toStrictByteString = BL.toStrict . toLazyByteString

toDoc :: Format -> Doc
toDoc = packedString . toStrictByteString

hPutFormat :: Handle -> Format -> IO ()
hPutFormat h = B.hPutBuilder h . unFormat

putFormat :: Format -> IO ()
putFormat = hPutFormat stdout
