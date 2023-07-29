module Darcs.Util.ValidHash
    ( ValidHash(..)
    , InventoryHash
    , PatchHash
    , PristineHash
    , HashedDir(..)
    , encodeValidHash
    , decodeValidHash
    , parseValidHash
    , getHash
    , getSize
    , fromHash
    , fromSizeAndHash
    , checkHash
    , okayHash -- only used for garbage collection
    ) where

import qualified Data.ByteString as B
import Data.Maybe ( isJust )
import Text.Read ( readMaybe )

import Prelude ( (^) )
import Darcs.Prelude

import Darcs.Util.Hash ( Hash, decodeBase16, decodeHash, encodeHash, sha256strict )
import qualified Darcs.Util.Parser as P

-- | Semantically, this is the type of hashed objects. Git has a type tag
-- inside the hashed file itself, whereas in Darcs the type is determined
-- by the subdirectory.
data HashedDir
  = HashedPristineDir
  | HashedPatchesDir
  | HashedInventoriesDir
  deriving (Eq)

-- | External API for the various hash types.
class (Eq h, IsSizeHash h) => ValidHash h where
  -- | The 'HashedDir' belonging to this type of hash
  dirofValidHash :: h -> HashedDir
  -- | Compute hash from file content.
  calcValidHash :: B.ByteString -> h
  -- default definitions
  calcValidHash content = fromSizeAndHash (B.length content) (sha256strict content)

newtype InventoryHash = InventoryHash SizeHash
  deriving (Eq, Show, IsSizeHash)

instance ValidHash InventoryHash where
  dirofValidHash _ = HashedInventoriesDir

newtype PatchHash = PatchHash SizeHash
  deriving (Eq, Show, IsSizeHash)

instance ValidHash PatchHash where
  dirofValidHash _ = HashedPatchesDir

newtype PristineHash = PristineHash SizeHash
  deriving (Eq, Show, IsSizeHash)

instance ValidHash PristineHash where
  dirofValidHash _ = HashedPristineDir
  -- note: not the default definition here
  calcValidHash = fromHash . sha256strict

encodeValidHash :: ValidHash h => h -> String
encodeValidHash = encodeSizeHash . getSizeHash

decodeValidHash :: ValidHash h => String -> Maybe h
decodeValidHash = fmap fromSizeHash . decodeSizeHash

parseValidHash :: ValidHash h => P.Parser h
parseValidHash = fromSizeHash <$> parseSizeHash

getHash :: ValidHash h => h -> Hash
getHash sh =
  case getSizeHash sh of
    (NoSize h) -> h
    (WithSize _ h) -> h

getSize :: ValidHash h => h -> Maybe Int
getSize sh =
  case getSizeHash sh of
    (NoSize _) -> Nothing
    (WithSize s _) -> Just s

fromHash :: ValidHash h => Hash -> h
fromHash h = fromSizeHash (NoSize h)

numSizeDigits :: Int
numSizeDigits = 10

sizeLimit :: Int
sizeLimit = 10 ^ numSizeDigits

fromSizeAndHash :: ValidHash h => Int -> Hash -> h
fromSizeAndHash size hash =
  fromSizeHash $ if size < sizeLimit then WithSize size hash else NoSize hash

-- | Check that the given 'String' is an encoding of some 'ValidHash'.
okayHash :: String -> Bool
okayHash = isJust . decodeSizeHash

-- | Verify file content against a given 'ValidHash'.
checkHash :: ValidHash h => h -> B.ByteString -> Bool
checkHash vh content =
  -- It is tempting to simplify this to
  --   vh == calcValidHash content
  -- However, since we need to check old-style (sized) pristine hashes,
  -- this would require a non-standard Eq instance for SizeHash.
  case getSizeHash vh of
    NoSize h -> h == hash
    WithSize s h -> s == size && h == hash
  where
    hash = sha256strict content
    size = B.length content

-- * Internal definitions, not exported

-- | Combined size and hash, where the size is optional.
-- The invariant for a valid @'WithSize' size _@ is that
--
-- > size >=0 and size < 'sizeLimit'
data SizeHash
  = WithSize !Int !Hash
  | NoSize !Hash
  deriving (Eq, Show)

-- | Methods to wrap and unwrap 'ValidHash'es
class IsSizeHash h where
  getSizeHash :: h -> SizeHash
  fromSizeHash :: SizeHash -> h

-- This instance is only there so we can derive the instances above
instance IsSizeHash SizeHash where
  getSizeHash = id
  fromSizeHash = id

{-
-- This non-standard Eq instance would allow us to implement 'checkHash'
-- using equality with a freshly calculated hash.
instance Eq SizeHash where
  NoSize h1 == NoSize h2 = h1 == h2
  WithSize s1 h1 == WithSize s2 h2 = s1 == s2 && h1 == h2
  NoSize h1 == WithSize _ h2 = h1 == h2
  WithSize _ h1 == NoSize h2 = h1 == h2
-}

encodeSizeHash :: SizeHash -> String
encodeSizeHash (NoSize hash) = encodeHash hash
encodeSizeHash (WithSize size hash) =
    padZero (show size) ++ '-' : encodeHash hash
  where padZero s = replicate (numSizeDigits - length s) '0' ++ s

decodeSizeHash :: String -> Maybe SizeHash
decodeSizeHash s =
  case splitAt numSizeDigits s of
    (sizeStr, '-':hashStr)
      | Just size <- decodeSize sizeStr -> WithSize size <$> decodeHash hashStr
    _ -> NoSize <$> decodeHash s
  where
    decodeSize :: String -> Maybe Int
    decodeSize ss =
      case readMaybe ss of
        Just size | size >= 0 && size < sizeLimit -> Just size
        _ -> Nothing

parseSizeHash :: P.Parser SizeHash
parseSizeHash =
    (WithSize <$> pSize <*> pNoSize) P.<|> (NoSize <$> pNoSize)
  where
    pSize = do
      P.lookAhead (P.take numSizeDigits >> P.char '-')
      P.unsigned <* P.char '-'
    pNoSize = do
      x <- P.take 64
      maybe (fail "expecting b16-encoded sha256 hash") return (decodeBase16 x)
