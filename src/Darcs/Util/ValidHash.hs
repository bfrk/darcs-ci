module Darcs.Util.ValidHash
    ( ValidHash(..)
    , InventoryHash
    , PatchHash
    , PristineHash
    , HashedDir(..)
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

import Darcs.Prelude

import Darcs.Util.Hash ( Hash, decodeHash, encodeHash, sha256strict )

data HashedDir
  = HashedPristineDir
  | HashedPatchesDir
  | HashedInventoriesDir
  deriving (Eq)

-- | External API for the various hash types.
class (Eq h, IsSizeHash h) => ValidHash h where
  encodeValidHash :: h -> String
  decodeValidHash :: String -> Maybe h
  -- | The 'HashedDir' belonging to this type of hash
  dirofValidHash :: h -> HashedDir
  -- | Compute hash from file content.
  calcValidHash :: B.ByteString -> h
  -- default definitions
  encodeValidHash = encodeSizeHash . getSizeHash
  decodeValidHash = fmap fromSizeHash . decodeSizeHash
  calcValidHash content = fromSizeAndHash (B.length content) (sha256strict content)

newtype InventoryHash = InventoryHash SizeHash
  deriving (Eq, Ord, Show, IsSizeHash)

instance ValidHash InventoryHash where
  dirofValidHash _ = HashedInventoriesDir

newtype PatchHash = PatchHash SizeHash
  deriving (Eq, Ord, Show, IsSizeHash)

instance ValidHash PatchHash where
  dirofValidHash _ = HashedPatchesDir

newtype PristineHash = PristineHash SizeHash
  deriving (Eq, Ord, Show, IsSizeHash)

instance ValidHash PristineHash where
  dirofValidHash _ = HashedPristineDir
  -- note: not the default definition here
  calcValidHash = fromHash . sha256strict

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

fromSizeAndHash :: ValidHash h => Int -> Hash -> h
fromSizeAndHash size hash =
  fromSizeHash $ if size < 1000000000 then WithSize size hash else NoSize hash

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
-- > size >=0 and size < 1_000_000_000
data SizeHash
  = WithSize !Int !Hash
  | NoSize !Hash
  deriving (Eq, Ord, Show)

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
  where padZero s = replicate (10 - length s) '0' ++ s

decodeSizeHash :: String -> Maybe SizeHash
decodeSizeHash s =
  case splitAt 10 s of
    (sizeStr, '-':hashStr)
      | Just size <- decodeSize sizeStr -> WithSize size <$> decodeHash hashStr
    _ -> NoSize <$> decodeHash s
  where
    decodeSize :: String -> Maybe Int
    decodeSize ss =
      case readMaybe ss of
        Just size | size >= 0 && size < 1000000000 -> Just size
        _ -> Nothing
