{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Darcs.Repository.Inventory.Format
    ( Inventory(..)
    , HeadInventory
    , InventoryEntry
    , ValidHash(..) -- re-export
    , decodeValidHash -- re-export
    , encodeValidHash -- re-export
    , InventoryHash
    , PatchHash
    , PristineHash
    , inventoryPatchNames
    , parseInventory
    , parseInventoryParent
    , parseHeadInventory -- not used
    , formatInventory
    , formatInventoryPatches
    , formatInventoryEntry
    , emptyInventory
    , pokePristineHash
    , peekPristineHash
    , skipPristineHash
    -- properties
    , prop_inventoryParseFormat
    , prop_peekPokePristineHash
    , prop_skipPokePristineHash
    ) where

import Darcs.Prelude

import Control.Applicative ( optional, many )

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL

import Darcs.Patch.Info ( PatchInfo, readPatchInfo, formatPatchInfo )
import Darcs.Test.TestOnly
import Darcs.Util.Parser
    ( Parser, char, parse, string, skipSpace )
import Darcs.Util.Format
    ( Format
    , byteString
    , newline
    , toStrictByteString
    , ($$)
    , (<+>)
    )
import Darcs.Util.ValidHash
    ( InventoryHash
    , PatchHash
    , PristineHash
    , ValidHash(..)
    , calcValidHash
    , decodeValidHash
    , encodeValidHash
    , formatValidHash
    , parseValidHash
    )

-- * Inventories

-- This type and the parser combinators for it aren't actually used. They are
-- here to serve as documentation for the API we would like to use but won't
-- because of efficiency: we want to be able to access the pristine hash
-- without forcing a complete parse of the head inventory. Thus we retain the
-- lower-level peek/poke/skip API for the pristine hash.
type HeadInventory = (PristineHash, Inventory)

data Inventory = Inventory
  { inventoryParent :: Maybe InventoryHash
  , inventoryPatches :: [InventoryEntry]
  } deriving (Eq, Show)

-- The 'String' is the (hashed) patch filename.
type InventoryEntry = (PatchInfo, PatchHash)

inventoryPatchNames :: Inventory -> [String]
inventoryPatchNames = map (encodeValidHash . snd) . inventoryPatches

emptyInventory :: Inventory
emptyInventory = Inventory Nothing []

-- * Parsing

parseHeadInventory :: B.ByteString -> Either String HeadInventory
parseHeadInventory = fmap fst . parse pHeadInv

parseInventory :: B.ByteString -> Either String Inventory
parseInventory = fmap fst . parse pInv

-- | Parse only the (optional) parent inventory hash, ignore the patches.
parseInventoryParent :: B.ByteString -> Either String (Maybe InventoryHash)
parseInventoryParent = fmap fst . parse pInvParent

pHeadInv :: Parser HeadInventory
pHeadInv = (,) <$> pPristineHash <*> pInv

pPristineHash :: Parser PristineHash
pPristineHash = do
  string kwPristine
  skipSpace
  pHash

pInv :: Parser Inventory
pInv = Inventory <$> pInvParent <*> pInvPatches

pInvParent :: Parser (Maybe InventoryHash)
pInvParent = optional $ do
  string kwParent
  skipSpace
  pHash

pHash :: ValidHash h => Parser h
pHash = parseValidHash <* char '\n'

pInvPatches :: Parser [InventoryEntry]
pInvPatches = many pInvEntry

pInvEntry :: Parser InventoryEntry
pInvEntry = do
  info <- readPatchInfo
  skipSpace
  string kwHash
  skipSpace
  hash <- pHash
  return (info, hash)

-- * Formatting

formatInventory :: Inventory -> Format
formatInventory inv =
  formatParent (inventoryParent inv) <>
  formatInventoryPatches (inventoryPatches inv)

formatInventoryPatches :: [InventoryEntry] -> Format
formatInventoryPatches = mconcat . map formatInventoryEntry

formatInventoryEntry :: InventoryEntry -> Format
formatInventoryEntry (pinf, hash) =
  formatPatchInfo pinf $$
  byteString kwHash <+> formatValidHash hash <> newline

formatParent :: Maybe InventoryHash -> Format
formatParent (Just hash) =
  byteString kwParent $$ formatValidHash hash <> newline
formatParent Nothing = mempty

-- * Accessing the pristine hash

-- | Replace the pristine hash at the start of a raw, unparsed 'HeadInventory'
-- or add it if none is present.
pokePristineHash :: PristineHash -> B.ByteString -> Format
pokePristineHash hash inv =
  byteString kwPristine <> formatValidHash hash <> newline
    <> byteString (skipPristineHash inv)

takeHash :: B.ByteString -> Maybe (PristineHash, B.ByteString)
takeHash input = do
  let (hline,rest) = BC.breakSubstring (BC.pack "\n") input
  ph <- decodeValidHash (BC.unpack hline)
  return (ph, rest)

peekPristineHash :: B.ByteString -> PristineHash
peekPristineHash inv =
  case tryDropPristineName inv of
    Just rest ->
      case takeHash rest of
        Just (h, _) -> h
        Nothing -> error $ "Bad hash in inventory!"
    Nothing -> calcValidHash BL.empty

-- |skipPristineHash drops the 'pristine: HASH' prefix line, if present.
skipPristineHash :: B.ByteString -> B.ByteString
skipPristineHash ps =
  case tryDropPristineName ps of
    Just rest -> B.drop 1 $ BC.dropWhile (/= '\n') rest
    Nothing -> ps

tryDropPristineName :: B.ByteString -> Maybe B.ByteString
tryDropPristineName input =
    if prefix == kwPristine then Just rest else Nothing
  where
    (prefix, rest) = B.splitAt (B.length kwPristine) input

-- * Key phrases

kwPristine :: B.ByteString
kwPristine = BC.pack "pristine:"

kwParent :: B.ByteString
kwParent = BC.pack "Starting with inventory:"

kwHash :: B.ByteString
kwHash = BC.pack "hash:"

-- * Properties

prop_inventoryParseFormat :: TestOnly => Inventory -> Bool
prop_inventoryParseFormat inv =
  Right inv == parseInventory (toStrictByteString (formatInventory inv))

prop_peekPokePristineHash :: TestOnly => (PristineHash, B.ByteString) -> Bool
prop_peekPokePristineHash (hash, raw) =
  hash == peekPristineHash (toStrictByteString (pokePristineHash hash raw))

prop_skipPokePristineHash :: TestOnly => (PristineHash, B.ByteString) -> Bool
prop_skipPokePristineHash (hash, raw) =
  raw == skipPristineHash (toStrictByteString (pokePristineHash hash raw))
