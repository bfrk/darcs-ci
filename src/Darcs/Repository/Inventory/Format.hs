module Darcs.Repository.Inventory.Format
    ( Inventory(..)
    , HeadInventory
    , InventoryEntry
    , ValidHash(..)
    , InventoryHash
    , PatchHash
    , PristineHash
    , inventoryPatchNames
    , parseInventory
    , parseHeadInventory -- not used
    , showInventory
    , showInventoryPatches
    , showInventoryEntry
    , emptyInventory
    , pokePristineHash
    , peekPristineHash
    , skipPristineHash
    , pristineName
    -- properties
    , prop_inventoryParseShow
    , prop_peekPokePristineHash
    , prop_skipPokePristineHash
    ) where

import Darcs.Prelude hiding ( take )

import Control.Applicative ( optional, many )

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Darcs.Patch.Info ( PatchInfo, showPatchInfo, readPatchInfo )
import Darcs.Util.Parser
    ( Parser, parse, string, skipSpace, take, takeTillChar )
import Darcs.Patch.Show ( ShowPatchFor(..) )
import Darcs.Util.Printer
    ( Doc, (<+>), ($$), hcat, text, invisiblePS, packedString, renderPS )
import Darcs.Util.ValidHash
    ( InventoryHash
    , PatchHash
    , PristineHash
    , ValidHash(..)
    , calcValidHash
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

pHeadInv :: Parser HeadInventory
pHeadInv = (,) <$> pPristineHash <*> pInv

pPristineHash :: Parser PristineHash
pPristineHash = do
  string pristineName
  skipSpace
  pHash

pInv :: Parser Inventory
pInv = Inventory <$> pInvParent <*> pInvPatches

pInvParent :: Parser (Maybe InventoryHash)
pInvParent = optional $ do
  string parentName
  skipSpace
  pHash

pHash :: ValidHash h => Parser h
pHash = do
  hash <- pLine
  maybe (fail "expected valid hash") return $ decodeValidHash (BC.unpack hash)

pLine :: Parser B.ByteString
pLine = takeTillChar '\n' <* take 1

pInvPatches :: Parser [InventoryEntry]
pInvPatches = many pInvEntry

pInvEntry :: Parser InventoryEntry
pInvEntry = do
  info <- readPatchInfo
  skipSpace
  string hashName
  skipSpace
  hash <- pHash
  return (info, hash)

-- * Showing

showInventory :: Inventory -> Doc
showInventory inv =
  showParent (inventoryParent inv) <>
  showInventoryPatches (inventoryPatches inv)

showInventoryPatches :: [InventoryEntry] -> Doc
showInventoryPatches = hcat . map showInventoryEntry

showInventoryEntry :: InventoryEntry -> Doc
showInventoryEntry (pinf, hash) =
  showPatchInfo ForStorage pinf $$
  packedString hashName <+> text (encodeValidHash hash) <> packedString newline

showParent :: Maybe InventoryHash -> Doc
showParent (Just hash) =
  packedString parentName $$ text (encodeValidHash hash) <> packedString newline
showParent Nothing = mempty

-- * Accessing the pristine hash

-- | Replace the pristine hash at the start of a raw, unparsed 'HeadInventory'
-- or add it if none is present.
pokePristineHash :: PristineHash -> B.ByteString -> Doc
pokePristineHash hash inv =
  invisiblePS pristineName <> text (encodeValidHash hash) $$ invisiblePS (skipPristineHash inv)

takeHash :: B.ByteString -> Maybe (PristineHash, B.ByteString)
takeHash input = do
  let (hline,rest) = BC.breakSubstring newline input
  ph <- decodeValidHash (BC.unpack hline)
  return (ph, rest)

peekPristineHash :: B.ByteString -> PristineHash
peekPristineHash inv =
  case tryDropPristineName inv of
    Just rest ->
      case takeHash rest of
        Just (h, _) -> h
        Nothing -> error $ "Bad hash in inventory!"
    Nothing -> calcValidHash B.empty

-- |skipPristineHash drops the 'pristine: HASH' prefix line, if present.
skipPristineHash :: B.ByteString -> B.ByteString
skipPristineHash ps =
  case tryDropPristineName ps of
    Just rest -> B.drop 1 $ BC.dropWhile (/= '\n') rest
    Nothing -> ps

tryDropPristineName :: B.ByteString -> Maybe B.ByteString
tryDropPristineName input =
    if prefix == pristineName then Just rest else Nothing
  where
    (prefix, rest) = B.splitAt (B.length pristineName) input

-- * Key phrases

pristineName :: B.ByteString
pristineName = BC.pack "pristine:"

parentName :: B.ByteString
parentName = BC.pack "Starting with inventory:"

hashName :: B.ByteString
hashName = BC.pack "hash:"

newline :: B.ByteString
newline = BC.pack "\n"

-- * Properties

prop_inventoryParseShow :: Inventory -> Bool
prop_inventoryParseShow inv =
  Right inv == parseInventory (renderPS (showInventory inv))

prop_peekPokePristineHash :: (PristineHash, B.ByteString) -> Bool
prop_peekPokePristineHash (hash, raw) =
  hash == peekPristineHash (renderPS (pokePristineHash hash raw))

prop_skipPokePristineHash :: (PristineHash, B.ByteString) -> Bool
prop_skipPokePristineHash (hash, raw) =
  raw == skipPristineHash (renderPS (pokePristineHash hash raw))
