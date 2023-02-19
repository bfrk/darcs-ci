--  Copyright (C) 2009-2011 Petr Rockai
--
--  BSD3

-- | A few darcs-specific utility functions. These are used for reading and
-- writing darcs and darcs-compatible hashed trees.
module Darcs.Util.Tree.Hashed
    ( -- * Obtaining Trees.
    --
    -- | Please note that Trees obtained this way will contain Stub
    -- items. These need to be executed (they are IO actions) in order to be
    -- accessed. Use 'expand' to do this. However, many operations are
    -- perfectly fine to be used on a stubbed Tree (and it is often more
    -- efficient to do everything that can be done before expanding a Tree).
      readDarcsHashed
    -- * Writing trees.
    , writeDarcsHashed
    -- * Interact with hashed tree
    , hashedTreeIO
    -- * Other
    , readDarcsHashedNosize
    , darcsAddMissingHashes
    , darcsTreeHash
    , darcsUpdateHashes
    , followPristineHashes
    ) where

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL

import Control.Monad.State.Strict ( liftIO )
import Data.List ( sortBy )

import Darcs.Prelude

import Darcs.Util.Cache
    ( Cache
    , Compression(..)
    , fetchFileUsingCache
    , writeFileUsingCache
    )
import Darcs.Util.Hash
    ( Hash
    , encodeBase16
    , encodeHash
    , sha256
    , showHash
    )
import Darcs.Util.Parser
import Darcs.Util.Path ( Name, decodeWhiteName, encodeWhiteName )
import Darcs.Util.Progress ( debugMessage )
import Darcs.Util.Tree
    ( Blob(..)
    , ItemType(..)
    , Tree(..)
    , TreeItem(..)
    , addMissingHashes
    , expand
    , itemHash
    , list
    , listImmediate
    , makeTreeWithHash
    , readBlob
    , updateSubtrees
    , updateTree
    )
import Darcs.Util.Tree.Monad ( TreeIO, runTreeMonad )
import Darcs.Util.ValidHash
    ( PristineHash
    , ValidHash(..)
    , fromHash
    , getHash
    , getSize
    )

----------------------------------------------
-- Darcs directory format.
--

-- Precondition: all (immediate) items in the tree have hashes
darcsFormatDir :: Tree m -> BL.ByteString
darcsFormatDir =
  BL.fromChunks . map formatItem . sortBy cmp . listImmediate
  where
    cmp (a, _) (b, _) = compare a b
    formatItem (name, item) = BC.unlines
      [ case item of
          File _ -> kwFile
          _      -> kwDir
      , encodeWhiteName name
      , case itemHash item of
          Nothing -> error "precondition of darcsFormatDir"
          Just h  -> encodeBase16 h
      ]

darcsParseDir
  :: FilePath -> BC.ByteString -> Either String [(ItemType, Name, PristineHash)]
darcsParseDir path = withPath path . parseAll (many pDir)
  where
    pDir = do
      t <- pHeader
      char '\n'
      n <- pName
      char '\n'
      h <- pHash
      char '\n'
      return (t, n, h)
    pHeader = (BlobType <$ string kwFile) <|> (TreeType <$ string kwDir)
    pName   = do
      name <- takeTillChar '\n'
      either fail return (decodeWhiteName name)
    pHash = do
      hash <- takeTillChar '\n'
      maybe (fail "expected valid hash") return (decodeValidHash (BC.unpack hash))

kwFile, kwDir :: BC.ByteString
kwFile = BC.pack "file:"
kwDir = BC.pack "directory:"

----------------------------------------
-- Utilities.
--

-- | Compute a darcs-compatible hash value for a tree-like structure.
darcsTreeHash :: Tree m -> Hash
darcsTreeHash = sha256 . darcsFormatDir

darcsUpdateDirHashes :: Tree m -> Tree m
darcsUpdateDirHashes = updateSubtrees update
    where update t = t { treeHash = Just (darcsTreeHash t) }

darcsUpdateHashes :: Monad m => Tree m -> m (Tree m)
darcsUpdateHashes = updateTree update
    where update (SubTree t) =
              -- why not recursively ensure that hashes exist here?
              return . SubTree $ t { treeHash = Just (darcsTreeHash t) }
          update (File blob@(Blob con _)) =
              do hash <- sha256 <$> readBlob blob
                 return $ File (Blob con (Just hash))
          update stub = return stub

darcsHash :: Monad m => TreeItem m -> m Hash
darcsHash (SubTree t) = return (darcsTreeHash t)
darcsHash (File blob) = sha256 <$> readBlob blob
darcsHash (Stub unstub _) = darcsTreeHash <$> unstub

darcsAddMissingHashes :: (Monad m) => Tree m -> m (Tree m)
darcsAddMissingHashes = addMissingHashes darcsHash

-------------------------------------------
-- Reading darcs pristine data
--

-- | Read and parse a darcs-style hashed directory listing from a given @cache@
-- and with a given @hash@.
readDarcsHashedDir :: Cache
                   -> PristineHash
                   -> IO [(ItemType, Name, PristineHash)]
readDarcsHashedDir cache ph = do
  debugMessage $ "readDarcsHashedDir: " ++ encodeValidHash ph
  (file, content) <- fsReadHashedFile cache ph
  either fail return $ darcsParseDir file content

-- | Read a darcs-style hashed tree.
readDarcsHashed' :: Bool -> Cache -> PristineHash -> IO (Tree IO)
readDarcsHashed' sizefail cache root = do
  items' <- readDarcsHashedDir cache root
  subs <- sequence [
           do let h = getHash ph
              case getSize ph of
                Just _ | sizefail ->
                  fail ("Unexpectedly encountered size-prefixed hash in " ++ encodeValidHash root)
                _ -> return ()
              case tp of
                BlobType -> return (d, File $
                                       Blob (readBlob' ph) (Just h))
                TreeType ->
                  do let t = readDarcsHashed cache ph
                     return (d, Stub t (Just h))
           | (tp, d, ph) <- items' ]
  return $ makeTreeWithHash subs (getHash root)
    where readBlob' = fmap (BL.fromStrict . snd) . fsReadHashedFile cache

readDarcsHashed :: Cache -> PristineHash -> IO (Tree IO)
readDarcsHashed = readDarcsHashed' False

readDarcsHashedNosize :: Cache -> PristineHash -> IO (Tree IO)
readDarcsHashedNosize = readDarcsHashed' True

----------------------------------------------------
-- Writing darcs-style hashed trees.
--

-- | Write a Tree into a darcs-style hashed directory.
writeDarcsHashed :: Tree IO -> Cache -> IO PristineHash
writeDarcsHashed tree' cache =
    do debugMessage "writeDarcsHashed"
       t <- darcsUpdateDirHashes <$> expand tree'
       sequence_ [ dump =<< readBlob b | (_, File b) <- list t ]
       let dirs = darcsFormatDir t : [ darcsFormatDir d | (_, SubTree d) <- list t ]
       _ <- mapM dump dirs
       return (fromHash (darcsTreeHash t))
  where
    dump = fsCreateHashedFile cache

-- | Create a hashed file from a 'Cache' and file content. In case the file
-- exists it is kept untouched and is assumed to have the right content.
-- TODO
-- Corrupt files should be probably renamed out of the way automatically or
-- something (probably when they are being read though).
fsCreateHashedFile :: Cache -> BL.ByteString -> IO PristineHash
fsCreateHashedFile cache content =
  -- FIXME pass Compression as an argument as we do elsewhere
  writeFileUsingCache cache GzipCompression (BL.toStrict content)

fsReadHashedFile :: Cache -> PristineHash -> IO (FilePath, BC.ByteString)
fsReadHashedFile = fetchFileUsingCache

-- | Run a 'TreeIO' @action@ in a hashed setting. The @initial@ tree is assumed
-- to be fully available from the @cache@, and any changes will be written
-- out to same. Please note that actual filesystem files are never removed.
hashedTreeIO :: TreeIO a -- ^ action
             -> Tree IO -- ^ initial
             -> Cache
             -> IO (a, Tree IO)
hashedTreeIO action t cache = runTreeMonad action t (Just darcsHash) updateItem
  where
    updateItem _ (File b) = File <$> updateFile b
    updateItem _ (SubTree s) = SubTree <$> updateSub s
    updateItem _ x = return x

    -- This code is somewhat tricky. The original Tree may have come from
    -- anywhere e.g. a plain Tree. So when we modify the content of a
    -- file, we not only write a new hashed file, but also modify the
    -- Blob itself, so that the embedded read action read this new hashed
    -- file.
    -- FIXME this assumes that each Blob contains a valid hash; it would
    -- be safer not to rely on this and instead re-calculate the hash.
    updateFile (Blob _ Nothing) =
      error "expected Blob with hash"
    updateFile b@(Blob _ !(Just h)) = do
      liftIO $ debugMessage $ "hashedTreeIO.updateFile: old hash=" ++ encodeHash h
      content <- liftIO $ readBlob b
      let nblob = Blob rblob (Just h)
          rblob =
            BL.fromStrict . snd <$>
            fsReadHashedFile cache (fromHash h)
      nhash <- liftIO $ fsCreateHashedFile cache content
      liftIO $ debugMessage $ "hashedTreeIO.updateFile: new hash=" ++ encodeValidHash nhash
      return nblob
    updateSub s = do
      let !hash = treeHash s
      liftIO $ debugMessage $ "hashedTreeIO.updateSub: old hash=" ++ showHash hash
      nhash <- liftIO $ fsCreateHashedFile cache (darcsFormatDir s)
      liftIO $ debugMessage $ "hashedTreeIO.updateSub: new hash=" ++ encodeValidHash nhash
      return s

-- | Return all 'PristineHash'es reachable from the given root set, which must
-- consist of directory hashes only.
followPristineHashes :: Cache -> [PristineHash] -> IO [PristineHash]
followPristineHashes cache = followAll
  where
    followAll roots = concat <$> mapM followOne roots
    followOne root = do
      x <- readDarcsHashedDir cache root
      let subs   = [ ph | (TreeType, _, ph) <- x ]
          hashes = root : [ ph | (_, _, ph) <- x ]
      (hashes ++) <$> followAll subs
