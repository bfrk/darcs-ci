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
    , decodeDarcsHash
    , decodeDarcsSize
    , darcsUpdateHashes
    , getHashedFiles
    ) where

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL

import Control.Monad.State.Strict ( liftIO, when )
import Data.List ( sortBy )
import Data.Maybe ( fromJust, isJust )

import Darcs.Prelude

import Darcs.Util.Cache
    ( Cache
    , Compression(..)
    , HashedDir(HashedPristineDir)
    , fetchFileUsingCache
    , writeFileUsingCache
    )
import Darcs.Util.Hash ( Hash(..), decodeBase16, encodeBase16, sha256 )
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

---------------------------------------------------------------------
-- Utilities for coping with the darcs directory format.
--

decodeDarcsHash :: BC.ByteString -> Hash
decodeDarcsHash bs = case BC.split '-' bs of
                       [s, h] | BC.length s == 10 -> decodeBase16 h
                       _ -> decodeBase16 bs

decodeDarcsSize :: BC.ByteString -> Maybe Int
decodeDarcsSize bs = case BC.split '-' bs of
                       [s, _] | BC.length s == 10 ->
                                  case reads (BC.unpack s) of
                                    [(x, _)] -> Just x
                                    _ -> Nothing
                       _ -> Nothing

hashedFilename :: (Maybe Int, Hash) -> String
hashedFilename (s,h) = case hash of
                            "" -> error "hashedFilename: invalid hash"
                            _ -> sizeprefix s ++ hash
    where sizeprefix Nothing = ""
          sizeprefix (Just s') = formatSize s' ++ "-"
          formatSize s' = let n = show s' in replicate (10 - length n) '0' ++ n
          hash = showHash h

----------------------------------------------
-- Darcs directory format.
--

darcsFormatDir :: Tree m -> Maybe BL.ByteString
darcsFormatDir t = BL.fromChunks . concat <$>
                       mapM string (sortBy cmp $ listImmediate t)
    where cmp (a, _) (b, _) = compare a b
          string (name, item) =
              do header <- case item of
                             File _ -> Just $ BC.pack "file:\n"
                             _ -> Just $ BC.pack "directory:\n"
                 hash <- case itemHash item of
                           NoHash -> Nothing
                           x -> Just $ encodeBase16 x
                 return   [ header
                          , encodeWhiteName name
                          , BC.singleton '\n'
                          , hash, BC.singleton '\n' ]

darcsParseDir :: FilePath -> BC.ByteString -> [(ItemType, Name, Maybe Int, Hash)]
darcsParseDir path content = parse (BC.split '\n' content)
    where
      parse (t:n:h':r) = (header t,
                          decodeWhiteName n,
                          decodeDarcsSize h',
                          decodeDarcsHash h') : parse r
      parse _ = []
      header x
          | x == BC.pack "file:" = BlobType
          | x == BC.pack "directory:" = TreeType
          | otherwise =
              error $ "Error parsing darcs hashed directory, in file " ++ path

----------------------------------------
-- Utilities.
--

-- | Compute a darcs-compatible hash value for a tree-like structure.
darcsTreeHash :: Tree m -> Hash
darcsTreeHash t = case darcsFormatDir t of
                    Nothing -> NoHash
                    Just x -> sha256 x

darcsUpdateDirHashes :: Tree m -> Tree m
darcsUpdateDirHashes = updateSubtrees update
    where update t = t { treeHash = darcsTreeHash t }

darcsUpdateHashes :: (Monad m) => Tree m -> m (Tree m)
darcsUpdateHashes = updateTree update
    where update (SubTree t) = return . SubTree $ t { treeHash = darcsTreeHash t }
          update (File blob@(Blob con _)) =
              do hash <- sha256 <$> readBlob blob
                 return $ File (Blob con hash)
          update stub = return stub

darcsHash :: (Monad m) => TreeItem m -> m Hash
darcsHash (SubTree t) = return $ darcsTreeHash t
darcsHash (File blob) = sha256 <$> readBlob blob
darcsHash _ = return NoHash

darcsAddMissingHashes :: (Monad m) => Tree m -> m (Tree m)
darcsAddMissingHashes = addMissingHashes darcsHash

-------------------------------------------
-- Reading darcs pristine data
--

-- | Read and parse a darcs-style hashed directory listing from a given @cache@
-- and with a given @hash@.
readDarcsHashedDir :: Cache
                   -> (Maybe Int, Hash)
                   -> IO [(ItemType, Name, Maybe Int, Hash)]
readDarcsHashedDir cache h = do
  debugMessage $ "readDarcsHashedDir: " ++ showHash (snd h)
  uncurry darcsParseDir <$>
    fetchFileUsingCache cache HashedPristineDir (hashedFilename h)

-- | Read a darcs-style hashed tree.
readDarcsHashed' :: Bool -> Cache -> (Maybe Int, Hash) -> IO (Tree IO)
readDarcsHashed' _ _ (_, NoHash) = fail "Cannot readDarcsHashed NoHash"
readDarcsHashed' sizefail cache root@(_, hash) = do
  items' <- readDarcsHashedDir cache root
  subs <- sequence [
           do when (sizefail && isJust s) $
                fail ("Unexpectedly encountered size-prefixed hash in " ++ showHash hash)
              case tp of
                BlobType -> return (d, File $
                                       Blob (readBlob' (s, h)) h)
                TreeType ->
                  do let t = readDarcsHashed cache (s, h)
                     return (d, Stub t h)
           | (tp, d, s, h) <- items' ]
  return $ makeTreeWithHash subs hash
    where readBlob' =
            fmap (BL.fromStrict . snd) .
            fetchFileUsingCache cache HashedPristineDir . hashedFilename

readDarcsHashed :: Cache -> (Maybe Int, Hash) -> IO (Tree IO)
readDarcsHashed = readDarcsHashed' False

readDarcsHashedNosize :: Cache -> Hash -> IO (Tree IO)
readDarcsHashedNosize cache hash = readDarcsHashed' True cache (Nothing, hash)

----------------------------------------------------
-- Writing darcs-style hashed trees.
--

-- | Write a Tree into a darcs-style hashed directory.
writeDarcsHashed :: Tree IO -> Cache -> IO Hash
writeDarcsHashed tree' cache =
    do debugMessage "writeDarcsHashed"
       t <- darcsUpdateDirHashes <$> expand tree'
       sequence_ [ dump =<< readBlob b | (_, File b) <- list t ]
       let dirs = darcsFormatDir t : [ darcsFormatDir d | (_, SubTree d) <- list t ]
       _ <- mapM (dump . fromJust) dirs
       return $ darcsTreeHash t
  where
    dump = fsCreateHashedFile cache

-- | Create a hashed file from a 'Cache' and file content. In case the file
-- exists it is kept untouched and is assumed to have the right content.
-- TODO
-- Corrupt files should be probably renamed out of the way automatically or
-- something (probably when they are being read though).
fsCreateHashedFile :: Cache -> BL.ByteString -> IO String
fsCreateHashedFile cache content =
  -- FIXME pass Compression as an argument as we do elsewhere
  writeFileUsingCache cache GzipCompression HashedPristineDir $
  BL.toStrict content

-- | Run a 'TreeIO' @action@ in a hashed setting. The @initial@ tree is assumed
-- to be fully available from the @cache@, and any changes will be written
-- out to same. Please note that actual filesystem files are never removed.
hashedTreeIO :: TreeIO a -- ^ action
             -> Tree IO -- ^ initial
             -> Cache
             -> IO (a, Tree IO)
hashedTreeIO action t cache =
    runTreeMonad action t darcsHash updateItem
    where updateItem _ (File b) = File <$> updateFile b
          updateItem _ (SubTree s) = SubTree <$> updateSub s
          updateItem _ x = return x

          -- This code is somewhat tricky. The original Tree may have come from
          -- anywhere e.g. a plain Tree. So when we modify the content of a
          -- file, we not only write a new hashed file, but also modify the
          -- Blob itself, so that the embedded read action read this new hashed
          -- file.
          -- FIXME this assumes that each Blob contains a valid hash; it would
          -- be safer not to rely on this and instead re-calculate the hash. I
          -- think this would allow us to remove darcsAddMissingHashes. It is
          -- quite unintuitive to guess when we have to call that function!
          updateFile b@(Blob _ !h) = do
            liftIO $ debugMessage $ "hashedTreeIO.updateFile: old hash=" ++ showHash h
            content <- liftIO $ readBlob b
            let fn = showHash h
                nblob = Blob rblob h
                rblob =
                  BL.fromStrict . snd <$>
                  fetchFileUsingCache cache HashedPristineDir fn
            nhash <- liftIO $ fsCreateHashedFile cache content
            liftIO $ debugMessage $ "hashedTreeIO.updateFile: new hash=" ++ nhash
            return nblob
          updateSub s = do
            let !hash = treeHash s
                Just dirdata = darcsFormatDir s
            liftIO $ debugMessage $ "hashedTreeIO.updateSub: old hash=" ++ showHash hash
            nhash <- liftIO $ fsCreateHashedFile cache dirdata
            liftIO $ debugMessage $ "hashedTreeIO.updateSub: new hash=" ++ nhash
            return s

showHash :: Hash -> String
showHash = BC.unpack . encodeBase16

-- | getHashedFiles returns all hash files targeted by files in hashroots in
-- the hashdir directory.
getHashedFiles :: Cache -> [String] -> IO [String]
getHashedFiles cache hashroots = do
  let listone h = do
        let size = decodeDarcsSize $ BC.pack h
            hash = decodeDarcsHash $ BC.pack h
        x <- readDarcsHashedDir cache (size, hash)
        let subs = [hashedFilename (s, h') | (TreeType, _, s, h') <- x]
            hashes = h : [hashedFilename (s, h') | (_, _, s, h') <- x]
        (hashes ++) . concat <$> mapM listone subs
  concat <$> mapM listone hashroots
