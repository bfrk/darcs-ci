--  Copyright (C) 2009-2011 Petr Rockai
--
--  BSD3
{-# LANGUAGE ParallelListComp #-}

-- | This module implements an "object storage". This is a directory on disk
-- containing a content-addressed storage. This is useful for storing all kinds
-- of things, particularly filesystem trees, or darcs pristine caches and patch
-- objects. However, this is an abstract, flat storage: no tree semantics are
-- provided. You just need to provide a reference-collecting functionality,
-- computing a list of references for any given object. The system provides
-- transparent garbage collection and packing.
module Darcs.Util.ObjectStore
  ( Format(..)
  , Block
  , OS
    -- * Basic operations.
  , hatch
  , compact
  , repack
  , lookup
    -- * Creating and loading.
  , create
  , load
    -- * Low-level.
  , format
  , blockLookup
  , live
  , hatchery
  , mature
  , roots
  , references
  , rootdir
  ) where

import Prelude hiding ((<$>), lookup, read)

import Control.Applicative ( (<$>) )
import Control.Monad ( forM, forM_ )

import Data.Binary ( decode, encode )
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as BS
import Data.Int ( Int64 )
import Data.List ( sort )
import qualified Data.Map as M
import Data.Maybe ( catMaybes, isNothing, listToMaybe )
import qualified Data.Set as S

import System.Directory
    ( createDirectoryIfMissing
    , doesFileExist
    , getDirectoryContents
    , removeFile
    )
import System.FilePath ( (<.>), (</>) )
import System.Posix.Files ( isDirectory )

import Darcs.Util.File ( getFileStatus )
import Darcs.Util.Hash
import Darcs.Util.ObjectStore.Utils

-- | On-disk format for object storage: we implement a completely loose format
-- (one file per object), a compact format stored in a single append-only file
-- and an immutable \"pack\" format.
data Format
  = Loose
  | Compact
  | Pack
  deriving (Show, Eq)

loose_dirs :: [[Char]]
loose_dirs =
  let chars = ['0' .. '9'] ++ ['a' .. 'f']
   in [[a, b] | a <- chars, b <- chars]

loosePath :: OS -> Hash -> FilePath
loosePath os hash =
  let hash' = BC.unpack (encodeBase16 hash)
   in rootdir os </> "hatchery" </> take 2 hash' </> drop 2 hash'

looseLookup :: OS -> Hash -> IO (Maybe FileSegment)
looseLookup os hash = do
  let path = loosePath os hash
  exist <- doesFileExist path
  return $ if exist then Just (path, Nothing) else Nothing

-- | Object storage block. When used as a hatchery, the loose or compact format
-- are preferable, while for mature space, the pack format is more useful.
data Block = Block
  { blockLookup :: Hash -> IO (Maybe FileSegment)
  , size :: Int64
  , format :: Format
  }

-- | Object storage. Contains a single \"hatchery\" and possibly a number of
-- mature space blocks, usually in form of packs. It also keeps a list of root
-- pointers and has a way to extract pointers from objects (externally
-- supplied). These last two things are used to implement a simple GC.
data OS = OS
  { hatchery :: Block
  , mature :: [Block]
  , roots :: [Hash]
  , references :: FileSegment -> IO [Hash]
  , rootdir :: FilePath
  }

-- | Reduce number of packs in the object storage. This may both recombine
-- packs to eliminate dead objects and join some packs to form bigger packs.
repack :: OS -> IO OS
repack _ = error "repack undefined"

-- | Add new objects to the object storage (i.e. put them into hatchery). It is
-- safe to call this even on objects that are already present in the storage:
-- such objects will be skipped.
hatch :: OS -> [BL.ByteString] -> IO OS
hatch os blobs = do
  processed <- mapM sieve blobs
  write [(h, b) | (True, h, b) <- processed]
  where
    write bits =
      case format (hatchery os) of
        Loose -> do
          _ <- forM bits $ \(hash, blob) -> BL.writeFile (loosePath os hash) blob
          return os
        Compact -> error "hatch/compact undefined"
        _ -> fail "Hatchery must be either Loose or Compact."
    sieve blob = do
      let hash = sha256 blob
      absent <- isNothing <$> lookup os hash
      return (absent, hash, blob)

-- | Move things from hatchery into a (new) pack.
compact :: OS -> IO OS
compact os = do
  objects <- live os [hatchery os]
  block <- createPack os (M.toList objects)
  cleanup
  return $ os {mature = block : mature os}
  where
    cleanup =
      case format (hatchery os) of
        Loose -> forM_ loose_dirs $ nuke . ((rootdir os </> "hatchery") </>)
        Compact -> removeFile (rootdir os </> "hatchery") >> return ()
        _ -> fail "Hatchery must be either Loose or Compact."
    nuke dir =
      mapM (removeFile . (dir </>)) =<<
      (Prelude.filter (`notElem` [".", ".."]) `fmap` getDirectoryContents dir)

blocksLookup :: [Block] -> Hash -> IO (Maybe (Hash, FileSegment))
blocksLookup blocks hash = do
  segment <- cat `fmap` mapM (flip blockLookup hash) blocks
  return $
    case segment of
      Nothing -> Nothing
      Just seg -> Just (hash, seg)
  where
    cat = listToMaybe . catMaybes

lookup :: OS -> Hash -> IO (Maybe FileSegment)
lookup os hash = do
  res <- blocksLookup (hatchery os : mature os) hash
  return $
    case res of
      Nothing -> Nothing
      Just (_, seg) -> Just seg

-- | Create an empty object storage in given directory, with a hatchery of
-- given format. The directory is created if needed, but is assumed to be
-- empty.
create :: FilePath -> Format -> IO OS
create path fmt = do
  createDirectoryIfMissing True path
  _ <- initHatchery
  load path
  where
    initHatchery
      | fmt == Loose = do
        mkdir hatchpath
        forM loose_dirs $ mkdir . (hatchpath </>)
      | fmt == Compact = error "create/mkHatchery Compact undefined"
      | otherwise = error "create/mkHatchery Pack undefined"
    mkdir = createDirectoryIfMissing False
    hatchpath = path </> "hatchery"

load :: FilePath -> IO OS
load rootdir = do
  have_hatch <- getFileStatus $ rootdir </> "hatchery"
  case have_hatch of
    Nothing -> fail $ rootdir ++ " does not exist!"
    Just hatch_stat -> do
      let is_dir = isDirectory hatch_stat
          hatchery =
            Block
              { blockLookup = look os
              , format = if is_dir then Loose else Compact
              , size = undefined
              }
          references = undefined
          look
            | format hatchery == Loose = looseLookup
            | otherwise = undefined
          mature = [] -- FIXME read packs
          roots = [] -- FIXME read root pointers
          os = OS {..}
      return os

readPack :: FilePath -> IO Block
readPack file = do
  bits <- readSegment (file, Nothing)
  let count = decode (BL.take 8 $ bits)
      lookup_ hash first final = do
        let middle = first + ((final - first) `div` 2)
            rawhash = rawHash hash
        res <-
          case ( compare rawhash (hashof first)
               , compare rawhash (hashof middle)
               , compare rawhash (hashof final)) of
            (LT, _, _) -> return Nothing
            (_, _, GT) -> return Nothing
            (EQ, _, _) -> return $ Just (segof first)
            (_, _, EQ) -> return $ Just (segof final)
            (GT, EQ, LT) -> return $ Just (segof middle)
            (GT, GT, LT)
              | middle /= final -> lookup_ hash middle final
            (GT, LT, LT)
              | first /= middle -> lookup_ hash first middle
            (_, _, _) -> return Nothing
        return res
      headerof i = BL.take 51 $ BL.drop (8 + i * 51) bits
      hashof i = BC.concat $ BL.toChunks $ BL.take 32 $ headerof i
      segof i = (file, Just (count * 51 + 8 + from, sz))
        where
          from = decode (BL.take 8 $ BL.drop 33 $ headerof i)
          sz = decode (BL.take 8 $ BL.drop 42 $ headerof i)
  return $
    Block
      { size = BL.length bits
      , format = Pack
      , blockLookup = \h -> lookup_ h 0 (count - 1)
      }

createPack :: OS -> [(Hash, FileSegment)] -> IO Block
createPack os bits = do
  contents <- mapM readSegment (map snd bits)
  let offsets = scanl (+) 0 $ map BL.length contents
      headerbits =
        [ BL.concat
            [ BL.fromChunks [BS.fromShort rawhash]
            , packAscii "@"
            , encode offset
            , packAscii "!"
            , encode $ BL.length string
            , packAscii "\n"
            ]
        | (SHA256 rawhash, _) <- bits
        | string <- contents
        | offset <- offsets
        ]
      header = BL.concat $ (encode $ length bits) : sort headerbits
      blob = BL.concat $ header : contents
      hash = sha256 blob
      path = rootdir os </> BC.unpack (encodeBase16 hash) <.> "bin"
      packAscii = BL.fromStrict . BC.pack
  BL.writeFile path blob
  readPack path

-- | Build a map of live objects (i.e. those reachable from the given roots) in
-- a given list of Blocks.
live :: OS -> [Block] -> IO (M.Map Hash FileSegment)
live os blocks =
  reachable (references os) (blocksLookup blocks) (S.fromList $ roots os)
