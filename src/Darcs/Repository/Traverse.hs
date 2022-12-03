module Darcs.Repository.Traverse
    ( cleanRepository
    , cleanPristineDir
    , listInventories
    , specialPatches
    ) where

import Darcs.Prelude

import Data.List ( stripPrefix )
import Data.Maybe ( mapMaybe )
import qualified Data.Set as S

import System.Directory ( listDirectory, withCurrentDirectory )
import System.FilePath.Posix( takeFileName, (</>) )

import Darcs.Repository.InternalTypes
    ( AccessType(..)
    , Repository
    , repoCache
    , repoLocation
    , withRepoDir
    )
import Darcs.Repository.Inventory
    ( Inventory(..)
    , InventoryHash
    , PristineHash
    , emptyInventory
    , encodeValidHash
    , inventoryPatchNames
    , parseInventory
    , parseInventoryParent
    , peekPristineHash
    , skipPristineHash
    )
import Darcs.Repository.Paths
    ( inventoriesDirPath
    , patchesDirPath
    , pristineDirPath
    , tentativeHashedInventoryPath
    , tentativePristinePath
    )

import Darcs.Util.ByteString ( gzReadFilePS )
import Darcs.Util.Cache
    ( Cache
    , HashedDir(..)
    , cleanCachesWithHint
    , fetchFileUsingCache
    )
import Darcs.Util.Exception ( ifDoesNotExistError, ifIOError )
import Darcs.Util.Global ( debugMessage )
import Darcs.Util.Lock ( removeFileMayNotExist )
import Darcs.Util.Tree.Hashed ( followPristineHashes )


cleanRepository :: Repository 'RW p wU wR -> IO ()
cleanRepository r = cleanPristine r >> cleanInventories r >> cleanPatches r

-- | Remove unreferenced entries in the pristine cache.
cleanPristine :: Repository 'RW p wU wR -> IO ()
cleanPristine r = withRepoDir r $ do
    debugMessage "Cleaning out the pristine cache..."
    i <- gzReadFilePS tentativePristinePath
    cleanPristineDir (repoCache r) [peekPristineHash i]

cleanPristineDir :: Cache -> [PristineHash] -> IO ()
cleanPristineDir cache roots = do
    reachable <- map encodeValidHash <$> followPristineHashes cache roots
    files <- listDirectory pristineDirPath
    let to_remove = diffLists files reachable
    withCurrentDirectory pristineDirPath $
      mapM_ removeFileMayNotExist to_remove
    -- and also clean out any global caches
    debugMessage "Cleaning out any global caches..."
    cleanCachesWithHint cache HashedPristineDir to_remove

-- | Set difference between two lists.
diffLists :: Ord a => [a] -> [a] -> [a]
diffLists xs ys =
  S.toList $ S.fromList xs `S.difference` S.fromList ys

-- | Remove unreferenced files in the inventories directory.
cleanInventories :: Repository 'RW p wU wR -> IO ()
cleanInventories repo = do
  let cache = repoCache repo
  debugMessage "Cleaning out inventories..."
  mHash <- inventoryParent <$> readInventoryFile tentativeHashedInventoryPath
  reachable <-
    case mHash of
      Nothing -> return []
      Just hash -> map takeFileName <$> followInventories (repoCache repo) hash
  debugMessage $ unlines ("Reachable:":reachable)
  files <- listDirectory inventoriesDirPath
  let to_remove = diffLists files reachable
  withCurrentDirectory inventoriesDirPath $
    mapM_ (removeFileMayNotExist) to_remove
  cleanCachesWithHint cache HashedInventoriesDir to_remove

-- FIXME this is ugly, these files should be directly under _darcs
-- since they are not hashed. And 'unrevert' isn't even a real patch but
-- a patch bundle.

-- | List of special patch files that may exist in the directory
-- _darcs/patches/. We must not clean those.
specialPatches :: [FilePath]
specialPatches = ["unrevert", "pending", "pending.tentative"]

-- | Remove unreferenced files in the patches directory.
cleanPatches :: Repository 'RW p wU wR -> IO ()
cleanPatches _ = do
    debugMessage "Cleaning out patches..."
    hs <- (specialPatches ++) <$> listPatchesLocal
    fs <- ifDoesNotExistError [] (listDirectory patchesDirPath)
    mapM_ (removeFileMayNotExist . (patchesDirPath </>)) (diffLists fs hs)

-- | Follow the chain of 'InventoryHash'es starting with the given hash. The
-- path to the corresponding hashed file is returned, along with those of its
-- parent inventories.
--
-- The first parameter of type 'Cache' determines where we search for hashed
-- files. To restrict the search to the current directory, pass something like
-- @mkCache [Cache Repo Writable (repoLocation repo]@.
followInventories :: Cache -> InventoryHash -> IO [FilePath]
followInventories cache = go where
  go hash =
    ifIOError [] $ do
      (path, mHash) <- readInventoryParent cache hash
      case mHash of
        Nothing -> return [path]
        Just parentHash -> do
          paths <- go parentHash
          return (path : paths)

listInventories :: Repository 'RW p wU wR -> IO [FilePath]
listInventories repo = do
  mHash <- inventoryParent <$> readInventoryFile tentativeHashedInventoryPath
  case mHash of
    Nothing -> return []
    Just hash ->
      mapMaybe (stripPrefix (repoLocation repo ++ "/")) <$>
        followInventories (repoCache repo) hash

-- | Return a list of the patch filenames, extracted from inventory
-- files, by starting with the head inventory and then following the
-- chain of parent inventories.
--
-- This function does not attempt to download missing inventory files.
listPatchesLocal :: IO [String]
listPatchesLocal = do
  inventory <- readInventoryFile tentativeHashedInventoryPath
  followStartingWiths
    (inventoryParent inventory)
    (inventoryPatchNames inventory)
  where
    invDir = inventoriesDirPath
    followStartingWiths Nothing patches = return patches
    followStartingWiths (Just hash) patches = do
      let startingWith = encodeValidHash hash
      inv <- readInventoryLocal (invDir </> startingWith)
      (patches ++) <$>
        followStartingWiths (inventoryParent inv) (inventoryPatchNames inv)

-- | Read the given inventory file if it exist, otherwise return an empty
-- inventory. Used when we expect that some inventory files may be missing.
-- Still fails with an error message if file cannot be parsed.
readInventoryLocal :: FilePath -> IO Inventory
readInventoryLocal path =
  ifDoesNotExistError emptyInventory $ readInventoryFile path

-- | Read an inventory from a file. Fails with an error message if
-- file is not there or cannot be parsed.
readInventoryFile :: FilePath -> IO Inventory
readInventoryFile path = do
  inv <- skipPristineHash <$> gzReadFilePS path
  case parseInventory inv of
    Right r -> return r
    Left e -> fail $ unlines [unwords ["parse error in file", path], e]

readInventoryParent :: Cache -> InventoryHash -> IO (FilePath, Maybe InventoryHash)
readInventoryParent cache hash = do
  (path, content) <- fetchFileUsingCache cache hash
  case parseInventoryParent content of
    Right r -> return (path, r)
    Left e -> fail $ unlines [unwords ["parse error in file", path], e]
