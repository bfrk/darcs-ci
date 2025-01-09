module Darcs.Repository.Traverse
    ( cleanRepository
    , cleanPristineDir
    , listInventories
    , specialPatches
    ) where

import Darcs.Prelude

import qualified Data.ByteString.Char8 as BC ( unpack, pack )
import qualified Data.Set as Set

import System.Directory ( listDirectory, withCurrentDirectory )
import System.FilePath.Posix( (</>) )

import Darcs.Repository.Inventory
    ( Inventory(..)
    , PristineHash
    , emptyInventory
    , encodeValidHash
    , inventoryPatchNames
    , parseInventory
    , peekPristineHash
    , skipPristineHash
    )
import Darcs.Repository.InternalTypes
    ( Repository
    , AccessType(..)
    , repoCache
    , withRepoDir
    )
import Darcs.Repository.Paths
    ( tentativeHashedInventoryPath
    , tentativePristinePath
    , inventoriesDirPath
    , patchesDirPath
    , pristineDirPath
    )

import Darcs.Util.ByteString ( gzReadFilePS )
import Darcs.Util.Cache
    ( Cache
    , HashedDir(HashedPristineDir)
    , cleanCachesWithHint
    )
import Darcs.Util.Exception ( ifDoesNotExistError )
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
    reachable <- set . map encodeValidHash <$> followPristineHashes cache roots
    files <- set <$> listDirectory pristineDirPath
    let to_remove = unset $ files `Set.difference` reachable
    withCurrentDirectory pristineDirPath $
      mapM_ removeFileMayNotExist to_remove
    -- and also clean out any global caches
    debugMessage "Cleaning out any global caches..."
    cleanCachesWithHint cache HashedPristineDir to_remove
  where
    set = Set.fromList . map BC.pack
    unset = map BC.unpack . Set.toList

-- | Set difference between two lists of hashes.
diffHashLists :: [String] -> [String] -> [String]
diffHashLists xs ys = from_set $ (to_set xs) `Set.difference` (to_set ys)
  where
    to_set = Set.fromList . map BC.pack
    from_set = map BC.unpack . Set.toList

-- | Remove unreferenced files in the inventories directory.
cleanInventories :: Repository 'RW p wU wR -> IO ()
cleanInventories _ = do
    debugMessage "Cleaning out inventories..."
    hs <- listInventoriesLocal
    fs <- ifDoesNotExistError [] $ listDirectory inventoriesDirPath
    mapM_ (removeFileMayNotExist . (inventoriesDirPath </>))
        (diffHashLists fs hs)

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
    mapM_ (removeFileMayNotExist . (patchesDirPath </>)) (diffHashLists fs hs)

-- | Return a list of the inventories hashes.
-- The argument can be 'readInventory' or 'readInventoryLocal'.
listInventoriesWith :: (FilePath -> IO Inventory) -> IO [String]
listInventoriesWith readInv = do
    mbNextInv <- getParent tentativeHashedInventoryPath
    withCurrentDirectory inventoriesDirPath (follow mbNextInv)
  where
    getParent path = inventoryParent <$> readInv path
    follow Nothing = return []
    follow (Just hash) = do
      let parentFileName = encodeValidHash hash
      mbNextInv <- getParent parentFileName
      (parentFileName :) <$> follow mbNextInv

-- | Return a list of the inventories hashes.
-- This function attempts to retrieve missing inventory files from the cache.
listInventories :: IO [String]
listInventories = listInventoriesWith readInventory

-- | Return inventories hashes by following the head inventory.
-- This function does not attempt to retrieve missing inventory files.
listInventoriesLocal :: IO [String]
listInventoriesLocal = listInventoriesWith readInventoryLocal

-- | Return a list of the patch filenames, extracted from inventory
-- files, by starting with the head inventory and then following the
-- chain of parent inventories.
--
-- This function does not attempt to download missing inventory files.
listPatchesLocal :: IO [String]
listPatchesLocal = do
  inventory <- readInventory tentativeHashedInventoryPath
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
  ifDoesNotExistError emptyInventory $ readInventory path

-- | Read an inventory from a file. Fails with an error message if
-- file is not there or cannot be parsed.
readInventory :: FilePath -> IO Inventory
readInventory path = do
  -- FIXME we should check the hash (if this is a hashed file)
  inv <- skipPristineHash <$> gzReadFilePS path
  case parseInventory inv of
    Right r -> return r
    Left e -> fail $ unlines [unwords ["parse error in file", path], e]
