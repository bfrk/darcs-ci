module Darcs.Repository.Traverse
    ( cleanRepository
    , cleanPristineDir
    , listInventories
    , specialPatches
    ) where

import Darcs.Prelude

import Control.Monad.Catch ( handleIOError )
import qualified Data.Set as S

import System.Directory ( listDirectory, withCurrentDirectory )
import System.FilePath.Posix ( (</>) )

import Darcs.Repository.Branch
    ( Branch(..)
    , getAllBranches
    , readCurrentBranchName
    , updateBranch
    )
import Darcs.Repository.InternalTypes ( AccessType(..), Repository, repoCache )
import Darcs.Repository.Inventory
    ( Inventory(..)
    , InventoryHash
    , PristineHash
    , encodeValidHash
    , inventoryPatchNames
    , parseInventory
    , parseInventoryParent
    )
import Darcs.Repository.Paths ( inventoriesDirPath, patchesDirPath, pristineDirPath )

import Darcs.Util.Cache
    ( Cache
    , HashedDir(..)
    , cleanCachesWithHint
    , fetchFileUsingCache
    )
import Darcs.Util.Exception ( ifDoesNotExistError )
import Darcs.Util.Global ( debugMessage )
import Darcs.Util.Lock ( removeFileMayNotExist )
import Darcs.Util.Tree.Hashed ( followPristineHashes )


cleanRepository :: Repository 'RW p wU wR -> IO ()
cleanRepository r = do
  -- In case this is called bu code other than that for `optimize clean`.
  -- TODO remove as soon as all code has been updated to use current
  -- branch instead of tentativeHashedInventory
  readCurrentBranchName r >>= updateBranch r
  cleanPristine r >> cleanInventories r >> cleanPatches r

-- | Remove unreferenced entries in the pristine cache.
cleanPristine :: Repository 'RW p wU wR -> IO ()
cleanPristine repo = do
    debugMessage "Cleaning out the pristine cache..."
    roots <- map pristineHash <$> getAllBranches repo
    cleanPristineDir (repoCache repo) roots

cleanPristineDir :: Cache -> [PristineHash] -> IO ()
cleanPristineDir cache roots = do
    reachable <- map encodeValidHash <$> followPristineHashes cache roots
    files <- listDirectory pristineDirPath
    let to_remove = diffLists files reachable
    withCurrentDirectory pristineDirPath $
      mapM_ removeFileMayNotExist to_remove
    cleanCachesWithHint cache HashedPristineDir to_remove

-- | Set difference between two lists.
diffLists :: Ord a => [a] -> [a] -> [a]
diffLists xs ys =
  S.toList $ S.fromList xs `S.difference` S.fromList ys

-- | Remove unreferenced files in the inventories directory.
cleanInventories :: Repository 'RW p wU wR -> IO ()
cleanInventories repo = do
  debugMessage "Cleaning out inventories..."
  reachable <- map encodeValidHash <$> listInventoryHashes repo
  debugMessage $ unlines ("Reachable inventories:":reachable)
  files <- listDirectory inventoriesDirPath
  let to_remove = diffLists files reachable
  withCurrentDirectory inventoriesDirPath $
    mapM_ (removeFileMayNotExist) to_remove
  cleanCachesWithHint (repoCache repo) HashedInventoriesDir to_remove

-- FIXME this is ugly, these files should be directly under _darcs
-- since they are not hashed. And 'unrevert' isn't even a real patch but
-- a patch bundle. Unfortunately this is an incompatible format change.

-- | List of special patch files that may exist in the directory
-- _darcs/patches/. We must not clean those.
specialPatches :: [FilePath]
specialPatches = ["unrevert", "pending", "pending.tentative"]

-- | Remove unreferenced files in the patches directory.
cleanPatches :: Repository 'RW p wU wR -> IO ()
cleanPatches repo = do
  debugMessage "Cleaning out patches..."
  reachable <- (specialPatches ++) <$> listPatchNames repo
  files <- ifDoesNotExistError [] (listDirectory patchesDirPath)
  let to_remove = diffLists files reachable
  withCurrentDirectory patchesDirPath $
    mapM_ removeFileMayNotExist to_remove
  cleanCachesWithHint (repoCache repo) HashedPatchesDir to_remove

-- | Follow the chain of 'InventoryHash'es starting with the given hash. The
-- path to the corresponding hashed file is returned, along with those of its
-- parent inventories.
--
-- The first parameter of type 'Cache' determines where we search for hashed
-- files. To restrict the search to the current directory, pass something like
-- @mkCache [Cache Repo Writable (repoLocation repo]@.
followInventories :: Cache -> [InventoryHash] -> IO [InventoryHash]
followInventories cache = fmap S.toList . go S.empty where
  go done [] = return done
  go done (hash:hashes)
    | hash `S.member` done = go done hashes
    | otherwise = do
      let done' = S.insert hash done
      handleIOError (\_ -> go done' hashes) $ do
        (_, mHash) <- readInventoryParent cache hash
        case mHash of
          Nothing -> go done' hashes
          Just parentHash -> go done' (parentHash:hashes)

listInventories :: Repository 'RW p wU wR -> IO [FilePath]
listInventories repo = do
  roots <- map inventoryHash <$> getAllBranches repo
  map ((inventoriesDirPath </>) . encodeValidHash) <$>
    followInventories (repoCache repo) roots

listInventoryHashes :: Repository 'RW p wU wR -> IO [InventoryHash]
listInventoryHashes repo = do
  roots <- map inventoryHash <$> getAllBranches repo
  followInventories (repoCache repo) roots

listPatchNames :: Repository 'RW p wU wR -> IO [FilePath]
listPatchNames repo = do
  patchnames <-
    fmap concat $
    mapM (fmap (inventoryPatchNames . snd) . readInventory (repoCache repo)) =<<
    listInventoryHashes repo
  return $ S.toList $ S.fromList patchnames

readInventory :: Cache -> InventoryHash -> IO (FilePath, Inventory)
readInventory cache hash = do
  (path, content) <- fetchFileUsingCache cache hash
  case parseInventory content of
    Right r -> return (path, r)
    Left e -> fail $ unlines [unwords ["parse error in file", path], e]

readInventoryParent :: Cache -> InventoryHash -> IO (FilePath, Maybe InventoryHash)
readInventoryParent cache hash = do
  (path, content) <- fetchFileUsingCache cache hash
  case parseInventoryParent content of
    Right r -> return (path, r)
    Left e -> fail $ unlines [unwords ["parse error in file", path], e]
