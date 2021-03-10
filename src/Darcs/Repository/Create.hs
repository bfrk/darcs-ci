module Darcs.Repository.Create
    ( createRepository
    , createRepositoryV1
    , createRepositoryV2
    , EmptyRepository(..)
    , writePristine
    ) where

import Darcs.Prelude

import Control.Monad ( when )
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Maybe( isJust )
import System.Directory
    ( createDirectory
    , getCurrentDirectory
    , setCurrentDirectory
    )
import System.IO.Error
    ( catchIOError
    , isAlreadyExistsError
    )

import Darcs.Patch ( RepoPatch )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Patch.Set ( Origin, emptyPatchSet )
import Darcs.Patch.V1 ( RepoPatchV1 )
import Darcs.Patch.V2 ( RepoPatchV2 )
import Darcs.Patch.V3 ( RepoPatchV3 )
import qualified Darcs.Patch.V1.Prim as V1 ( Prim(..) )
import qualified Darcs.Patch.V2.Prim as V2 ( Prim(..) )

import Darcs.Util.Cache ( Cache )
import Darcs.Repository.Format
    ( RepoFormat
    , createRepoFormat
    , writeRepoFormat
    )
import Darcs.Repository.Flags
    ( UseCache(..)
    , WithWorkingDir (..)
    , WithPatchIndex (..)
    , PatchFormat (..)
    )
import Darcs.Repository.Inventory
    ( pokePristineHash
    , mkValidHash
    )
import Darcs.Repository.Paths
    ( pristineDirPath
    , patchesDirPath
    , inventoriesDirPath
    , hashedInventoryPath
    , formatPath
    )
import Darcs.Repository.Identify ( seekRepo )
import Darcs.Repository.InternalTypes
    ( AccessType(..)
    , PristineType(..)
    , Repository
    , mkRepo
    )
import Darcs.Repository.PatchIndex ( createOrUpdatePatchIndexDisk )
import Darcs.Repository.Prefs
    ( writeDefaultPrefs
    , getCaches
    , prefsDirPath
    )

import Darcs.Util.ByteString( gzReadFilePS )
import Darcs.Util.File ( withCurrentDirectory )
import Darcs.Util.Global ( darcsdir )
import Darcs.Util.Hash( encodeBase16 )
import Darcs.Util.Lock
    ( writeBinFile
    , writeDocBinFile
    )
import Darcs.Util.Path ( AbsoluteOrRemotePath, ioAbsoluteOrRemote )
import Darcs.Util.Tree( Tree, emptyTree )
import Darcs.Util.Tree.Hashed( writeDarcsHashed, darcsAddMissingHashes )

createRepositoryFiles :: PatchFormat -> WithWorkingDir -> IO RepoFormat
createRepositoryFiles patchfmt withWorkingDir = do
  cwd <- getCurrentDirectory
  x <- seekRepo
  when (isJust x) $ do
      setCurrentDirectory cwd
      putStrLn "WARNING: creating a nested repository."
  createDirectory darcsdir `catchIOError`
      (\e-> if isAlreadyExistsError e
            then fail "Tree has already been initialized!"
            else fail $ "Error creating directory `"++darcsdir++"'.")
  createDirectory pristineDirPath
  createDirectory patchesDirPath
  createDirectory inventoriesDirPath
  createDirectory prefsDirPath
  writeDefaultPrefs
  let repo_format = createRepoFormat patchfmt withWorkingDir
  writeRepoFormat repo_format formatPath
  -- note: all repos we create nowadays are hashed
  writeBinFile hashedInventoryPath B.empty
  return repo_format

data EmptyRepository where
  EmptyRepository :: (RepoPatch p, ApplyState p ~ Tree)
                  => Repository 'RO p Origin Origin Origin
                  -> EmptyRepository

createRepository :: PatchFormat -> WithWorkingDir -> WithPatchIndex -> UseCache
                 -> IO EmptyRepository
createRepository patchfmt withWorkingDir withPatchIndex useCache = do
  rfmt <- createRepositoryFiles patchfmt withWorkingDir
  rdir <- ioAbsoluteOrRemote here
  cache <- getCaches useCache Nothing
  writePristine here cache emptyTree
  repo@(EmptyRepository r) <- case patchfmt of
    PatchFormat1 -> return $ EmptyRepository $ mkRepoV1 rdir rfmt cache
    PatchFormat2 -> return $ EmptyRepository $ mkRepoV2 rdir rfmt cache
    PatchFormat3 -> return $ EmptyRepository $ mkRepoV3 rdir rfmt cache
  maybeCreatePatchIndex withPatchIndex r
  return repo

mkRepoV1
  :: AbsoluteOrRemotePath
  -> RepoFormat
  -> Cache
  -> Repository 'RO (RepoPatchV1 V1.Prim) Origin Origin Origin
mkRepoV1 rdir repofmt cache = mkRepo rdir repofmt HashedPristine cache

mkRepoV2
  :: AbsoluteOrRemotePath
  -> RepoFormat
  -> Cache
  -> Repository 'RO (RepoPatchV2 V2.Prim) Origin Origin Origin
mkRepoV2 rdir repofmt cache = mkRepo rdir repofmt HashedPristine cache

mkRepoV3
  :: AbsoluteOrRemotePath
  -> RepoFormat
  -> Cache
  -> Repository 'RO (RepoPatchV3 V2.Prim) Origin Origin Origin
mkRepoV3 rdir repofmt cache = mkRepo rdir repofmt HashedPristine cache

createRepositoryV1
  :: WithWorkingDir -> WithPatchIndex -> UseCache
  -> IO (Repository 'RO (RepoPatchV1 V1.Prim) Origin Origin Origin)
createRepositoryV1 withWorkingDir withPatchIndex useCache = do
  rfmt <- createRepositoryFiles PatchFormat1 withWorkingDir
  rdir <- ioAbsoluteOrRemote here
  cache <- getCaches useCache Nothing
  writePristine here cache emptyTree
  let repo = mkRepoV1 rdir rfmt cache
  maybeCreatePatchIndex withPatchIndex repo
  return repo

createRepositoryV2
  :: WithWorkingDir -> WithPatchIndex -> UseCache
  -> IO (Repository 'RO (RepoPatchV2 V2.Prim) Origin Origin Origin)
createRepositoryV2 withWorkingDir withPatchIndex useCache = do
  rfmt <- createRepositoryFiles PatchFormat2 withWorkingDir
  rdir <- ioAbsoluteOrRemote here
  cache <- getCaches useCache Nothing
  writePristine here cache emptyTree
  let repo = mkRepoV2 rdir rfmt cache
  maybeCreatePatchIndex withPatchIndex repo
  return repo

maybeCreatePatchIndex :: (RepoPatch p, ApplyState p ~ Tree)
                      => WithPatchIndex -> Repository 'RO p Origin wU Origin -> IO ()
maybeCreatePatchIndex NoPatchIndex _ = return ()
maybeCreatePatchIndex YesPatchIndex repo =
  createOrUpdatePatchIndexDisk repo emptyPatchSet

writePristine :: FilePath -> Cache -> Tree IO -> IO ()
writePristine dir cache tree =
  withCurrentDirectory dir $ do
    inv <- gzReadFilePS hashedInventoryPath
    tree' <- darcsAddMissingHashes tree
    root <- writeDarcsHashed tree' cache
    writeDocBinFile hashedInventoryPath $
      pokePristineHash (mkValidHash $ BC.unpack $ encodeBase16 root) inv

here :: String
here = "."
