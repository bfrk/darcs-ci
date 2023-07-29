module Darcs.Repository.Create
    ( createRepository
    , createRepositoryV1
    , createRepositoryV2
    , EmptyRepository(..)
    ) where

import Darcs.Prelude

import Control.Monad ( when )
import qualified Data.ByteString as B
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
    ( PatchFormat(..)
    , UseCache(..)
    , WithPatchIndex(..)
    , WithPrefsTemplates(..)
    , WithWorkingDir(..)
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
import Darcs.Repository.Pristine ( writePristine )

import Darcs.Util.Global ( darcsdir )
import Darcs.Util.Lock ( writeBinFile )
import Darcs.Util.Path ( AbsoluteOrRemotePath, ioAbsoluteOrRemote )
import Darcs.Util.Tree( Tree, emptyTree )

createRepositoryFiles :: PatchFormat -> WithWorkingDir -> WithPrefsTemplates -> IO RepoFormat
createRepositoryFiles patchfmt withWorkingDir withPrefsTemplates = do
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
  writeDefaultPrefs withPrefsTemplates
  let repo_format = createRepoFormat patchfmt withWorkingDir
  writeRepoFormat repo_format formatPath
  -- note: all repos we create nowadays are hashed
  writeBinFile hashedInventoryPath B.empty
  return repo_format

data EmptyRepository where
  EmptyRepository :: (RepoPatch p, ApplyState p ~ Tree)
                  => Repository 'RO p Origin Origin
                  -> EmptyRepository

createRepository :: PatchFormat -> WithWorkingDir -> WithPatchIndex -> UseCache -> WithPrefsTemplates
                 -> IO EmptyRepository
createRepository patchfmt withWorkingDir withPatchIndex useCache withPrefsTemplates = do
  rfmt <- createRepositoryFiles patchfmt withWorkingDir withPrefsTemplates
  rdir <- ioAbsoluteOrRemote here
  cache <- getCaches useCache Nothing
  repo@(EmptyRepository r) <- case patchfmt of
    PatchFormat1 -> return $ EmptyRepository $ mkRepoV1 rdir rfmt cache
    PatchFormat2 -> return $ EmptyRepository $ mkRepoV2 rdir rfmt cache
    PatchFormat3 -> return $ EmptyRepository $ mkRepoV3 rdir rfmt cache
  _ <- writePristine r emptyTree
  maybeCreatePatchIndex withPatchIndex r
  return repo

mkRepoV1
  :: AbsoluteOrRemotePath
  -> RepoFormat
  -> Cache
  -> Repository 'RO (RepoPatchV1 V1.Prim) Origin Origin
mkRepoV1 rdir repofmt cache = mkRepo rdir repofmt HashedPristine cache

mkRepoV2
  :: AbsoluteOrRemotePath
  -> RepoFormat
  -> Cache
  -> Repository 'RO (RepoPatchV2 V2.Prim) Origin Origin
mkRepoV2 rdir repofmt cache = mkRepo rdir repofmt HashedPristine cache

mkRepoV3
  :: AbsoluteOrRemotePath
  -> RepoFormat
  -> Cache
  -> Repository 'RO (RepoPatchV3 V2.Prim) Origin Origin
mkRepoV3 rdir repofmt cache = mkRepo rdir repofmt HashedPristine cache

createRepositoryV1
  :: WithWorkingDir -> WithPatchIndex -> UseCache -> WithPrefsTemplates
  -> IO (Repository 'RO (RepoPatchV1 V1.Prim) Origin Origin)
createRepositoryV1 withWorkingDir withPatchIndex useCache withPrefsTemplates = do
  rfmt <- createRepositoryFiles PatchFormat1 withWorkingDir withPrefsTemplates
  rdir <- ioAbsoluteOrRemote here
  cache <- getCaches useCache Nothing
  let repo = mkRepoV1 rdir rfmt cache
  _ <- writePristine repo emptyTree
  maybeCreatePatchIndex withPatchIndex repo
  return repo

createRepositoryV2
  :: WithWorkingDir -> WithPatchIndex -> UseCache -> WithPrefsTemplates
  -> IO (Repository 'RO (RepoPatchV2 V2.Prim) Origin Origin)
createRepositoryV2 withWorkingDir withPatchIndex useCache withPrefsTemplates = do
  rfmt <- createRepositoryFiles PatchFormat2 withWorkingDir withPrefsTemplates
  rdir <- ioAbsoluteOrRemote here
  cache <- getCaches useCache Nothing
  let repo = mkRepoV2 rdir rfmt cache
  _ <- writePristine repo emptyTree
  maybeCreatePatchIndex withPatchIndex repo
  return repo

maybeCreatePatchIndex :: (RepoPatch p, ApplyState p ~ Tree)
                      => WithPatchIndex -> Repository 'RO p wU Origin -> IO ()
maybeCreatePatchIndex NoPatchIndex _ = return ()
maybeCreatePatchIndex YesPatchIndex repo =
  createOrUpdatePatchIndexDisk repo emptyPatchSet

here :: String
here = "."
