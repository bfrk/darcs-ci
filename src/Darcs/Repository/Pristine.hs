{-# LANGUAGE OverloadedStrings #-}
module Darcs.Repository.Pristine
    ( applyToTentativePristine
    , readHashedPristineRoot
    , pokePristineHash
    , peekPristineHash
    , createPristineDirectoryTree
    , readPristine
    , writePristine
    , convertSizePrefixedPristine
    ) where

import Darcs.Prelude

import Control.Exception ( catch, IOException, throwIO )

import System.Directory ( withCurrentDirectory )
import System.FilePath.Posix ( (</>) )
import System.IO ( hPutStrLn, stderr )
import System.IO.Error ( catchIOError )

import Darcs.Patch ( PatchInfoAnd, RepoPatch, description )
import Darcs.Patch.Apply ( Apply(..) )
import Darcs.Patch.Invertible ( Invertible )
import Darcs.Patch.Show ( ShowPatch )
import Darcs.Patch.Witnesses.Ordered ( FL )

import Darcs.Repository.Flags ( WithWorkingDir(..) )
import Darcs.Repository.Format ( RepoProperty(HashedInventory), formatHas )
import Darcs.Repository.Inventory
import Darcs.Repository.InternalTypes
    ( Repository
    , AccessType(..)
    , SAccessType(..)
    , repoAccessType
    , repoCache
    , repoFormat
    , repoLocation
    , withRepoDir
    )
import Darcs.Repository.Old ( oldRepoFailMsg )
import Darcs.Repository.Paths
    ( hashedInventoryPath
    , tentativePristinePath
    )

import Darcs.Util.ByteString ( gzReadFilePS )
import Darcs.Util.Cache ( Cache )
import Darcs.Util.Lock ( writeDocBinFile )
import Darcs.Util.Printer ( ($$), renderString, text )
import Darcs.Util.Tree ( Tree )
import Darcs.Util.Tree.Hashed
    ( darcsAddMissingHashes
    , darcsTreeHash
    , hashedTreeIO
    , readDarcsHashed
    , readDarcsHashedNosize
    , writeDarcsHashed
    )
import Darcs.Util.Tree.Plain ( writePlainTree )
import Darcs.Util.ValidHash ( fromHash, getSize )


-- | Apply a patch to the 'Tree' identified by the given root 'PristineHash',
-- then return the root hash of the result. The 'ApplyDir' argument says
-- whether to add or remove the changes. The 'Cache' argument specifies the
-- possible locations for hashed files.
applyToHashedPristine :: (Apply p, ApplyState p ~ Tree, ShowPatch p)
                      => Cache
                      -> PristineHash
                      -> p wX wY
                      -> IO PristineHash
applyToHashedPristine cache root patch = tryApply `catchIOError` annotateError
  where
    tryApply :: IO PristineHash
    tryApply = do
        -- Read a non-size-prefixed pristine, failing if we encounter one.
        tree <- readDarcsHashedNosize cache root
        (_, updatedTree) <- hashedTreeIO (apply patch) tree cache
        return $ fromHash $ darcsTreeHash updatedTree

    annotateError e =
      throwIO $
      userError $
      renderString $
      "Cannot apply patch to pristine:" $$ (description patch) $$
      "You may want to run 'darcs repair' on the repository containing this patch." $$
      "Reason: " <> text (show e)

convertSizePrefixedPristine :: Cache -> PristineHash -> IO PristineHash
convertSizePrefixedPristine cache ph = do
  case getSize ph of
    Nothing -> return ph
    Just _ -> do
      hPutStrLn stderr "Converting pristine..."
      -- Read the old size-prefixed pristine tree
      old <- readDarcsHashed cache ph
      -- Write out the pristine tree as a non-size-prefixed pristine
      -- and return the new root hash.
      writeDarcsHashed old cache

-- | Apply an 'FL' of 'Invertible' patches tentative pristine tree, and update
-- the tentative pristine hash. The patches need to be 'Invertible' so that we
-- can use it when removing patches from the repository, too.
applyToTentativePristine :: (ApplyState p ~ Tree, RepoPatch p)
                         => Repository 'RW p wU wR
                         -> Invertible (FL (PatchInfoAnd p)) wR wY
                         -> IO ()
applyToTentativePristine r p = do
    tentativePristine <- gzReadFilePS tentativePristinePath
    -- Extract the pristine hash from the tentativePristine file, using
    -- peekPristineHash (this is valid since we normally just extract the hash
    -- from the first line of an inventory file; we can pass in a one-line file
    -- that just contains said hash).
    let tentativePristineHash = peekPristineHash tentativePristine
    newPristineHash <- applyToHashedPristine (repoCache r) tentativePristineHash p
    writeDocBinFile tentativePristinePath $
        pokePristineHash newPristineHash tentativePristine

readHashedPristineRoot :: Repository rt p wU wR -> IO PristineHash
readHashedPristineRoot r =
  withRepoDir r $
    case repoAccessType r of
      SRO -> getHash hashedInventoryPath
      SRW -> getHash tentativePristinePath -- note the asymmetry!
  where
    getHash path =
      peekPristineHash <$>
        gzReadFilePS path `catch` (\(_ :: IOException) -> fail oldRepoFailMsg)

-- | Write the pristine tree into a plain directory at the given path.
createPristineDirectoryTree ::
     Repository rt p wU wR -> FilePath -> WithWorkingDir -> IO ()
createPristineDirectoryTree r _ NoWorkingDir = do
    tree <- readPristine r
    -- evaluate the tree to force copying of pristine files
    _ <- darcsAddMissingHashes tree
    return ()
createPristineDirectoryTree r dir WithWorkingDir = do
    tree <- readPristine r
    writePlainTree tree dir

-- | Obtains a Tree corresponding to the "recorded" state of the repository:
-- this is the same as the pristine cache, which is the same as the result of
-- applying all the repository's patches to an empty directory.
readPristine :: Repository rt p wU wR -> IO (Tree IO)
readPristine repo
  | formatHas HashedInventory (repoFormat repo) =
    case repoAccessType repo of
      SRO -> do
        inv <- gzReadFilePS $ repoLocation repo </> hashedInventoryPath
        let root = peekPristineHash inv
        readDarcsHashed (repoCache repo) root
      SRW -> do
        hash <-
          peekPristineHash <$>
            gzReadFilePS (repoLocation repo </> tentativePristinePath)
        readDarcsHashedNosize (repoCache repo) hash
  | otherwise = fail oldRepoFailMsg

-- | Replace the existing pristine with a new one (loaded up in a Tree object).
-- Warning: If @rt ~ 'RO@ this overwrites the recorded state, use only when
-- creating a new repo!
writePristine :: Repository rt p wU wR -> Tree IO -> IO PristineHash
writePristine repo tree =
  withCurrentDirectory (repoLocation repo) $ do
    tree' <- darcsAddMissingHashes tree
    root <- writeDarcsHashed tree' (repoCache repo)
    -- now update the current pristine hash
    case repoAccessType repo of
      SRO -> putHash root hashedInventoryPath
      SRW -> putHash root tentativePristinePath -- note the asymmetry!
  where
    putHash root path = do
      content <- gzReadFilePS path
      writeDocBinFile path $ pokePristineHash root content
      return root
