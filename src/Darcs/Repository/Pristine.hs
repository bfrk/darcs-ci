module Darcs.Repository.Pristine
    ( ApplyDir(..)
    , applyToHashedPristine
    , applyToTentativePristine
    , applyToTentativePristineCwd
    , readHashedPristineRoot
    , pokePristineHash
    , peekPristineHash
    , createPristineDirectoryTree
    , withRecorded
    , readPristine
    , readTentativePristine
    , cleanPristineDir
    ) where

import Darcs.Prelude

import Control.Arrow ( (&&&) )
import Control.Exception ( catch, IOException )
import Control.Monad ( when )

import qualified Data.ByteString.Char8 as BC ( unpack, pack )
import qualified Data.Set as Set

import System.Directory ( doesDirectoryExist, listDirectory )
import System.IO ( hPutStrLn, stderr )

import Darcs.Patch ( description )
import Darcs.Patch.Apply ( Apply(..) )
import Darcs.Patch.Show ( ShowPatch )

import Darcs.Repository.Flags ( Verbosity(..), WithWorkingDir(..) )
import Darcs.Repository.Format ( RepoProperty(HashedInventory), formatHas )
import Darcs.Repository.Inventory
import Darcs.Repository.InternalTypes
    ( Repository
    , repoCache
    , repoFormat
    , withRepoDir
    )
import Darcs.Repository.Old ( oldRepoFailMsg )
import Darcs.Repository.Paths
    ( hashedInventoryPath
    , tentativePristinePath
    , oldPristineDirPath
    , oldCurrentDirPath
    )

import Darcs.Util.ByteString ( gzReadFilePS )
import Darcs.Util.Cache
    ( Cache
    , HashedDir(HashedPristineDir)
    , cleanCachesWithHint
    , hashedDir
    , okayHash
    )
import Darcs.Util.Global ( darcsdir )
import Darcs.Util.Hash ( Hash(..), encodeBase16 )
import Darcs.Util.Lock ( removeFileMayNotExist, writeDocBinFile )
import Darcs.Util.Path ( AbsolutePath, toFilePath )
import Darcs.Util.Printer ( (<+>), putDocLn, text )
import Darcs.Util.Progress ( debugMessage )
import Darcs.Util.Tree ( Tree, treeHash )
import Darcs.Util.Tree.Hashed
    ( decodeDarcsHash
    , decodeDarcsSize
    , getHashedFiles
    , hashedTreeIO
    , readDarcsHashed
    , readDarcsHashedNosize
    , writeDarcsHashed
    )
import Darcs.Util.Tree.Plain ( readPlainTree, writePlainTree )


data ApplyDir = ApplyNormal | ApplyInverted

-- | 'applyToHashedPristine' takes a root hash, a patch @p@ and attempts to
-- apply the patch to the 'Tree' identified by @h@. If we encounter an old,
-- size-prefixed pristine, we first convert it to the non-size-prefixed format,
-- then apply the patch.
applyToHashedPristine :: (Apply p, ApplyState p ~ Tree)
                      => Cache
                      -> ApplyDir
                      -> PristineHash
                      -> p wX wY
                      -> IO PristineHash
applyToHashedPristine cache dir h p = applyOrConvertOldPristineAndApply
  where
    applyOrConvertOldPristineAndApply =
        tryApply hash `catch` \(_ :: IOException) -> handleOldPristineAndApply

    hash = decodeDarcsHash $ BC.pack $ getValidHash h

    failOnMalformedRoot (SHA256 _) = return ()
    failOnMalformedRoot root = fail $ "Cannot handle hash: " ++ show root

    hash2root = mkValidHash . BC.unpack . encodeBase16

    tryApply :: Hash -> IO PristineHash
    tryApply root = do
        failOnMalformedRoot root
        -- Read a non-size-prefixed pristine, failing if we encounter one.
        tree <- readDarcsHashedNosize cache root
        (_, updatedTree) <- case dir of
            ApplyNormal -> hashedTreeIO (apply p) tree cache
            ApplyInverted -> hashedTreeIO (unapply p) tree cache
        return $ hash2root $ treeHash updatedTree

    warn = "WARNING: Doing a one-time conversion of pristine format.\n"
           ++ "This may take a while. The new format is backwards-compatible."

    handleOldPristineAndApply = do
        hPutStrLn stderr warn
        inv <- gzReadFilePS hashedInventoryPath
        let oldroot = BC.pack $ getValidHash $ peekPristineHash inv
            oldrootSizeandHash = (decodeDarcsSize &&& decodeDarcsHash) oldroot
        -- Read the old size-prefixed pristine tree
        old <- readDarcsHashed cache oldrootSizeandHash
        -- Write out the pristine tree as a non-size-prefixed pristine.
        root <- writeDarcsHashed old cache
        let newroot = hash2root root
        -- Write out the new inventory.
        writeDocBinFile hashedInventoryPath $ pokePristineHash newroot inv
        cleanPristineDir cache [newroot]
        hPutStrLn stderr "Pristine conversion done..."
        -- Retry applying the patch, which should now succeed.
        tryApply root

-- |applyToTentativePristine applies a patch @p@ to the tentative pristine
-- tree, and updates the tentative pristine hash
applyToTentativePristine :: (ApplyState q ~ Tree, Apply q, ShowPatch q)
                         => Repository rt p wR wU wT
                         -> ApplyDir
                         -> Verbosity
                         -> q wT wY
                         -> IO ()
applyToTentativePristine r dir verb p =
  withRepoDir r $ do
    when (verb == Verbose) $
      putDocLn $ text "Applying to pristine..." <+> description p
    applyToTentativePristineCwd (repoCache r) dir p

applyToTentativePristineCwd :: (ApplyState p ~ Tree, Apply p)
                            => Cache
                            -> ApplyDir
                            -> p wX wY
                            -> IO ()
applyToTentativePristineCwd cache dir p = do
    tentativePristine <- gzReadFilePS tentativePristinePath
    -- Extract the pristine hash from the tentativePristine file, using
    -- peekPristineHash (this is valid since we normally just extract the hash
    -- from the first line of an inventory file; we can pass in a one-line file
    -- that just contains said hash).
    let tentativePristineHash = peekPristineHash tentativePristine
    newPristineHash <- applyToHashedPristine cache dir tentativePristineHash p
    writeDocBinFile tentativePristinePath $
        pokePristineHash newPristineHash tentativePristine

readTentativePristine :: Repository rt p wR wU wT -> IO (Tree IO)
readTentativePristine repo = do
    hash <- peekPristineHash <$> gzReadFilePS tentativePristinePath
    readDarcsHashedNosize (repoCache repo) $
      decodeDarcsHash $ BC.pack $ getValidHash hash

readHashedPristineRoot :: Repository rt p wR wU wT -> IO PristineHash
readHashedPristineRoot r = withRepoDir r $ do
    peekPristineHash <$>
      gzReadFilePS hashedInventoryPath
        `catch` (\(_ :: IOException) -> fail oldRepoFailMsg)

-- | Write the pristine tree into a plain directory at the given path.
createPristineDirectoryTree ::
     Repository rt p wR wU wT -> FilePath -> WithWorkingDir -> IO ()
createPristineDirectoryTree _ _ NoWorkingDir = return ()
createPristineDirectoryTree r dir WithWorkingDir = do
    tree <- readPristine r
    writePlainTree tree dir

withRecorded :: Repository rt p wR wU wT
             -> ((AbsolutePath -> IO a) -> IO a)
             -> (AbsolutePath -> IO a)
             -> IO a
withRecorded repository mk_dir f =
  mk_dir $ \d -> do
    createPristineDirectoryTree repository (toFilePath d) WithWorkingDir
    f d

-- | Obtains a Tree corresponding to the "recorded" state of the repository:
-- this is the same as the pristine cache, which is the same as the result of
-- applying all the repository's patches to an empty directory.
readPristine :: Repository rt p wR wU wT -> IO (Tree IO)
readPristine r
  | formatHas HashedInventory (repoFormat r) = do
      pris <- getValidHash <$> readHashedPristineRoot r
      let hash = decodeDarcsHash $ BC.pack pris
          size = decodeDarcsSize $ BC.pack pris
      when (hash == NoHash) $
          fail $ "Bad pristine root: " ++ pris ++
                 "\nYou should run darcs repair!"
      readDarcsHashed (repoCache r) (size, hash)
  | otherwise = do
      have_pristine <- doesDirectoryExist $ oldPristineDirPath
      have_current <- doesDirectoryExist $ oldCurrentDirPath
      case (have_pristine, have_current) of
        (True, _) -> readPlainTree $ oldPristineDirPath
        (False, True) -> readPlainTree $ oldCurrentDirPath
        (_, _) -> fail "No pristine tree is available!"

-- TODO clean this up!
cleanPristineDir :: Cache -> [PristineHash] -> IO ()
cleanPristineDir c hashroots =
   do let dir = HashedPristineDir
      -- we'll remove obsolete bits of "dir"
      debugMessage $ "Cleaning out " ++ hashedDir dir ++ "..."
      let hashdir = darcsdir ++ "/" ++ hashedDir dir ++ "/"
      hs <- set <$> getHashedFiles c (map getValidHash hashroots)
      fs <- set . filter okayHash <$> listDirectory hashdir
      mapM_ (removeFileMayNotExist . (hashdir++)) (unset $ fs `Set.difference` hs)
      -- and also clean out any global caches.
      debugMessage "Cleaning out any global caches..."
      cleanCachesWithHint c dir (unset $ fs `Set.difference` hs)
   where set = Set.fromList . map BC.pack
         unset = map BC.unpack . Set.toList
