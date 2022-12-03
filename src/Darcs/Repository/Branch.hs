module Darcs.Repository.Branch where

import Darcs.Prelude

import Control.Exception
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char ( isLetter )
import Data.List ( sort, (\\) )
import System.Directory
import System.FilePath.Posix
import System.IO.Error

import Darcs.Repository.InternalTypes
import Darcs.Repository.Inventory
import Darcs.Repository.Paths
import Darcs.Util.Cache
import Darcs.Util.Encoding
import Darcs.Util.Exception
import Darcs.Util.File ( copySubTree )
import Darcs.Util.Lock
import Darcs.Util.Parser
import Darcs.Util.Printer ( Doc, packedString, text, (<+>) )

-- Not yet implemented: push and pull between local (and remote?) branches
-- For remote branches: need to design UI for targeting a branch in a (remote) repo
-- For local branches just use the branch name

data Branch = Branch
  { pristineHash :: PristineHash
  , inventoryHash :: InventoryHash
  } deriving (Eq)

newtype BranchName = BranchName String
  deriving (Eq, Ord)

makeBranchName :: String -> IO BranchName
makeBranchName name
  | not (validBranchName name) =
      fail $ "Branch name '" ++ name ++ "' contains invalid characters. "
        ++ "Only digits, letters, '-', '_', and '.' are allowed."
  | otherwise = do
      checkValidFileName name >>= \case
        True -> return $ BranchName name
        False -> fail $ "Branch name'" ++ name ++ "' is not a valid file name."

validBranchName :: String -> Bool
validBranchName name = all allowed name
  where allowed c = isLetter c || c `elem` "-_."

showBranchName :: BranchName -> String
showBranchName (BranchName name) = name

defaultBranchName :: BranchName
defaultBranchName = BranchName "master"

branchesDir :: Repository rt p wU wR -> FilePath
branchesDir repo =
  case repoAccessType repo of
    SRO -> repoLocation repo </> branchesDirPath
    SRW -> repoLocation repo </> tentativeBranchesDirPath

listBranches :: Repository rt p wU wR -> IO [BranchName]
listBranches repo =
  ifIOError [] $
    map BranchName . sort . filter validBranchName <$>
      listDirectory (branchesDir repo)

revertDir :: FilePath -> FilePath -> IO ()
revertDir regular tentative = do
  removePathForcibly tentative
  copySubTree regular tentative

finalizeDir :: FilePath -> FilePath -> IO ()
finalizeDir tentative regular = do
  createDirectoryIfMissing False regular
  old_files <- sort <$> listDirectory regular
  new_files <- sort <$> listDirectory tentative
  forM_ (old_files \\ new_files) $ removeFile . (regular </>)
  forM_ new_files $ \filename ->
    renameFile (tentative </> filename) (regular </> filename)

revertTentativeBranch :: Repository 'RO p wU wR -> IO ()
revertTentativeBranch repo = do
  copyFile branchPath tentativeBranchPath
    `catchDoesNotExistError`
    -- we assume that revertTentativeChange has been called so we are
    -- justified assuming that the tentative files exist
    writeCurrentBranchName defaultBranchName
  revertDir branchesDirPath tentativeBranchesDirPath
    `catchDoesNotExistError`
    createDirectory tentativeBranchesDirPath
  readCurrentBranchName repo >>= updateBranch (unsafeStartTransaction repo)

finalizeTentativeBranch :: IO ()
finalizeTentativeBranch = do
  renameFile tentativeBranchPath branchPath
  finalizeDir tentativeBranchesDirPath branchesDirPath

-- | Forward compatibility: Check if hashed_inventory has been changed relative
-- to the current branch (i.e. by invocations of older darcs versions). If so,
-- update the current branch to the actual repo state.
updateBranch :: Repository 'RW p wU wR -> BranchName -> IO ()
updateBranch repo name = do
  actual_head <- getCurrent repo
  branch_head <- tryIOError (readBranch repo name)
  case branch_head of
    Right bh | bh == actual_head -> return ()
    _ -> writeBranch repo name actual_head

getCurrent :: Repository 'RW p wU wR -> IO Branch
getCurrent repo = do
  pristineHash <- peekPristineHash <$> readBinFile tentativePristinePath
  inventoryHash <- do
    inv <- readBinFile tentativeHashedInventoryPath
    writeFileUsingCache (repoCache repo) (skipPristineHash inv)
  return Branch{..}

renameBranch :: Repository 'RW p wU wR -> BranchName -> BranchName -> IO ()
renameBranch repo old new = do
  current <- readCurrentBranchName repo
  let old_path = branchesDir repo </> showBranchName old
      new_path = branchesDir repo </> showBranchName new
  eold <- doesFileExist old_path
  enew <- doesFileExist new_path
  unless eold $ fail $ "Branch does not exist: " ++ showBranchName old
  when enew $ fail $ "Branch already exists: " ++ showBranchName new
  renameFile old_path new_path
  when (current == old) $ writeCurrentBranchName new

readCurrentBranchName :: Repository rt p wU wR -> IO BranchName
readCurrentBranchName repo = do
  let path :: FilePath
      path =
        case repoAccessType repo of
          SRO -> branchPath
          SRW -> tentativeBranchPath
  try (readBinFile path) >>= \case
    Left e
      | isDoesNotExistError e -> return defaultBranchName
      | otherwise -> fail $ "Cannot read " ++ path ++ ": " ++ show e
    Right bs -> do
      name <- decodeUtf8 bs
      branch_name <- makeBranchName name
      let branch_path = branchesDir repo </> name
      doesFileExist branch_path >>= \case
        True -> return branch_name
        False -> fail $ "Current branch '" ++ name ++ "' does not exist."

writeCurrentBranchName :: BranchName -> IO ()
writeCurrentBranchName name =
  writeBinFile tentativeBranchPath =<< encodeUtf8 (showBranchName name)

-- | Switch to the branch with the given name. This updates the tentative
-- hashed inventory, pristine, and current branch. It does not touch the
-- working tree. Very cheap operation.
switchBranch
  :: Repository 'RW p wU wR
  -> BranchName
  -> IO (Repository 'RW p wU wB)
switchBranch repo name = do
  Branch pristine_hash_new inv_hash_new <- readBranch repo name
  -- update tentative head inventory; note that this is never compressed;
  -- the pristine hash will be prepended on finalize
  fetchFileUsingCache (repoCache repo) inv_hash_new >>=
    writeBinFile tentativeHashedInventoryPath . snd
  -- update tentative pristine
  writeDocBinFile tentativePristinePath $ pokePristineHash pristine_hash_new mempty
  -- update "current branch" info
  writeCurrentBranchName name
  return (unsafeCoerceR repo)

addBranch :: Repository 'RW p wU wR -> BranchName -> IO ()
addBranch repo name = do
  current <- readCurrentBranchName repo
  copyFile
    (branchesDir repo </> showBranchName current)
    (branchesDir repo </> showBranchName name)

removeBranch :: Repository 'RW p wU wR -> BranchName -> IO ()
removeBranch repo name = do
  current <- readCurrentBranchName repo
  let path = branchesDir repo </> showBranchName name
  when (current == name) $
    fail "Cannot remove the current branch."
  exists <- doesFileExist path
  unless exists $
    fail $ "No such branch: " ++ showBranchName name
  removeFile path

readBranch :: Repository rt p wU wR -> BranchName -> IO Branch
readBranch repo name = do
  content <-
    readBinFile (branchesDir repo </> showBranchName name)
  either fail return $  parseBranch content

writeBranch :: Repository 'RW p wU wR -> BranchName -> Branch -> IO ()
writeBranch repo name branch = do
  createDirectoryIfMissing False (branchesDir repo)
  writeDocBinFile (branchesDir repo </> showBranchName name) (showBranch branch)

getAllBranches :: Repository rt p wU wR -> IO [Branch]
getAllBranches repo = do
  branch_names <- listBranches repo
  forM branch_names $ readBranch repo

-- * Parsing

parseBranch :: B.ByteString -> Either String Branch
parseBranch = fmap fst . parse pBranch
  where
    pBranch = Branch <$> pPristineHash <*> pInventoryHash
    pInventoryHash = do
      string inventoryName
      skipSpace
      pHash

inventoryName :: B.ByteString
inventoryName = BC.pack "inventory:"

newline :: B.ByteString
newline = BC.pack "\n"

-- * Showing

showBranch :: Branch -> Doc
showBranch b =
    showHash pristineName (pristineHash b) <>
    showHash inventoryName (inventoryHash b)
  where
    showHash k v = packedString k <+> text (encodeValidHash v) <> packedString newline
