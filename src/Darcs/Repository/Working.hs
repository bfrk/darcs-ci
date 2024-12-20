module Darcs.Repository.Working
    ( applyToWorking
    , replaceWorking
    , setAllScriptsExecutable
    , setScriptsExecutablePatches
    , notSoQuickApplyDiff
    )  where

import Darcs.Prelude

import Control.Monad ( filterM, unless, void, when )
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC ( pack )
import qualified Data.ByteString.Lazy as BL
import Data.List ( sortBy )
import GHC.IO.Exception ( IOErrorType(..) )
import System.Directory
    ( createDirectory
    , createDirectoryIfMissing
    , doesFileExist
    , removeDirectory
    , removeFile
    , withCurrentDirectory
    )
import System.IO ( hPutStrLn, stderr )
import System.IO.Error ( catchIOError, ioeGetErrorType )

import Darcs.Patch ( PrimOf, RepoPatch, apply, showPatch, listTouchedFiles )
import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch.Inspect ( PatchInspect )
import Darcs.Patch.Progress ( progressFL )
import Darcs.Patch.Witnesses.Ordered ( FL(..) )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..), unFreeLeft )
import Darcs.Repository.ApplyPatches ( runSilently, runTolerantly )
import Darcs.Repository.Diff ( treeDiff )
import Darcs.Repository.Flags ( DiffAlgorithm, DiffOpts(..), Verbosity(..) )
import Darcs.Repository.Format ( RepoProperty(NoWorkingDir), formatHas )
import Darcs.Repository.InternalTypes
    ( AccessType(..)
    , Repository
    , repoFormat
    , repoLocation
    , unsafeCoerceU
    )
import Darcs.Repository.Prefs ( filetypeFunction )
import Darcs.Repository.State ( TreeFilter(..), readWorking )
import Darcs.Util.File ( backupByRenaming )
import Darcs.Util.Path ( anchorPath, realPath )
import Darcs.Util.Printer ( renderString )
import Darcs.Util.Progress ( debugMessage )
import Darcs.Util.Tree ( Tree, TreeItem(..), diffTrees, zipTrees )
import qualified Darcs.Util.Tree as Tree
import Darcs.Util.Tree.Diff ( TreeDiff(..), getTreeDiff, organise )
import Darcs.Util.Workaround ( setExecutable )

applyToWorking :: (ApplyState p ~ Tree, RepoPatch p)
               => Repository rt p wU wR
               -> Verbosity
               -> FL (PrimOf p) wU wY
               -> IO (Repository rt p wY wR)
applyToWorking repo verb ps =
  do
    unless (formatHas NoWorkingDir (repoFormat repo)) $ do
      debugMessage "Applying changes to working tree:"
      debugMessage $ renderString $ showPatch ps
      withCurrentDirectory (repoLocation repo) $
        let ps' = progressFL "Applying patches to working" ps in
        if verb == Quiet
          then runSilently $ apply ps'
          else runTolerantly $ apply ps'
    return $ unsafeCoerceU repo
  `catchIOError` (\e -> fail $ "Error applying changes to working tree:\n" ++ show e)

{-
-- | Replace the working tree with pristine. This reverts all unrecorded changes,
-- except for the addition of new files.
replaceWorking :: Repository rt p wU wR -> IO (Repository rt p wR wR)
replaceWorking r = do
    tree <- readPristine r
    withCurrentDirectory (repoLocation r) $ writeTree tree
    return (unsafeCoerceU r)

writeTree :: Tree IO -> IO ()
writeTree t = Tree.expand t >>= mapM_ write . Tree.list
  where
    write (p, File b) = writeBlob p b
    write (p, SubTree _) =
      runTolerantly $ do
        -- e <- mDoesDirectoryExist p
        -- unless e
        mCreateDirectory p
    write _ = return ()
    writeBlob p b = do
      runTolerantly $ do
        -- e <- mDoesFileExist p
        -- unless e $ 
      mCreateFile p
      content <- Tree.readBlob b
      runTolerantly $ mModifyFilePS p (\_ -> return (BL.toStrict content))
-}

-- | Change the working tree so that it coincides with pristine. Not quite as
-- cheap as 'switchBranch' but a lot cheaper than applying the patchset
-- difference between old and new repo state.
--
-- This is procedure has an ugly set of preconditions. It assumes that the
-- first tree corresponds to what we get when we call 'readUnrecorded' in
-- the unmodified repo. That is, it should represent the working state at
-- the start of the transaction, including pending changes, but not including
-- un-added files and directories. The second tree is supposed to be the
-- (possibly) modified pristine tree.
--
-- This operates on a repo that is 'RO' because it is executed outside the
-- transaction (like most operations on the working tree).
--
-- * TODO Move this to D.R.State?
-- * TODO Replace files and directories atomically?
replaceWorking
  :: Repository 'RO p wU wR
  -> DiffOpts
  -> Tree IO -- old working tree
  -> Tree IO -- new pristine tree
  -> IO (Repository 'RO p wR wR)
replaceWorking repo DiffOpts{} tree_old tree_new = do
  quickApplyDiff tree_old tree_new
  -- notSoQuickApplyDiff repo diffAlg tree_old tree_new
  return (unsafeCoerceU repo)

-- | Given an existing plain tree and another tree, update the first tree in
-- place to be identical to the second one.
--
-- The twist here is that the first tree may contain less items than we have on
-- disk due to un-added files and directories. We must backup these items
-- before overwriting them. Any files and directories already known to darcs
-- will be contained in the first tree; these are handled by merging unrecorded
-- changes.
quickApplyDiff :: Tree IO -> Tree IO -> IO ()
quickApplyDiff tree_old tree_new = do
    diffs <- diffTrees tree_old tree_new >>=
      return . sortBy organise . uncurry (zipTrees getTreeDiff)
    mapM_ (uncurry updateItem) diffs
  where
    updateItem p (Added n) = addItem (realPath p) n
    updateItem p (Removed o) = removeItem (realPath p) o
    updateItem p (Changed o n) = replaceItem (realPath p) o n

    -- backup un-added files before overwriting them
    addItem p (File b) = backupItem p >> writeBlob p b
    -- backup and then remove a file with the same name
    -- (existing directories need no backup, just the files in them)
    addItem p (SubTree _) = do
      e <- doesFileExist p
      when e $ backupItem p
      warning $ createDirectoryIfMissing False p
    addItem _ (Stub {}) = error "impossible case"

    removeItem p (SubTree _) =
      removeDirectory p `catchIOError` \e ->
        hPutStrLn stderr $
          case ioeGetErrorType e of
            UnsatisfiedConstraints ->
              "Warning: Not deleting " ++ p ++ " because" ++ " it is not empty."
            _ -> "Warning: Not deleting " ++ p ++ " because: " ++ show e
    removeItem p (File _) = warning $ removeFile p
    removeItem _ (Stub {}) = error "impossible case"

    replaceItem p (File _) (File b) = writeBlob p b -- overwrite old content
    replaceItem _ (SubTree _) (SubTree _) = return () -- children already handled
    -- in the "mixed" case we have to remove existing items first
    replaceItem p old new = removeItem p old >> writeItem p new

    writeItem p (SubTree _) = warning $ createDirectory p
    writeItem p (File b) = writeBlob p b
    writeItem _ (Stub {}) = error "impossible case"

    writeBlob p b = warning $ B.writeFile p . BL.toStrict =<< Tree.readBlob b

    backupItem p = backupByRenaming p
    handler e = hPutStrLn stderr $ "Warning: " ++ show e
    warning job = job `catchIOError` handler

notSoQuickApplyDiff
  :: (ApplyState p ~ Tree, RepoPatch p)
  => Repository 'RO p wU wR
  -> DiffAlgorithm
  -> Tree IO
  -> Tree IO
  -> IO ()
notSoQuickApplyDiff repo da tree_old tree_new = do
  ftf <- filetypeFunction
  Sealed diff <- unFreeLeft <$> treeDiff da ftf tree_old tree_new
  void $ applyToWorking repo NormalVerbosity diff

-- | Set the given paths executable if they are scripts.
--   A script is any file that starts with the bytes '#!'.
--   This is used for --set-scripts-executable.
setScriptsExecutable_ :: [FilePath] -> IO ()
setScriptsExecutable_ paths = do
    debugMessage "Making scripts executable"
    mapM_ setExecutableIfScript paths

setAllScriptsExecutable :: IO ()
setAllScriptsExecutable = do
    tree <- readWorking (TreeFilter id)
    setScriptsExecutable_ [anchorPath "." p | (p, Tree.File _) <- Tree.list tree]

setScriptsExecutablePatches :: PatchInspect p => p wX wY -> IO ()
setScriptsExecutablePatches pw = do
    paths <- filterM doesFileExist $ map (anchorPath ".") $ listTouchedFiles pw
    setScriptsExecutable_ paths

setExecutableIfScript :: FilePath -> IO ()
setExecutableIfScript f = do
    contents <- B.readFile f
    when (BC.pack "#!" `B.isPrefixOf` contents) $ do
        debugMessage ("Making executable: " ++ f)
        setExecutable f True
