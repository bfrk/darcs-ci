module Darcs.Repository.Working
    ( applyToWorking
    , setScriptsExecutable
    , setScriptsExecutablePatches
    , replaceWorking
    , notSoQuickApplyDiff
    ) where

import Control.Monad ( filterM, unless, when )
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC ( pack )
import System.Directory
    ( doesFileExist
    , removeDirectory
    , removeFile
    , withCurrentDirectory
    )
import System.IO.Error ( catchIOError )

import Darcs.Prelude

import Darcs.Patch ( PrimOf, RepoPatch, apply, listTouchedFiles )
import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch.Inspect ( PatchInspect )
import Darcs.Patch.Progress ( progressFL )
import Darcs.Patch.Witnesses.Ordered ( FL(..) )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..), unFreeLeft )

import Darcs.Repository.ApplyPatches ( runSilently, runTolerantly )
import Darcs.Repository.Diff ( treeDiff )
import Darcs.Repository.Flags ( DiffAlgorithm, UseIndex(..), Verbosity(..) )
import Darcs.Repository.Format ( RepoProperty(NoWorkingDir), formatHas )
import Darcs.Repository.InternalTypes
    ( AccessType(..)
    , Repository
    , repoFormat
    , repoLocation
    , unsafeCoerceU
    )
import Darcs.Repository.Prefs ( filetypeFunction )
import Darcs.Repository.State
    ( TreeFilter(..)
    , readPristine
    , readUnrecorded
    , readWorking
    )

import Darcs.Util.Path ( anchorPath, realPath )
import Darcs.Util.Progress ( debugMessage )
import Darcs.Util.Tree ( Tree, TreeItem(..), diffTrees, zipTrees )
import qualified Darcs.Util.Tree as Tree
import Darcs.Util.Tree.Plain ( writePlainFile, writePlainTree )
import Darcs.Util.Workaround ( setExecutable )

applyToWorking :: (ApplyState p ~ Tree, RepoPatch p)
               => Repository rt p wU wR
               -> Verbosity
               -> FL (PrimOf p) wU wY
               -> IO (Repository rt p wY wR)
applyToWorking repo verb ps =
  do
    unless (formatHas NoWorkingDir (repoFormat repo)) $ do
      debugMessage "Applying changes to working tree"
      withCurrentDirectory (repoLocation repo) $
        let ps' = progressFL "Applying patches to working" ps in
        if verb == Quiet
          then runSilently $ apply ps'
          else runTolerantly $ apply ps'
    return $ unsafeCoerceU repo
  `catchIOError` (\e -> fail $ "Error applying changes to working tree:\n" ++ show e)

-- | Change the working tree so that it coincides with pristine. Not quite as
-- cheap as 'switchBranch' but a lot cheaper than applying the patchset
-- difference between old and new repo state.
-- This operates on a repo that is 'RO' because it is executed outside the
-- transaction (like most operations on the working tree).
-- TODO Move this to D.R.State?
-- TODO Better get the trees from inside the transaction?
-- TODO Replace files and directories atomically?
replaceWorking
  :: (RepoPatch p, ApplyState p ~ Tree)
  => Repository 'RO p wU wR
  -> UseIndex
  -> IO (Repository 'RO p wU wR)
replaceWorking repo use_index = do
  tree_old <- readUnrecorded repo use_index Nothing
  tree_new <- readPristine repo
  quickApplyDiff tree_old tree_new
  return (unsafeCoerceU repo)

-- but makes it possible to try to carry unrecorded/unrevert/rebase
-- over to another branch (optional)
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

quickApplyDiff :: Tree IO -> Tree IO -> IO ()
quickApplyDiff tree_old tree_new =
    diffTrees tree_old tree_new >>= sequence_ . uncurry (zipTrees updateItem)
  where
    updateItem path Nothing (Just n) = writeItem (realPath path) n
    updateItem path (Just o) Nothing = removeItem (realPath path) o
    updateItem path (Just o) (Just n) = replaceItem (realPath path) o n
    updateItem _ Nothing Nothing = error "impossible case"
    removeItem path (SubTree _) = removeDirectory path
    removeItem path (File _) = removeFile path
    removeItem _ (Stub {}) = error "impossible case"
    writeItem path (SubTree t) = writePlainTree t path
    writeItem path (File b) = writePlainFile b path
    writeItem _ (Stub {}) = error "impossible case"
    replaceItem path (File _) (File b) = writePlainFile b path -- overwrite
    replaceItem _ (SubTree _) (SubTree _) = return () -- children already handled
    replaceItem path old new = removeItem path old >> writeItem path new

-- | Set the given paths executable if they are scripts.
--   A script is any file that starts with the bytes '#!'.
--   This is used for --set-scripts-executable.
setScriptsExecutable_ :: [FilePath] -> IO ()
setScriptsExecutable_ paths = do
    debugMessage "Making scripts executable"
    mapM_ setExecutableIfScript paths

setScriptsExecutable :: IO ()
setScriptsExecutable = do
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
