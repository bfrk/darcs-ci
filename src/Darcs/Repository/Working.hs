module Darcs.Repository.Working
    ( applyToWorking
    , setAllScriptsExecutable
    , setScriptsExecutablePatches
    )  where

import Control.Monad ( when, unless, filterM )
import System.Directory ( doesFileExist, withCurrentDirectory )
import System.IO.Error ( catchIOError )

import qualified Data.ByteString as B ( readFile
                                      , isPrefixOf
                                      )
import qualified Data.ByteString.Char8 as BC (pack)

import Darcs.Prelude

import Darcs.Util.Progress ( debugMessage )
import Darcs.Util.Workaround ( setExecutable )
import Darcs.Util.Tree ( Tree )
import Darcs.Util.Path ( anchorPath )
import qualified Darcs.Util.Tree as Tree

import Darcs.Patch ( RepoPatch, PrimOf, apply, listTouchedFiles )
import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch.Witnesses.Ordered
    ( FL(..) )
import Darcs.Patch.Inspect ( PatchInspect )
import Darcs.Patch.Progress ( progressFL )

import Darcs.Repository.Format ( RepoProperty( NoWorkingDir ), formatHas )
import Darcs.Repository.Flags  ( Verbosity(..) )
import Darcs.Repository.InternalTypes
    ( Repository
    , repoFormat
    , repoLocation
    , unsafeCoerceU )
import Darcs.Repository.ApplyPatches ( runTolerantly, runSilently )
import Darcs.Repository.State ( readWorking, TreeFilter(..)  )

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
