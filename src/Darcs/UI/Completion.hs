-- | How to complete arguments
{-# LANGUAGE NamedFieldPuns #-}
module Darcs.UI.Completion
    ( fileArgs
    , knownFileArgs
    , unknownFileArgs
    , modifiedFileArgs
    , noArgs
    , Pref(..) -- re-export
    , prefArgs
    ) where

import Darcs.Prelude

import Data.List ( (\\), stripPrefix )
import Data.List.Ordered ( nubSort, minus )
import Data.Maybe ( mapMaybe )

import Darcs.Patch ( listTouchedFiles )

import Darcs.Repository.Flags
    ( UseCache(..)
    )
import Darcs.Repository.Prefs
    ( Pref(..), getPreflist
    )
import Darcs.Repository.Job
    ( RepoJob(..)
    , withRepository
    )
import Darcs.Repository.State
    ( readPristineAndPending
    , readUnrecordedFiltered
    , unrecordedChanges
    , restrictDarcsdir
    , applyTreeFilter
    , TreeFilter(..)
    )

import Darcs.UI.Flags ( DarcsFlag )
import qualified Darcs.UI.Flags as Flags
import Darcs.UI.Options ( (?) )
import qualified Darcs.UI.Options.All as O

import Darcs.Util.File
    ( doesDirectoryReallyExist
    )
import Darcs.Util.Global
    ( darcsdir
    )
import Darcs.Util.Path
    ( AnchoredPath, realPath
    , AbsolutePath, toPath, floatSubPath, makeSubPathOf
    )
import Darcs.Util.Tree as Tree
    ( Tree, ItemType(..)
    , expand, expandPath, list, findTree, itemType, emptyTree
    )
import Darcs.Util.Tree.Plain ( readPlainTree )

-- | Return all files available under the original working
-- directory regardless of their repo state.
fileArgs :: (AbsolutePath, AbsolutePath)
         -> [DarcsFlag]
         -> [String]
         -> IO [FilePath]
fileArgs (_, orig) _flags args =
  notYetListed args $
  fmap (map anchoredToFilePath . listItems) $
  Tree.expand . applyTreeFilter restrictDarcsdir =<< readPlainTree (toPath orig)

-- | Return all files available under the original working directory that
-- are unknown to darcs but could be added.
unknownFileArgs :: (AbsolutePath, AbsolutePath)
                -> [DarcsFlag]
                -> [String]
                -> IO [FilePath]
unknownFileArgs fps flags args = notYetListed args $ do
  let lfa = if O.includeBoring ? flags then O.EvenLookForBoring else O.YesLookForAdds
      dopts = Flags.diffingOpts flags
  RepoTrees {have, known} <- repoTrees dopts {O.lookForAdds = lfa}
  known_paths <- listHere known fps
  have_paths <- listHere have fps
  return $
    map anchoredToFilePath $ nubSort have_paths `minus` nubSort known_paths

-- | Return all files available under the original working directory that
-- are known to darcs (either recorded or pending).
knownFileArgs :: (AbsolutePath, AbsolutePath)
              -> [DarcsFlag]
              -> [String]
              -> IO [FilePath]
knownFileArgs fps flags args = notYetListed args $ do
  RepoTrees {known} <- repoTrees (Flags.diffingOpts flags)
  map anchoredToFilePath <$> listHere known fps

-- | Return all files available under the original working directory that
-- are modified (relative to the recorded state).
modifiedFileArgs :: (AbsolutePath, AbsolutePath)
                 -> [DarcsFlag]
                 -> [String]
                 -> IO [FilePath]
modifiedFileArgs fps flags args = notYetListed args $ do
  RepoTrees {new} <- repoTrees (Flags.diffingOpts flags)
  case uncurry makeSubPathOf fps of
    Nothing -> return []
    Just here ->
      return $ mapMaybe (stripPathPrefix (toPath here)) $ map realPath new

-- | Return the available prefs of the given kind.
prefArgs :: Pref
         -> (AbsolutePath, AbsolutePath)
         -> [DarcsFlag]
         -> [String]
         -> IO [String]
prefArgs name _ _ _ = getPreflist name

-- | Return an empty list.
noArgs :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO [String]
noArgs _ _ _ = return []

-- * unexported helper functions

data RepoTrees m = RepoTrees
  { have  :: Tree m       -- ^ working tree
  , known :: Tree m       -- ^ recorded and pending
  , new :: [AnchoredPath] -- ^ unrecorded paths
  }

repoTrees :: O.DiffOpts -> IO (RepoTrees IO)
repoTrees dopts@O.DiffOpts {..} = do
  inDarcsRepo <- doesDirectoryReallyExist darcsdir
  if inDarcsRepo then
    withRepository NoUseCache $ RepoJob $ \r -> do
      known <- readPristineAndPending r
      have <- readUnrecordedFiltered r withIndex lookForAdds lookForMoves Nothing
      new <- listTouchedFiles <$> unrecordedChanges dopts r Nothing
      return $ RepoTrees {..}
  else
    return RepoTrees {have = emptyTree, known = emptyTree, new = []}

-- this is for completion which should give us everything under the original wd
subtreeHere :: Tree IO -> (AbsolutePath, AbsolutePath) -> IO (Maybe (Tree IO))
subtreeHere tree fps =
  case either error id . floatSubPath <$> uncurry makeSubPathOf fps of
    Nothing -> do
      return Nothing -- here is no subtree of the repo
    Just here -> do
      flip findTree here <$> expandPath tree here

listHere :: Tree IO
         -> (AbsolutePath, AbsolutePath)
         -> IO [(AnchoredPath, ItemType)]
listHere tree fps = do
  msubtree <- subtreeHere tree fps
  case msubtree of
    Nothing -> return []
    Just subtree -> listItems <$> expand subtree

listItems :: Tree m -> [(AnchoredPath, ItemType)]
listItems = map (\(p, i) -> (p, itemType i)) . Tree.list

anchoredToFilePath :: (AnchoredPath, ItemType) -> [Char]
anchoredToFilePath (path, _) = realPath path

stripPathPrefix :: FilePath -> FilePath -> Maybe FilePath
stripPathPrefix = stripPrefix . addSlash where
  addSlash [] = []
  addSlash xs = xs ++ "/"

-- | Turn an action that creates all possible completions into one
-- that removes already given arguments.
notYetListed :: [String] -> IO [String] -> IO [String]
notYetListed already complete = do
  possible <- complete
  return $ possible \\ already
