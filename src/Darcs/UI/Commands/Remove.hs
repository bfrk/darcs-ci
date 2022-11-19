--  Copyright (C) 2002-2004 David Roundy
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2, or (at your option)
--  any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; see the file COPYING.  If not, write to
--  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
--  Boston, MA 02110-1301, USA.

module Darcs.UI.Commands.Remove ( remove, rm, unadd ) where

import Darcs.Prelude

import Control.Monad ( when, foldM, void )
import Darcs.UI.Commands
    ( DarcsCommand(..)
    , withStdOpts, nodefaults
    , commandAlias, commandStub
    , putWarning, putInfo
    , amInHashedRepository
    )
import Darcs.UI.Completion ( knownFileArgs )
import Darcs.UI.Flags
    ( DarcsFlag, diffingOpts
    , useCache, umask, diffAlgorithm, pathsFromArgs )
import Darcs.UI.Options ( (^), parseFlags, (?) )
import qualified Darcs.UI.Options.All as O

import Darcs.Repository
    ( Repository
    , withRepoLock
    , RepoJob(..)
    , UpdatePending(..)
    , addToPending
    , finalizeRepositoryChanges
    , readPristine
    , readPristineAndPending
    , readUnrecorded
    )
import Darcs.Repository.Diff( treeDiff )

import Darcs.Patch ( RepoPatch, PrimOf, PrimPatch, listTouchedFiles )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Patch.Witnesses.Ordered ( FL(..), concatGapsFL, nullFL )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..), FreeLeft, unFreeLeft )
import Darcs.Repository.Prefs ( filetypeFunction, FileType )
import Darcs.Util.Tree( Tree, TreeItem(..), explodePaths )
import qualified Darcs.Util.Tree as T ( find, modifyTree, expand, list )
import Darcs.Util.Path( AnchoredPath, displayPath, isRoot, AbsolutePath )
import Darcs.Util.Printer ( Doc, text, vcat )

removeDescription :: String
removeDescription = "Remove files from version control."

removeHelp :: Doc
removeHelp = text $
 "The `darcs remove` command exists primarily for symmetry with `darcs\n" ++
 "add`, as the normal way to remove a file from version control is\n" ++
 "simply to delete it from the working tree.  This command is only\n" ++
 "useful in the unusual case where one wants to record a removal patch\n" ++
 "WITHOUT deleting the copy in the working tree (which can be re-added).\n" ++
 "\n" ++
 "Note that applying a removal patch to a repository (e.g. by pulling\n" ++
 "the patch) will ALWAYS affect the working tree of that repository.\n"

remove :: DarcsCommand
remove = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "remove"
    , commandHelp = removeHelp
    , commandDescription = removeDescription
    , commandExtraArgs = -1
    , commandExtraArgHelp = ["<FILE or DIRECTORY> ..."]
    , commandCommand = removeCmd
    , commandPrereq = amInHashedRepository
    , commandCompleteArgs = knownFileArgs
    , commandArgdefaults = nodefaults
    , commandOptions = removeOpts
    }
  where
    removeBasicOpts = O.repoDir ^ O.recursive
    removeAdvancedOpts = O.umask
    removeOpts = removeBasicOpts `withStdOpts` removeAdvancedOpts

removeCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
removeCmd fps opts relargs = do
    when (null relargs) $
        fail "Nothing specified, nothing removed."
    paths <- pathsFromArgs fps relargs
    when (any isRoot paths) $
        fail "Cannot remove a repository's root directory!"
    withRepoLock (useCache ? opts) (umask ? opts) $
      RepoJob $ \repository -> do
        recorded_and_pending <- readPristineAndPending repository
        let exploded_paths =
              (if parseFlags O.recursive opts
                then reverse . explodePaths recorded_and_pending
                else id) paths
        Sealed p <- makeRemovePatch opts repository exploded_paths
        when (nullFL p && not (null paths)) $
            fail "No files were removed."
        addToPending repository (diffingOpts opts) p
        void $ finalizeRepositoryChanges repository YesUpdatePending
            (O.compress ? opts) (O.dryRun ? opts)
        putInfo opts $ vcat $ map text $ ["Will stop tracking:"] ++
            map displayPath (listTouchedFiles p)

-- | makeRemovePatch builds a list of patches to remove the given filepaths.
--   This function does not recursively process directories. The 'Recursive'
--   flag should be handled by the caller by adding all offspring of a directory
--   to the files list.
makeRemovePatch :: (RepoPatch p, ApplyState p ~ Tree)
                => [DarcsFlag] -> Repository rt p wU wR
                -> [AnchoredPath] -> IO (Sealed (FL (PrimOf p) wU))
makeRemovePatch opts repository files = do
  recorded <- T.expand =<< readPristine repository
  pending <- T.expand =<< readPristineAndPending repository
  unrecorded <- readUnrecorded repository (O.useIndex ? opts) $ Just files
  ftf <- filetypeFunction
  result <- foldM removeOnePath (ftf, recorded, pending, unrecorded, []) files
  case result of
    (_, _, _, _, patches) ->
      return $
      unFreeLeft $ concatGapsFL $ reverse patches
  where
    removeOnePath (ftf, recorded, pending, unrecorded, patches) f = do
      let pending' = T.modifyTree pending f Nothing
          unrecorded' = T.modifyTree unrecorded f Nothing
      local <-
        makeRemoveGap opts ftf recorded pending pending' unrecorded unrecorded' f
      -- we can tell if the remove succeeded by looking if local is
      -- empty. If the remove succeeded, we should pass on updated
      -- pending and unrecorded that reflect the removal
      return $
        case local of
          Just gap -> (ftf, recorded, pending', unrecorded', gap : patches)
          _ -> (ftf, recorded, pending, unrecorded, patches)

-- | Return the FL of patches to remove the item at the given path, wrapped in
-- a 'Gap'. Returns 'Nothing' in case the path cannot be removed (if it is not
-- tracked (i.e. in the pending state), or if it's a directory and is not empty).
makeRemoveGap
  :: PrimPatch prim
  => [DarcsFlag]
  -> (FilePath -> FileType)
  -> Tree IO      -- ^ recorded state
  -> Tree IO      -- ^ pending state
  -> Tree IO      -- ^ pending state with item removed
  -> Tree IO      -- ^ unrecorded state
  -> Tree IO      -- ^ unrecorded state with item removed
  -> AnchoredPath -- ^ path of item
  -> IO (Maybe (FreeLeft (FL prim)))
makeRemoveGap opts ftf recorded pending pending' unrecorded unrecorded' path =
    case (T.find recorded path, T.find pending path, T.find unrecorded path) of
        (_, Just (SubTree _), Just (SubTree unrecordedChildren))
            | not $ null (T.list unrecordedChildren) ->
              skipAndWarn "it is not empty"
        (Just _, _, Nothing) ->
            -- item was already removed in the working tree
            -- (may or may not be removed in pending)
            -- FIXME The result here, if non-empty, is in the wrong context,
            -- since it applies after pending and not after working, as
            -- expected by addToPending. Concretely, the item exists after
            -- pending, but was removed in the working tree, so applying the
            -- remove after working is wrong. It is unclear to me why this
            -- still seems to work. Anyway, the previous solution is not
            -- better: it adds the file before removing it, which is also
            -- incorrect (the addfile should be right after pending, the
            -- remove should be after working.
            Just <$> treeDiff (diffAlgorithm ? opts) ftf pending pending'
        (_, Just _, Just _) ->
            -- item is present in both pending and working
            Just <$> treeDiff (diffAlgorithm ? opts) ftf unrecorded unrecorded'
        _ -> skipAndWarn "it is not tracked by darcs"
  where skipAndWarn reason =
            do putWarning opts . text $ "Can't remove " ++ displayPath path
                                        ++ " (" ++ reason ++ ")"
               return Nothing


rmDescription :: String
rmDescription = "Help newbies find `darcs remove'."

rmHelp :: Doc
rmHelp = text $
 "The `darcs rm' command does nothing.\n" ++
 "\n" ++
 "The normal way to remove a file from version control is simply to\n" ++
 "delete it from the working tree.  To remove a file from version\n" ++
 "control WITHOUT affecting the working tree, see `darcs remove'.\n"

rm :: DarcsCommand
rm = commandStub "rm" rmHelp rmDescription remove

unadd :: DarcsCommand
unadd = commandAlias "unadd" Nothing remove
