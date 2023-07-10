--  Copyright (C) 2002-2003,2005 David Roundy
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

{-# LANGUAGE OverloadedStrings #-}

module Darcs.UI.Commands.MarkConflicts ( markconflicts ) where

import Darcs.Prelude

import System.Exit ( exitSuccess )
import Data.List.Ordered ( nubSort, isect )
import Control.Monad ( when, unless, void )

import Darcs.Util.SignalHandler ( withSignalsBlocked )
import Darcs.Util.Path ( AbsolutePath, AnchoredPath, anchorPath )
import Darcs.Util.Printer
    ( Doc, formatWords, pathlist, text, debugDocLn
    , vcat, vsep, (<+>), ($$) )

import Darcs.UI.Commands
    ( DarcsCommand(..)
    , withStdOpts
    , nodefaults
    , amInHashedRepository
    , putInfo
    , putFinished
    )
import Darcs.UI.Commands.Util ( filterExistingPaths )
import Darcs.UI.Completion ( knownFileArgs )
import Darcs.UI.Flags
    ( DarcsFlag, diffingOpts, verbosity, dryRun, umask
    , useCache, pathSetFromArgs )
import Darcs.UI.Options ( (^), (?) )
import qualified Darcs.UI.Options.All as O

import Darcs.Repository
    ( withRepoLock
    , RepoJob(..)
    , addToPending
    , finalizeRepositoryChanges
    , applyToWorking
    , readPatches
    , unrecordedChanges )

import Darcs.Patch ( invert, listTouchedFiles, effectOnPaths )
import Darcs.Patch.Show
import Darcs.Patch.TouchesFiles ( chooseTouching )
import Darcs.Patch.Witnesses.Ordered ( mapFL )
import Darcs.Patch.Witnesses.Sealed ( Sealed(Sealed) )
import Darcs.Repository.Resolution
    ( StandardResolution(..)
    , patchsetConflictResolutions
    , warnUnmangled
    )
import Darcs.Patch.Named ( anonymous )
import Darcs.Patch.PatchInfoAnd ( n2pia )
import Darcs.Patch.Set ( patchSetSnoc )

-- * The mark-conflicts command

markconflictsDescription :: String
markconflictsDescription =
 "Mark unresolved conflicts in working tree, for manual resolution."

markconflictsHelp :: Doc
markconflictsHelp = vsep $
  [ formatWords
    [ "Darcs requires human guidance to reconcile independent changes to the same"
    , "part of a file.  When a conflict first occurs, darcs will add the"
    , "initial state and all conflicting choices to the working tree, delimited"
    , " by the markers `v v v`, `=====`,  `* * *` and `^ ^ ^`, as follows:"
    ]
  , vcat $ map text
    [ "    v v v v v v v"
    , "    initial state"
    , "    ============="
    , "    first choice"
    , "    *************"
    , "    ...more choices..."
    , "    *************"
    , "    last choice"
    , "    ^ ^ ^ ^ ^ ^ ^"
    ]
  ] ++ map formatWords
  [ [ "However, you might revert or manually delete these markers without"
    , "actually resolving the conflict.  In this case, `darcs mark-conflicts`"
    , "is useful to re-create the conflict markup.  It is also"
    , "useful if `darcs apply` or `darcs pull` is used with"
    , "`--allow-conflicts`, where conflicts aren't marked initially."
    ]
  , [ "Note that in Darcs, a conflict counts as resolved when all of the changes"
    , "involved in the conflict (which can be more than two) are depended on by"
    , "one or more later patches. If you record a resolution for a particular"
    , "conflict, `darcs mark-conflicts` will no longer mark it, indicating that"
    , "it is resolved. This principle works also for explicit \"semantic\""
    , "dependencies. For instance, recording a tag will automatically mark all"
    , "conflicts as resolved."
    ]
  , [ "In the above schematic example the \"initial state\" corresponds to the"
    , "recorded state of the file in your repository. That is to say, the"
    , "recorded effect of a conflict is to apply none of the conflicting changes."
    , "This is usually not a state you would regard as a successful resolution"
    , "of the conflict; but there are exceptional situations where this may be"
    , "exactly what you want. In order to tell darcs that you want this conflict"
    , "to be regarded as resolved, use `record record --ask-deps` to record a"
    , "patch that explicitly depends on all patches involved in the conflict."
    ]
  , [ "Unless you use the `--dry-run` flag, any unrecorded changes to the"
    , "affected files WILL be lost forever when you run this command!"
    , "You will be prompted for confirmation before this takes place."
    ]
 ]

markconflicts :: DarcsCommand
markconflicts = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "mark-conflicts"
    , commandHelp = markconflictsHelp
    , commandDescription = markconflictsDescription
    , commandExtraArgs = -1
    , commandExtraArgHelp = ["[FILE or DIRECTORY]..."]
    , commandCommand = markconflictsCmd
    , commandPrereq = amInHashedRepository
    , commandCompleteArgs = knownFileArgs
    , commandArgdefaults = nodefaults
    , commandOptions = markconflictsOpts
    }
  where
    markconflictsBasicOpts
      = O.repoDir
      ^ O.diffAlgorithm
      ^ O.dryRunXml
    markconflictsAdvancedOpts = O.umask
    markconflictsOpts = markconflictsBasicOpts `withStdOpts` markconflictsAdvancedOpts

markconflictsCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
markconflictsCmd fps opts args = do
  paths <- maybeToOnly <$> pathSetFromArgs fps args
  debugDocLn $ "::: paths =" <+>  (text . show) paths
  withRepoLock (useCache ? opts) (umask ? opts) $
    RepoJob $ \_repository -> do

{-
    What we do here:
    * read the unrecorded changes (all of them)
    * extract functions representing path rename effects from unrecorded
    * convert argument paths to pre-pending
    * read conflict resolutions that touch pre-pending argument paths
    * affected paths = intersection of paths touched by resolutions
                       and pre-pending argument paths
    * apply the conflict resolutions for affected paths
-}

    classified_paths <-
      traverse
        (filterExistingPaths _repository (verbosity ? opts) (diffingOpts opts))
        paths

    unrecorded <-
      unrecordedChanges (diffingOpts opts) _repository (fromOnly Everything)
    anonpw <- n2pia `fmap` anonymous unrecorded

    let forward_renames = effectOnPaths unrecorded
        backward_renames = effectOnPaths (invert unrecorded)
        existing_paths = fmap snd classified_paths
        pre_pending_paths = fmap backward_renames existing_paths
    debugDocLn $ "::: pre_pending_paths =" <+> (text . show) pre_pending_paths

    r <- readPatches _repository
    -- by including anonpw in the patch set, we regard unrecorded changes
    -- as potential conflict resolutions "under construction"
    Sealed res <- case patchsetConflictResolutions $ patchSetSnoc r anonpw of
      conflicts -> do
        warnUnmangled (fromOnly pre_pending_paths) conflicts
        Sealed mangled_res <- return $ mangled conflicts
        let raw_res_paths = pathSet $ listTouchedFiles mangled_res
        debugDocLn $ "::: raw_res_paths =" <+>  (text . show) raw_res_paths
        return $ chooseTouching (fromOnly pre_pending_paths) mangled_res
    let res_paths = pathSet $ listTouchedFiles res
    debugDocLn $ "::: res_paths =" <+>  (text . show) res_paths

    let affected_paths = res_paths `isectPathSet` pre_pending_paths
    debugDocLn $ "::: affected_paths =" <+>  (text . show) affected_paths

    when (affected_paths == Only []) $ do
      putInfo opts "No conflicts to mark."
      exitSuccess

    let post_pending_affected_paths = forward_renames <$> affected_paths
    putInfo opts $ "Marking conflicts in:" <+> showPathSet post_pending_affected_paths <> "."

    debugDocLn $ "::: res = " $$ vsep (mapFL displayPatch res)
    when (O.yes (dryRun ? opts)) $ do
        putInfo opts $ "Conflicts will not be marked: this is a dry run."
        exitSuccess

    addToPending _repository (diffingOpts opts) res
    withSignalsBlocked $ do
      _repository <- finalizeRepositoryChanges _repository (O.dryRun ? opts)
      unless (O.yes (O.dryRun ? opts)) $
        void $ applyToWorking _repository (verbosity ? opts) res
    putFinished opts "marking conflicts"

-- * Generic 'PathSet' support

{- $SupportCode

What follows is generic support code for working with argument path lists
that are used to restrict operations to a subset of the working or pristine
tree. The rest of Darcs uses two types for this:

 * @'Maybe' ['SubPath']@

 * @'Maybe' ['FilePath']@

The problem with both is the contra-intuitive name 'Nothing', which here
stands for 'Everything'. To make the intended use clearer, we use the 'Only'
type instead (which is is isomorphic to 'Maybe') and the synonym 'PathSet'
defined below.

These abstractions should get their own module (or become integrated into
Darcs.Util.Path) if and when someone decides to reuse it elsewhere. The
functionality provided is intentionally minimal and light-weight.
-}

-- | 'Only' is isomorphic to 'Maybe' but with the opposite semantics.
--
-- About the name: I like the data constructor names, they are pretty
-- suggestive. The data type name is up for grabs; a possible alternative
-- is @AtMost@.
data Only a = Everything | Only a deriving (Eq, Ord, Show)

instance Functor Only where
  fmap _ Everything = Everything
  fmap f (Only x) = Only (f x)

instance Foldable Only where
  foldMap _ Everything = mempty
  foldMap f (Only x) = f x

instance Traversable Only where
  traverse _ Everything = pure Everything
  traverse f (Only x) = Only <$> f x

-- | This is mostly for conversion to legacy APIs
fromOnly :: Only a -> Maybe a
fromOnly Everything = Nothing
fromOnly (Only x) = Just x

maybeToOnly :: Maybe a -> Only a
maybeToOnly Nothing = Everything
maybeToOnly (Just x) = Only x

{- | A set of repository paths. 'Everything' means every path in the repo,
it usually originates from an empty list of path arguments. The list of
'AnchoredPath's is always kept in sorted order with no duplicates.

It uses lists because the number of elements is expected to be small.
-}
type PathSet a = Only [a]

-- | Intersection of two 'PathSet's
isectPathSet :: Ord a => PathSet a -> PathSet a -> PathSet a
isectPathSet Everything ys = ys
isectPathSet xs Everything = xs
isectPathSet (Only xs) (Only ys) = Only (isect xs ys)

{-
-- | Union of two 'PathSet's
union :: PathSet -> PathSet -> PathSet
union Everything ys = Everything
union xs Everything = Everything
union (Only xs) (Only ys) = Only (union xs ys)
-}

pathSet :: Ord a => [a] -> PathSet a
pathSet = Only . nubSort

-- | Convert a 'PathSet' to a 'Doc'. Uses the English module
-- to generate a nicely readable list of file names.
showPathSet :: PathSet AnchoredPath -> Doc
showPathSet Everything = text "all paths"
showPathSet (Only xs) = pathlist (map (anchorPath "") xs)
