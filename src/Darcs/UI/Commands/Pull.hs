--  Copyright (C) 2002-2005 David Roundy
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
module Darcs.UI.Commands.Pull ( -- * Commands.
                                pull, fetch,
                                pullCmd, StandardPatchApplier,
                                -- * Utility functions.
                                fetchPatches
                              ) where

import Darcs.Prelude

import System.Exit ( exitSuccess )
import Control.Monad ( when, unless, (>=>) )
import Data.List ( nub )
import Data.Maybe ( fromMaybe )
import Safe ( headErr, tailErr )

import Darcs.UI.Commands
    ( DarcsCommand(..)
    , withStdOpts
    , putInfo
    , putVerbose
    , setEnvDarcsPatches
    , defaultRepo
    , amInHashedRepository
    )
import Darcs.UI.Commands.Clone ( otherHelpInheritDefault )
import Darcs.UI.Flags
    ( DarcsFlag
    , fixUrl, getOutput
    , changesReverse, verbosity,  dryRun, umask, useCache, selectDeps
    , reorder, setDefault
    , hasXmlOutput
    , isInteractive, quiet
    )
import Darcs.UI.Options ( parseFlags, (?), (^) )
import qualified Darcs.UI.Options.All as O
import Darcs.Repository
    ( Repository
    , AccessType(..)
    , identifyRepositoryFor
    , ReadingOrWriting(..)
    , withRepoLock
    , RepoJob(..)
    , readPatches
    , modifyCache
    , mkCache
    , cacheEntries
    , CacheLoc(..)
    , WritableOrNot(..)
    , CacheType(..)
    , filterOutConflicts
    )

import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, info, hopefully, patchDesc )
import Darcs.Patch ( RepoPatch, description )
import qualified Darcs.Patch.Bundle as Bundle ( makeBundle )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Patch.Set ( PatchSet, Origin, emptyPatchSet, SealedPatchSet )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..), seal )
import Darcs.Patch.Witnesses.Ordered
    ( (:>)(..), FL(..), Fork(..)
    , mapFL, nullFL, mapFL_FL )
import Darcs.Patch.Permutations ( partitionFL )
import Darcs.Repository.Prefs
    ( Pref(Defaultrepo, Repos)
    , addRepoSource
    , addToPreflist
    , getPreflist
    , showMotd
    )
import Darcs.Patch.Depends
    ( findCommon
    , findCommonWithThem
    , patchSetIntersection
    , patchSetUnion
    )
import Darcs.UI.ApplyPatches ( PatchApplier(..), StandardPatchApplier(..) )
import Darcs.UI.Completion ( prefArgs )
import Darcs.UI.Commands.Util ( checkUnrelatedRepos, getUniqueDPatchName )
import Darcs.UI.SelectChanges
    ( WhichChanges(..)
    , runSelection
    , selectionConfig
    )
import qualified Darcs.UI.SelectChanges as S ( PatchSelectionOptions (..) )
import Darcs.Util.Printer
    ( Doc
    , ($$)
    , ($+$)
    , (<+>)
    , formatWords
    , hsep
    , putDoc
    , quoted
    , text
    , vcat
    )
import Darcs.Util.Lock ( writeDocBinFile )
import Darcs.Util.Path ( useAbsoluteOrStd, stdOut, AbsolutePath )
import Darcs.Util.Workaround ( getCurrentDirectory )
import Darcs.Util.Tree( Tree )

pullDescription :: String
pullDescription =
 "Copy and apply patches from another repository to this one."

fetchDescription :: String
fetchDescription =
 "Fetch patches from another repository, but don't apply them."

pullHelp :: Doc
pullHelp =
  formatWords
  [ "Pull is used to bring patches made in another repository into the current"
  , "repository (that is, either the one in the current directory, or the one"
  , "specified with the `--repodir` option). Pull accepts arguments, which are"
  , "URLs from which to pull, and when called without an argument, pull will"
  , "use the repository specified at `_darcs/prefs/defaultrepo`."
  ]
  $+$ formatWords
  [ "The default (`--union`) behavior is to pull any patches that are in any of"
  , "the specified repositories.  If you specify the `--intersection` flag, darcs"
  , "will only pull those patches which are present in all source repositories."
  , "If you specify the `--complement` flag, darcs will only pull elements in the"
  , "first repository that do not exist in any of the remaining repositories."
  ]
  $+$ formatWords
  [ "If `--reorder` is supplied, the set of patches that exist only in the current"
  , "repository is brought at the top of the current history. This will work even"
  , "if there are no new patches to pull."
  ]
  $+$ otherHelpInheritDefault
  $+$ formatWords
  [ "See `darcs help apply` for detailed description of many options."
  ]

fetchHelp :: Doc
fetchHelp =
  formatWords
  [ "Fetch is similar to `pull` except that it does not apply any patches"
  , "to the current repository. Instead, it generates a patch bundle that"
  , "you can apply later with `apply`."
  ]
  $+$ formatWords
  [ "Fetch's behaviour is essentially similar to pull's, so please consult"
  , "the help of `pull` to know more."
  ]

fetch :: DarcsCommand
fetch = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "fetch"
    , commandHelp = fetchHelp
    , commandDescription = fetchDescription
    , commandExtraArgs = -1
    , commandExtraArgHelp = ["[REPOSITORY]..."]
    , commandCommand = fetchCmd
    , commandPrereq = amInHashedRepository
    , commandCompleteArgs = prefArgs Repos
    , commandArgdefaults = defaultRepo
    , commandOptions = allOpts
    }
  where
    basicOpts
      = O.matchSeveral
      ^ O.interactive -- True
      ^ O.dryRun
      ^ O.withSummary
      ^ O.selectDeps
      ^ O.setDefault
      ^ O.inheritDefault
      ^ O.repoDir
      ^ O.output
      ^ O.allowUnrelatedRepos
      ^ O.diffAlgorithm
    advancedOpts
      = O.repoCombinator
      ^ O.remoteDarcs
    allOpts = basicOpts `withStdOpts` advancedOpts

pull :: DarcsCommand
pull = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "pull"
    , commandHelp = pullHelp
    , commandDescription = pullDescription
    , commandExtraArgs = -1
    , commandExtraArgHelp = ["[REPOSITORY]..."]
    , commandCommand = pullCmd StandardPatchApplier
    , commandPrereq = amInHashedRepository
    , commandCompleteArgs = prefArgs Repos
    , commandArgdefaults = defaultRepo
    , commandOptions = allOpts
    }
  where
    basicOpts
      = O.matchSeveral
      ^ O.reorder
      ^ O.interactive
      ^ O.conflictsYes
      ^ O.testChanges
      ^ O.dryRunXml
      ^ O.withSummary
      ^ O.selectDeps
      ^ O.setDefault
      ^ O.inheritDefault
      ^ O.repoDir
      ^ O.allowUnrelatedRepos
      ^ O.diffAlgorithm
    advancedOpts
      = O.repoCombinator
      ^ O.setScriptsExecutable
      ^ O.umask
      ^ O.changesReverse
      ^ O.pauseForGui
      ^ O.remoteDarcs
    allOpts = basicOpts `withStdOpts` advancedOpts

pullCmd
  :: PatchApplier pa
  => pa -> (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
pullCmd patchApplier (_,o) opts repos =
  do
    pullingFrom <- mapM (fixUrl o) repos
    withRepoLock (useCache ? opts) (umask ? opts) $
     repoJob patchApplier $ \patchProxy initRepo -> do
      let repository = modifyCache (addReposToCache pullingFrom) initRepo
      Sealed fork <- fetchPatches o opts repos "pull" repository
      applyPatches patchApplier patchProxy "pull" opts repository fork
    where
      addReposToCache repos' cache =
        mkCache $ [ toReadOnlyCache r | r <- repos' ] ++ cacheEntries cache
      toReadOnlyCache = Cache Repo NotWritable


fetchCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
fetchCmd (_,o) opts repos =
    withRepoLock (useCache ? opts) (umask ? opts) $ RepoJob $
        fetchPatches o opts repos "fetch" >=> makeBundle opts

fetchPatches :: (RepoPatch p, ApplyState p ~ Tree)
             => AbsolutePath -> [DarcsFlag] -> [String] -> String
             -> Repository 'RW p wU wR
             -> IO (Sealed (Fork (PatchSet p)
                                 (FL (PatchInfoAnd p))
                                 (FL (PatchInfoAnd p)) Origin wR))
fetchPatches o opts unfixedrepourls@(_:_) jobname repository = do
  here <- getCurrentDirectory
  repourls <- (nub . filter (/= here)) `fmap` mapM (fixUrl o) unfixedrepourls
  -- Test to make sure we aren't trying to pull from the current repo
  when (null repourls) $
        fail "Can't pull from current repository!"
  old_default <- getPreflist Defaultrepo
  when (old_default == repourls && not (hasXmlOutput opts)) $
      let pulling = case dryRun ? opts of
                      O.YesDryRun -> "Would pull"
                      O.NoDryRun -> "Pulling"
      in  putInfo opts $ text pulling <+> "from" <+> hsep (map quoted repourls) <> "..."
  (Sealed them, Sealed compl) <- readRepos repository opts repourls
  addRepoSource (headErr repourls) (dryRun ? opts)
      (setDefault False opts) (O.inheritDefault ? opts) (isInteractive True opts)
  mapM_ (addToPreflist Repos) repourls
  unless (quiet opts || hasXmlOutput opts) $ mapM_ showMotd repourls
  us <- readPatches repository
  checkUnrelatedRepos (parseFlags O.allowUnrelatedRepos opts) us them

  Fork common us' them' <- return $ findCommon us them
  _ :> compl' <- return $ findCommonWithThem compl us

  let avoided = mapFL info compl'
  ps :> _ <- return $ partitionFL (not . (`elem` avoided) . info) them'
  putVerbose opts $
    case us' of
      (x@(_ :>: _)) ->
        text "We have the following new (to them) patches:" $$
        vcat (mapFL description x)
      _ -> mempty
  unless (nullFL ps) $ putVerbose opts $
      text "They have the following patches to pull:" $$
      vcat (mapFL description ps)
  (hadConflicts, Sealed psFiltered)
    <- if O.conflictsYes ? opts == Nothing
        then filterOutConflicts repository (O.useIndex ? opts) us' ps
        else return (False, Sealed ps)
  when hadConflicts $ putInfo opts $ text "Skipping some patches which would cause conflicts."
  when (nullFL psFiltered) $ do putInfo opts $ text "No remote patches to pull in!"
                                setEnvDarcsPatches psFiltered
                                when (reorder ? opts /= O.Reorder) exitSuccess
  let direction = if changesReverse ? opts then FirstReversed else First
      selection_config = selectionConfig direction jobname (pullPatchSelOpts opts) Nothing Nothing
  (to_be_pulled :> _) <- runSelection psFiltered selection_config
  return (Sealed (Fork common us' to_be_pulled))

fetchPatches _ _ [] jobname _ = fail $
  "No default repository to " ++ jobname ++ " from, please specify one"

makeBundle :: forall p wR . (RepoPatch p, ApplyState p ~ Tree)
           => [DarcsFlag]
           -> (Sealed (Fork (PatchSet p)
                      (FL (PatchInfoAnd p))
                      (FL (PatchInfoAnd p)) Origin wR))
           -> IO ()
makeBundle opts (Sealed (Fork common _ to_be_fetched)) =
    do
      bundle <- Bundle.makeBundle Nothing common $
                 mapFL_FL hopefully to_be_fetched
      let fname = case to_be_fetched of
                    (x:>:_)-> getUniqueDPatchName $ patchDesc x
                    _ -> error "impossible case"
      o <- fromMaybe (return stdOut) (getOutput opts fname)
      useAbsoluteOrStd writeDocBinFile putDoc o bundle

{- | Read in the specified pull-from repositories, and depending on whether
to perform intersection, union, or complement, return two 'PatchSet's: the
patches we want to pull and the ones we do not want to pull. In set-algebra
terms (using + for union, & for intersection):

[union]
    > (R1 + R2 + ... + Rn, {})
[intersection]
    > (R1 & R2 & ... & Rn, {})
[complement]
    > (R1, R2 + R3 + ... + Rn)

-}
readRepos :: RepoPatch p
          => Repository rt p wU wR -> [DarcsFlag] -> [String]
          -> IO (SealedPatchSet p Origin,SealedPatchSet p Origin)
readRepos _ _ [] = error "impossible case"
readRepos to_repo opts us =
    do rs <- mapM (\u -> do r <- identifyRepositoryFor Reading to_repo (useCache ? opts) u
                            ps <- readPatches r
                            return $ seal ps) us
       return $ case parseFlags O.repoCombinator opts of
                  O.Intersection -> (patchSetIntersection rs, seal emptyPatchSet)
                  O.Complement -> (headErr rs, patchSetUnion $ tailErr rs)
                  O.Union -> (patchSetUnion rs, seal emptyPatchSet)

pullPatchSelOpts :: [DarcsFlag] -> S.PatchSelectionOptions
pullPatchSelOpts flags = S.PatchSelectionOptions
    { S.verbosity = verbosity ? flags
    , S.matchFlags = parseFlags O.matchSeveral flags
    , S.interactive = isInteractive True flags
    , S.selectDeps = selectDeps ? flags
    , S.withSummary = O.withSummary ? flags
    }
