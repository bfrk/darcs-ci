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

{-# LANGUAGE OverloadedStrings #-}
module Darcs.UI.Commands.Push ( push ) where

import Darcs.Prelude

import System.Exit ( exitWith, ExitCode( ExitSuccess, ExitFailure ), exitSuccess )
import Control.Monad ( when, unless )
import Data.Maybe ( isJust )
import Darcs.UI.Commands
    ( DarcsCommand(..), withStdOpts
    , putVerbose
    , putInfo
    , putFinished
    , abortRun
    , setEnvDarcsPatches
    , defaultRepo
    , amInHashedRepository
    )
import Darcs.UI.Commands.Clone ( otherHelpInheritDefault )
import Darcs.UI.Commands.Util ( printDryRunMessageAndExit, checkUnrelatedRepos )
import Darcs.UI.Completion ( prefArgs )
import Darcs.UI.Flags
    ( DarcsFlag
    , isInteractive, verbosity
    , xmlOutput, selectDeps, applyAs
    , changesReverse, dryRun, useCache, setDefault, fixUrl )
import Darcs.UI.Options ( (?), (^) )
import qualified Darcs.UI.Options.All as O
import Darcs.Repository.Flags ( DryRun (..) )
import qualified Darcs.Repository.Flags as R ( remoteDarcs )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, hopefully )
import Darcs.Repository
    ( RepoJob(..)
    , Repository
    , identifyRepositoryFor
    , ReadingOrWriting(..)
    , readPatches
    , withRepository
    )
import Darcs.Patch ( RepoPatch, description )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Patch.Witnesses.Ordered
    ( (:>)(..), RL, FL, nullRL,
    nullFL, reverseFL, mapFL_FL, mapRL )
import Darcs.Repository.Prefs
    ( Pref(Defaultrepo, Repos)
    , addRepoSource
    , getPreflist
    )
import Darcs.UI.External ( signString, darcsProgram
                         , pipeDoc, pipeDocSSH )
import Darcs.Util.Exception ( die )
import Darcs.Util.URL ( isHttpUrl, isValidLocalPath
                      , isSshUrl, splitSshUrl, SshFilePath(..) )
import Darcs.Util.Path ( AbsolutePath )
import Darcs.UI.SelectChanges
    ( WhichChanges(..)
    , selectionConfig
    , runSelection
    )
import qualified Darcs.UI.SelectChanges as S ( PatchSelectionOptions (..) )
import Darcs.Patch.Depends ( findCommonWithThem, countUsThem )
import Darcs.Patch.Bundle ( makeBundle )
import Darcs.Patch.Show( ShowPatch )
import Darcs.Patch.Set ( PatchSet, Origin )
import Darcs.Util.Printer.Color ( ePutDocLn )
import Darcs.Util.Printer
    ( Doc
    , ($$)
    , ($+$)
    , (<+>)
    , empty
    , formatWords
    , quoted
    , text
    , vcat
    )
import Darcs.UI.Email ( makeEmail )
import Darcs.Util.English (englishNum, Noun(..))
import Darcs.Util.Workaround ( getCurrentDirectory )
import Darcs.Util.Tree( Tree )


pushDescription :: String
pushDescription =
 "Copy and apply patches from this repository to another one."

pushHelp :: Doc
pushHelp =
  formatWords
    [ "Push is the opposite of pull.  Push allows you to copy patches from the"
    , "current repository into another repository."
    ]
  $+$ formatWords
    [ "The --reorder-patches option works in the same way as it does for pull"
    , "and apply: instead of placing the new patches (coming from your local"
    , "repository) on top of (i.e. after) the existing (remote) ones, it puts"
    , "the remote-only patches on top of the ones that you are pushing. This"
    , "can be useful, for instance, if you have recorded a tag locally and want"
    , "this tag to be clean in the remote repository after pushing."
    ]
  $+$ formatWords
    [ "If you give the `--apply-as` flag, darcs will use `sudo` to apply the"
    , "patches as a different user.  This can be useful if you want to set up a"
    , "system where several users can modify the same repository, but you don't"
    , "want to allow them full write access.  This isn't secure against skilled"
    , "malicious attackers, but at least can protect your repository from clumsy,"
    , "inept or lazy users."
    ]
  $+$ formatWords
    [ "`darcs push` will compress the patch data before sending it to a remote"
    , "location via ssh. This works as long as the remote darcs is not older"
    , "than version 2.5. If you get errors that indicate a corrupt patch bundle,"
    , "you should try again with the `--no-compress` option."
    ]
  $+$
  otherHelpInheritDefault

push :: DarcsCommand
push = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "push"
    , commandHelp = pushHelp
    , commandDescription = pushDescription
    , commandExtraArgs = -1
    , commandExtraArgHelp = ["[REPOSITORY]"]
    , commandCommand = pushCmd
    , commandPrereq = amInHashedRepository
    , commandCompleteArgs = prefArgs Repos
    , commandArgdefaults = defaultRepo
    , commandOptions = pushOpts
    }
  where
    pushBasicOpts
      = O.matchSeveral
      ^ O.selectDeps
      ^ O.interactive
      ^ O.sign
      ^ O.dryRunXml
      ^ O.withSummary
      ^ O.repoDir
      ^ O.setDefault
      ^ O.inheritDefault
      ^ O.allowUnrelatedRepos
      ^ O.reorderPush
    pushAdvancedOpts
      = O.applyAs
      ^ O.changesReverse
      ^ O.compress
      ^ O.remoteDarcs
    pushOpts = pushBasicOpts `withStdOpts` pushAdvancedOpts

pushCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
pushCmd (_, o) opts [unfixedrepodir] = do
  repodir <- fixUrl o unfixedrepodir
  here <- getCurrentDirectory
  checkOptionsSanity opts repodir
  -- make sure we aren't trying to push to the current repo
  when (repodir == here) $ die "Cannot push from repository to itself."
  bundle <-
    withRepository (useCache ? opts) $ RepoJob $ prepareBundle opts repodir
  sbundle <- signString (O.sign ? opts) bundle
  let body =
        if isValidLocalPath repodir
          then sbundle
          else makeEmail repodir [] Nothing Nothing sbundle Nothing
  rval <- remoteApply opts repodir body
  case rval of
    ExitFailure ec -> do
      ePutDocLn (text "Apply failed!")
      exitWith (ExitFailure ec)
    ExitSuccess -> putFinished opts "pushing"
pushCmd _ _ [] = die "No default repository to push to, please specify one."
pushCmd _ _ _ = die "Cannot push to more than one repo."

prepareBundle :: (RepoPatch p, ApplyState p ~ Tree)
              => [DarcsFlag] -> String -> Repository rt p wU wR -> IO Doc
prepareBundle opts repodir repository = do
  old_default <- getPreflist Defaultrepo
  when (old_default == [repodir]) $
       let pushing = if dryRun ? opts == YesDryRun then "Would push" else "Pushing"
       in  putInfo opts $ text pushing <+> "to" <+> quoted repodir <> "..."
  them <- identifyRepositoryFor Writing repository (useCache ? opts) repodir >>= readPatches
  addRepoSource repodir (dryRun ? opts) (setDefault False opts)
      (O.inheritDefault ? opts)
  us <- readPatches repository
  common :> only_us <- return $ findCommonWithThem us them
  prePushChatter opts us (reverseFL only_us) them
  let direction = if changesReverse ? opts then FirstReversed else First
      selection_config = selectionConfig direction "push" (pushPatchSelOpts opts) Nothing Nothing
  runSelection only_us selection_config
                   >>= bundlePatches opts common

prePushChatter :: (RepoPatch p, ShowPatch a)
               => [DarcsFlag] -> PatchSet p Origin wX
               -> RL a wC wX -> PatchSet p Origin wY -> IO ()
prePushChatter opts us only_us them = do
  checkUnrelatedRepos (O.allowUnrelatedRepos ? opts) us them
  let num_to_pull = snd $ countUsThem us them
      pull_reminder = if num_to_pull > 0
                      then text $ "The remote repository has " ++ show num_to_pull
                      ++ " " ++ englishNum num_to_pull (Noun "patch") " to pull."
                      else empty
  putVerbose opts $ text "We have the following patches to push:" $$ vcat (mapRL description only_us)
  unless (nullRL only_us) $ putInfo opts pull_reminder
  when (nullRL only_us) $ do
      putInfo opts $ text "No recorded local patches to push!"
      exitSuccess

bundlePatches :: (RepoPatch p, ApplyState p ~ Tree)
              => [DarcsFlag] -> PatchSet p wA wZ
              -> (FL (PatchInfoAnd p) :> t) wZ wW
              -> IO Doc
bundlePatches opts common (to_be_pushed :> _) =
    do
      setEnvDarcsPatches to_be_pushed
      printDryRunMessageAndExit "push"
        (verbosity ? opts)
        (O.withSummary ? opts)
        (dryRun ? opts)
        (xmlOutput ? opts)
        (isInteractive True opts)
        to_be_pushed
      when (nullFL to_be_pushed) $ do
          putInfo opts $
            text "You don't want to push any patches, and that's fine with me!"
          exitSuccess
      makeBundle Nothing common (mapFL_FL hopefully to_be_pushed)

checkOptionsSanity :: [DarcsFlag] -> String -> IO ()
checkOptionsSanity opts repodir =
  if isHttpUrl repodir then do
       when (isJust $ applyAs ? opts) $
           abortRun opts $ text "Cannot --apply-as when pushing to URLs"
       let lprot = takeWhile (/= ':') repodir
           msg = text ("Pushing to "++lprot++" URLs is not supported.")
       abortRun opts msg
   else when (O.sign ? opts /= O.NoSign) $
        abortRun opts $ text "Signing doesn't make sense for local repositories or when pushing over ssh."


pushPatchSelOpts :: [DarcsFlag] -> S.PatchSelectionOptions
pushPatchSelOpts flags = S.PatchSelectionOptions
    { S.verbosity = verbosity ? flags
    , S.matchFlags = O.matchSeveral ? flags
    , S.interactive = isInteractive True flags
    , S.selectDeps = selectDeps ? flags
    , S.withSummary = O.withSummary ? flags
    }

remoteApply :: [DarcsFlag] -> String -> Doc -> IO ExitCode
remoteApply opts repodir bundle
    = case applyAs ? opts of
        Nothing
            | isSshUrl repodir -> applyViaSsh opts (splitSshUrl repodir) bundle
            | otherwise -> applyViaLocal opts repodir bundle
        Just un
            | isSshUrl repodir -> applyViaSshAndSudo opts (splitSshUrl repodir) un bundle
            | otherwise -> applyViaSudo opts un repodir bundle

applyViaSudo :: [DarcsFlag] -> String -> String -> Doc -> IO ExitCode
applyViaSudo opts user repo bundle =
  darcsProgram >>= \darcs ->
    pipeDoc "sudo" ("-u" : user : darcs : darcsArgs opts repo) bundle

applyViaLocal :: [DarcsFlag] -> String -> Doc -> IO ExitCode
applyViaLocal opts repo bundle =
  darcsProgram >>= \darcs -> pipeDoc darcs (darcsArgs opts repo) bundle

applyViaSsh :: [DarcsFlag] -> SshFilePath -> Doc -> IO ExitCode
applyViaSsh opts repo =
  pipeDocSSH
    (O.compress ? opts)
    repo
    [ unwords $
        R.remoteDarcs (O.remoteDarcs ? opts) :
        darcsArgs opts (shellQuote (sshRepo repo))
    ]

applyViaSshAndSudo :: [DarcsFlag] -> SshFilePath -> String -> Doc -> IO ExitCode
applyViaSshAndSudo opts repo username =
  pipeDocSSH
    (O.compress ? opts)
    repo
    [ unwords $
        "sudo" : "-u" : username :
        R.remoteDarcs (O.remoteDarcs ? opts) :
        darcsArgs opts (shellQuote (sshRepo repo))
    ]

darcsArgs :: [DarcsFlag] -> String -> [String]
darcsArgs opts repodir = "apply" : standardFlags ++ reorderFlags ++ debugFlags
  where
    standardFlags = ["--all", "--repodir", repodir]
    reorderFlags = if O.yes (O.reorderPush ? opts) then ["--reorder-patches"] else []
    debugFlags = if O.debug ? opts then ["--debug"] else []

shellQuote :: String -> String
shellQuote s = "'" ++ escapeQuote s ++ "'"
  where
    escapeQuote [] = []
    escapeQuote cs@('\'':_) = '\\' : escapeQuote cs
    escapeQuote (c:cs) = c : escapeQuote cs
