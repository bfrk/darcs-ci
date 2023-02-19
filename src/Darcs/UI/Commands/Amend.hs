--  Copyright (C) 2004,2007 David Roundy
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

-- |
-- Copyright   : 2004, 2007 David Roundy
-- License     : GPL
-- Maintainer  : darcs-devel@darcs.net
-- Stability   : experimental
-- Portability : portable

{-# LANGUAGE OverloadedStrings #-}
module Darcs.UI.Commands.Amend
    (
      amend
    , amendrecord
    ) where

import Darcs.Prelude

import Control.Monad ( unless )
import Data.Maybe ( isNothing, isJust )

import Darcs.UI.Commands
    ( DarcsCommand(..), withStdOpts
    , commandAlias
    , nodefaults
    , setEnvDarcsFiles
    , setEnvDarcsPatches
    , amInHashedRepository
    )
import Darcs.UI.Commands.Util
    ( announceFiles
    , historyEditHelp
    , testTentativeAndMaybeExit
    )
import Darcs.UI.Completion ( modifiedFileArgs, knownFileArgs )
import Darcs.UI.Flags ( diffingOpts, pathSetFromArgs )
import Darcs.UI.Options ( Config, (?), (^) )
import qualified Darcs.UI.Options.All as O
import Darcs.UI.PatchHeader
    ( AskAboutDeps(..)
    , HijackOptions(..)
    , patchHeaderConfig
    , runHijackT
    , updatePatchHeader
    )

import Darcs.Repository.Flags ( UpdatePending(..) )
import Darcs.Patch ( RepoPatch, description, PrimOf
                   , effect, invert, invertFL, canonizeFL
                   )
import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch.Depends ( contextPatches, patchSetUnion, findCommon )
import Darcs.Patch.Info ( isTag )
import Darcs.Patch.Named ( fmapFL_Named )
import Darcs.Patch.PatchInfoAnd ( hopefully )
import Darcs.Patch.Set ( Origin, PatchSet, appendPSFL )
import Darcs.Patch.Split ( primSplitter )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, info, patchDesc )
import Darcs.Patch.Rebase.Fixup ( RebaseFixup(..) )
import Darcs.Patch.Rebase.Name ( RebaseName(..) )
import Darcs.Util.Path ( AnchoredPath )
import Darcs.Repository
    ( Repository
    , AccessType(..)
    , withRepoLock
    , RepoJob(..)
    , identifyRepositoryFor
    , ReadingOrWriting(Reading)
    , tentativelyRemovePatches
    , tentativelyAddPatch
    , withManualRebaseUpdate
    , finalizeRepositoryChanges
    , readPendingAndWorking
    , readPristine
    , readPatches
    , tentativelyRemoveFromPW
    )
import Darcs.Repository.Prefs ( getDefaultRepo )
import Darcs.UI.SelectChanges
    ( WhichChanges(..)
    , selectionConfigPrim
    , runInvertibleSelection
    , withSelectedPatchFromList
    )
import qualified Darcs.UI.SelectChanges as S
    ( PatchSelectionOptions(..)
    )
import Darcs.Util.Exception ( clarifyErrors )
import Darcs.Patch.Witnesses.Ordered
    ( Fork(..), FL(..), RL, (:>)(..), (+>+)
    , nullFL, reverseRL, reverseFL, mapFL_FL
    )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..) )

import Darcs.Util.English ( anyOfClause, itemizeVertical )
import Darcs.Util.Printer ( Doc, formatWords, putDocLn, text, (<+>), ($$), ($+$) )
import Darcs.Util.Printer.Color ( ePutDocLn )
import Darcs.Util.Tree( Tree )


amendDescription :: String
amendDescription = "Improve a patch before it leaves your repository."


amendHelp :: Doc
amendHelp =
  formatWords
  [ "Amend updates a \"draft\" patch with additions or improvements,"
  , "resulting in a single \"finished\" patch."
  ]
  $+$ formatWords
  [ "By default `amend` proposes you to record additional changes."
  , "If instead you want to remove changes, use the flag `--unrecord`."
  ]
  $+$ formatWords
  [ "When recording a draft patch, it is a good idea to start the name with"
  , "`DRAFT:`. When done, remove it with `darcs amend --edit-long-comment`."
  , "Alternatively, to change the patch name without starting an editor, "
  , "use the `--name`/`-m` flag:"
  ]
  $+$ text
    "    darcs amend --match 'name \"DRAFT: foo\"' --name 'foo2'"
  $+$ formatWords
  [ "Like `darcs record`, if you call amend with files as arguments,"
  , "you will only be asked about changes to those files.  So to amend a"
  , "patch to foo.c with improvements in bar.c, you would run:"
  ]
  $+$ text
    "    darcs amend --match 'touch foo.c' bar.c"
  $+$ historyEditHelp

amend :: DarcsCommand
amend = DarcsCommand
    {
      commandProgramName          = "darcs"
    , commandName                 = "amend"
    , commandHelp                 = amendHelp
    , commandDescription          = amendDescription
    , commandExtraArgs            = -1
    , commandExtraArgHelp         = ["[FILE or DIRECTORY]..."]
    , commandCommand              = amendCmd
    , commandPrereq               = amInHashedRepository
    , commandCompleteArgs         = fileArgs
    , commandArgdefaults          = nodefaults
    , commandOptions              = allOpts
    }
  where
    fileArgs fps flags args =
      if (O.amendUnrecord ? flags)
        then knownFileArgs fps flags args
        else modifiedFileArgs fps flags args
    basicOpts
      = O.amendUnrecord
      ^ O.notInRemote
      ^ O.matchOneNontag
      ^ O.testChanges
      ^ O.interactive --True
      ^ O.author
      ^ O.selectAuthor
      ^ O.patchname
      ^ O.askDeps
      ^ O.askLongComment
      ^ O.keepDate
      ^ O.lookforadds
      ^ O.lookforreplaces
      ^ O.lookformoves
      ^ O.repoDir
      ^ O.withContext
      ^ O.diffAlgorithm
    advancedOpts
      = O.compress
      ^ O.umask
      ^ O.setScriptsExecutable
    allOpts = withStdOpts basicOpts advancedOpts
    amendCmd fps flags args = pathSetFromArgs fps args >>= doAmend flags

amendrecord :: DarcsCommand
amendrecord = commandAlias "amend-record" Nothing amend

doAmend :: Config -> Maybe [AnchoredPath] -> IO ()
doAmend cfg files =
  withRepoLock (O.useCache ? cfg) (O.umask ? cfg) $
      RebaseAwareJob $ \(repository :: Repository 'RW p wU wR) -> do
    patchSet <- readPatches repository
    common :> candidates <- filterNotInRemote cfg repository patchSet
    withSelectedPatchFromList "amend" candidates (patchSelOpts cfg) $
     \(kept :> oldp) -> do
      -- reconstruct context patchset
      context <- return $ appendPSFL common (reverseRL kept)
      announceFiles (O.verbosity ? cfg) files "Amending changes in"
      pristine <- readPristine repository
      pending :> working <-
        readPendingAndWorking (diffingOpts cfg) repository files
      -- auxiliary function needed because the witness types differ for the
      -- isTag case
      let go :: FL (PrimOf p) wR wU1 -> IO ()
          go NilFL | not (hasEditMetadata cfg) = putInfo cfg "No changes!"
          go ch = do
            let selection_config =
                   selectionConfigPrim First "record"
                       (patchSelOpts cfg)
                       (Just (primSplitter (O.diffAlgorithm ? cfg)))
                       files
                       (Just pristine)
            (chosenPatches :> _) <- runInvertibleSelection ch selection_config
            addChangesToPatch cfg repository context oldp chosenPatches pending working
      if not (isTag (info oldp))
        -- amending a normal patch
        then
          if O.amendUnrecord ? cfg
            then do
              let selection_config =
                    selectionConfigPrim Last "unrecord" (patchSelOpts cfg)
                      (Just (primSplitter (O.diffAlgorithm ? cfg)))
                      files (Just pristine)
              (_ :> chosenPrims) <-
                runInvertibleSelection (effect oldp) selection_config
              let invPrims = reverseRL (invertFL chosenPrims)
              addChangesToPatch cfg repository context oldp invPrims pending working
            else
              go (canonizeFL (O.diffAlgorithm ? cfg) (pending +>+ working))
        -- amending a tag
        else
          if hasEditMetadata cfg && isNothing files
            -- the user is not trying to add new changes to the tag so there is
            -- no reason to warn.
            then go NilFL
            -- the user is trying to add new changes to a tag.
            else do
              if hasEditMetadata cfg
                -- the user already knows that it is possible to edit tag metadata,
                -- note that s/he is providing editing options!
                then ePutDocLn "You cannot add new changes to a tag."
                -- the user may not be aware that s/he can edit tag metadata.
                else
                  ePutDocLn
                    "You cannot add new changes to a tag, but you are allowed to edit tag's metadata (see darcs help amend)."
              go NilFL


addChangesToPatch :: (RepoPatch p, ApplyState p ~ Tree)
                  => Config
                  -> Repository 'RW p wU wR
                  -> PatchSet p Origin wX -- ^ the context
                  -> PatchInfoAnd p wX wR -- ^ original patch
                  -> FL (PrimOf p) wR wY  -- ^ changes to add
                  -> FL (PrimOf p) wR wP  -- ^ pending
                  -> FL (PrimOf p) wP wU  -- ^ working
                  -> IO ()
addChangesToPatch cfg _repository context oldp chs pending working =
  if nullFL chs && not (hasEditMetadata cfg)
    then putInfo cfg "You don't want to record anything!"
    else do
      -- If a rebase is in progress, we want to manually update the rebase
      -- state, using the amendments directly as rebase fixups. This is
      -- necessary because otherwise we will first remove the original patch
      -- then add the amended patch,
      -- and this can lead to more conflicts than using the amendment as a fixup
      -- directly. For example, if a rename operation is amended in, the rename
      -- can be propagated to any edits to the file in the rebase state, whereas
      -- a delete then add would just cause a conflict.
      -- 
      -- We can also signal that any explicit dependencies of the old patch
      -- should be rewritten for the new patch using a 'NameFixup'.
      (_repository, (mlogf, newp)) <-
        withManualRebaseUpdate _repository $ \_repository -> do
          -- Note we pass NoUpdatePending here and below when re-adding the
          -- amended patch, and instead fix pending explicitly further below.
          _repository <-
            tentativelyRemovePatches
              _repository
              (O.compress ? cfg)
              NoUpdatePending
              (oldp :>: NilFL)
          (mlogf, newp) <-
            runHijackT AlwaysRequestHijackPermission $
            updatePatchHeader
              "amend"
              (if O.askDeps ? cfg
                 then AskAboutDeps context
                 else NoAskAboutDeps)
              (patchSelOpts cfg)
              (patchHeaderConfig cfg)
              (fmapFL_Named effect (hopefully oldp))
              chs
          let fixups =
                mapFL_FL PrimFixup (invert chs) +>+
                NameFixup (Rename (info newp) (info oldp)) :>:
                NilFL
          setEnvDarcsFiles newp
          _repository <-
            tentativelyAddPatch
              _repository
              (O.compress ? cfg)
              (O.verbosity ? cfg)
              NoUpdatePending
              newp
          return (_repository, fixups, (mlogf, newp))
      let failmsg = maybe "" (\lf -> "\nLogfile left in " ++ lf ++ ".") mlogf
      tp <- readPristine _repository
      testTentativeAndMaybeExit tp cfg
        ("you have a bad patch: '" ++ patchDesc newp ++ "'")
        "amend it"
        (Just failmsg)
      tentativelyRemoveFromPW _repository chs pending working
      _repository <-
        finalizeRepositoryChanges _repository YesUpdatePending (O.compress ? cfg)
          (O.dryRun ? cfg) `clarifyErrors` failmsg
      case O.verbosity ? cfg of
        O.NormalVerbosity -> putDocLn "Finished amending patch."
        O.Verbose -> putDocLn $ "Finished amending patch:" $$ description newp
        _ -> return ()
      setEnvDarcsPatches (newp :>: NilFL)

filterNotInRemote :: RepoPatch p
                  => Config
                  -> Repository 'RW p wU wR
                  -> PatchSet p Origin wR
                  -> IO ((PatchSet p :> RL (PatchInfoAnd p)) Origin wR)
filterNotInRemote cfg repository patchSet = do
    nirs <- mapM getNotInRemotePath (O.notInRemote ? cfg)
    if null nirs
      then
        -- We call contextPatches here because
        -- (a) selecting patches beyond the latest clean tag is impossible anyway
        -- (b) makes it easier to reconstruct a PatchSet w/o the selected patch
        -- (c) avoids listing the complete list of patches in the repo when user
        --     rejects the last selectable patch
        return (contextPatches patchSet)
      else do
        putInfo cfg $
          "Determining patches not in" <+> anyOfClause nirs $$ itemizeVertical 2 nirs
        Sealed thems <- patchSetUnion `fmap` mapM readNir nirs
        Fork in_remote only_ours _ <- return $ findCommon patchSet thems
        return (in_remote :> reverseFL only_ours)
  where
    readNir loc = do
      repo <- identifyRepositoryFor Reading repository (O.useCache ? cfg) loc
      rps <- readPatches repo
      return (Sealed rps)
    getNotInRemotePath (O.NotInRemotePath p) = return p
    getNotInRemotePath O.NotInDefaultRepo = do
        defaultRepo <- getDefaultRepo
        let err = fail $ "No default push/pull repo configured, please pass a "
                         ++ "repo name to --" ++ O.notInRemoteFlagName
        maybe err return defaultRepo

hasEditMetadata :: Config -> Bool
hasEditMetadata cfg = isJust (O.author ? cfg)
                    || O.selectAuthor ? cfg
                    || isJust (O.patchname ? cfg)
                    || O.askLongComment ? cfg == Just O.YesEditLongComment
                    || O.askLongComment ? cfg == Just O.PromptLongComment
                    || O.askDeps ? cfg

patchSelOpts :: Config -> S.PatchSelectionOptions
patchSelOpts cfg = S.PatchSelectionOptions
    { S.verbosity = O.verbosity ? cfg
    , S.matchFlags = O.matchOneNontag ? cfg
    , S.interactive = isInteractive cfg
    , S.selectDeps = O.PromptDeps -- option not supported, use default
    , S.withSummary = O.NoSummary -- option not supported, use default
    , S.withContext = O.withContext ? cfg
    }

isInteractive :: Config -> Bool
isInteractive cfg = maybe True id (O.interactive ? cfg)

putInfo :: Config -> Doc -> IO ()
putInfo cfg what = unless (O.verbosity ? cfg == O.Quiet) $ putDocLn what
