--  Copyright (C) 2002-2003 David Roundy
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

module Darcs.UI.Commands.Record
    ( record
    , commit
    ) where

import Darcs.Prelude

import Control.Exception ( handleJust )
import Control.Monad ( unless, void, when )
import Data.Char ( ord )
import Data.Foldable ( traverse_ )
import System.Directory ( removeFile )
import System.Exit ( ExitCode(..), exitFailure, exitSuccess )

import Darcs.Patch ( PrimOf, RepoPatch, summaryFL )
import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch.Info ( PatchInfo, patchinfo )
import Darcs.Patch.Named ( adddeps, infopatch )
import Darcs.Patch.PatchInfoAnd ( n2pia )
import Darcs.Patch.Progress ( progressFL )
import Darcs.Patch.Split ( primSplitter )
import Darcs.Patch.Witnesses.Ordered ( (:>)(..), FL(..), nullFL )
import Darcs.Repository
    ( RepoJob(..)
    , Repository
    , AccessType(..)
    , finalizeRepositoryChanges
    , readPristine
    , readPatches
    , tentativelyAddPatch
    , tentativelyRemoveFromPending
    , unrecordedChanges
    , withRepoLock
    )
import Darcs.Repository.Flags ( UpdatePending(..) )
import Darcs.UI.Commands
    ( DarcsCommand(..)
    , amInHashedRepository
    , commandAlias
    , nodefaults
    , setEnvDarcsFiles
    , setEnvDarcsPatches
    , withStdOpts
    )
import Darcs.UI.Commands.Util
    ( announceFiles
    , filterExistingPaths
    , testTentativeAndMaybeExit
    )
import Darcs.UI.Completion ( modifiedFileArgs )
import Darcs.UI.Flags
    ( DarcsFlag
    , diffingOpts
    , fileHelpAuthor
    , getAuthor
    , getDate
    , pathSetFromArgs
    )
import Darcs.UI.Options ( Config, (?), (^) )
import qualified Darcs.UI.Options.All as O
import Darcs.UI.PatchHeader ( getLog )
import Darcs.UI.SelectChanges
    ( WhichChanges(..)
    , askAboutDepends
    , runInvertibleSelection
    , selectionConfigPrim
    )
import qualified Darcs.UI.SelectChanges as S ( PatchSelectionOptions(..) )
import Darcs.Util.Exception ( clarifyErrors )
import Darcs.Util.Global ( darcsLastMessage )
import Darcs.Util.Path ( AbsolutePath, AnchoredPath, displayPath )
import Darcs.Util.Printer
    ( Doc
    , formatWords
    , pathlist
    , putDocLn
    , text
    , vcat
    , vsep
    , ($+$)
    , (<+>)
    )
import Darcs.Util.Progress ( debugMessage )
import Darcs.Util.Prompt ( promptYorn )
import Darcs.Util.Tree ( Tree )

recordHelp :: Doc
recordHelp =
  vsep (map formatWords
  [ [ "The `darcs record` command is used to create a patch from changes in"
    , "the working tree.  If you specify a set of files and directories,"
    , "changes to other files will be skipped."
    ]
  , [ "Every patch has a name, an optional description, an author and a date."
    ]
  , [ "Darcs will launch a text editor (see `darcs help environment`) after the"
    , "interactive selection, to let you enter the patch name (first line) and"
    , "the patch description (subsequent lines)."
    ]
  , [ "You can supply the patch name in advance with the `-m` option, in which"
    , "case no text editor is launched, unless you use `--edit-long-comment`."
    ]
  , [ "The patch description is an optional block of free-form text.  It is"
    , "used to supply additional information that doesn't fit in the patch"
    , "name.  For example, it might include a rationale of WHY the change was"
    , "necessary."
    ]
  , [ "A technical difference between patch name and patch description, is"
    , "that matching with the flag `-p` is only done on patch names."
    ]
  , [ "Finally, the `--logfile` option allows you to supply a file that already"
    , "contains the patch name and description.  This is useful if a previous"
    , "record failed and left a `_darcs/patch_description.txt` file."
    ]
  , fileHelpAuthor
  , [ "If you want to manually define any explicit dependencies for your patch,"
    , "you can use the `--ask-deps` flag. Some dependencies may be automatically"
    , "inferred from the patch's content and cannot be removed. A patch with"
    , "specific dependencies can be empty."
    ]
  , [ "The patch date is generated automatically.  It can only be spoofed by"
    , "using the `--pipe` option."
    ]
  , [ "If you run record with the `--pipe` option, you will be prompted for"
    , "the patch date, author, and the long comment. The long comment will extend"
    , "until the end of file or stdin is reached. This interface is intended for"
    , "scripting darcs, in particular for writing repository conversion scripts."
    , "The prompts are intended mostly as a useful guide (since scripts won't"
    , "need them), to help you understand the input format. Here's an example of"
    , "what the `--pipe` prompts look like:"
    ]
  ])
  $+$ vcat
    [ "    What is the date? Mon Nov 15 13:38:01 EST 2004"
    , "    Who is the author? David Roundy"
    , "    What is the log? One or more comment lines"
    ]
  $+$ vsep (map formatWords
  [ [ "If a test command has been defined with `darcs setpref`, attempting to"
    , "record a patch will cause the test command to be run in a clean copy"
    , "of the working tree (that is, including only recorded changes).  If"
    , "the test fails, you will be offered to abort the record operation."
    ]
  , [ "The `--set-scripts-executable` option causes scripts to be made"
    , "executable in the clean copy of the working tree, prior to running the"
    , "test.  See `darcs clone` for an explanation of the script heuristic."
    ]
  , [ "If your test command is tediously slow (e.g. `make all`) and you are"
    , "recording several patches in a row, you may wish to use `--no-test` to"
    , "skip all but the final test."
    ]
  , [ "To see some context (unchanged lines) around each change, use the"
    , "`--unified` option."
    ]
  ])

record :: DarcsCommand
record = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "record"
    , commandHelp = recordHelp
    , commandDescription = "Create a patch from unrecorded changes."
    , commandExtraArgs = -1
    , commandExtraArgHelp = ["[FILE or DIRECTORY]..."]
    , commandCommand = recordCmd
    , commandPrereq = amInHashedRepository
    , commandCompleteArgs = modifiedFileArgs
    , commandArgdefaults = nodefaults
    , commandOptions = allOpts
    }
  where
    basicOpts
      = O.patchname
      ^ O.author
      ^ O.testChanges
      ^ O.interactive
      ^ O.pipe
      ^ O.askDeps
      ^ O.askLongComment
      ^ O.lookforadds
      ^ O.lookforreplaces
      ^ O.lookformoves
      ^ O.repoDir
      ^ O.diffAlgorithm
    advancedOpts
      = O.logfile
      ^ O.umask
      ^ O.setScriptsExecutable
    allOpts = basicOpts `withStdOpts` advancedOpts

-- | commit is an alias for record
commit :: DarcsCommand
commit = commandAlias "commit" Nothing record

reportNonExisting :: O.LookForAdds -> ([AnchoredPath], [AnchoredPath]) -> IO ()
reportNonExisting lfa (paths_only_in_working, _) = do
  unless (lfa /= O.NoLookForAdds || null paths_only_in_working) $  putDocLn $
    "These paths are not yet in the repository and will be added:" <+>
    pathlist (map displayPath paths_only_in_working)

recordCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
recordCmd fps cfg args = do
    checkNameIsNotOption (O.patchname ? cfg) (isInteractive cfg)
    withRepoLock (O.useCache ? cfg) (O.umask ? cfg) $ RepoJob $ \(repository :: Repository 'RW p wU wR) -> do
      existing_files <- do
        files <- pathSetFromArgs fps args
        files' <-
          traverse
            (filterExistingPaths repository (O.verbosity ? cfg) (diffingOpts cfg)) files
        when (O.verbosity ? cfg /= O.Quiet) $
            traverse_ (reportNonExisting (O.lookforadds ? cfg)) files'
        let files'' = fmap snd files'
        when (files'' == Just []) $
            fail "None of the files you specified exist."
        return files''
      announceFiles (O.verbosity ? cfg) existing_files "Recording changes in"
      debugMessage "About to get the unrecorded changes."
      unrecorded <- unrecordedChanges (diffingOpts cfg) repository existing_files
      case unrecorded of
          NilFL | not (O.askDeps ? cfg) -> do
              -- We need to grab any input waiting for us, since we
              -- might break scripts expecting to send it to us; we
              -- don't care what that input is, though.
              void (getDate (O.pipe ? cfg))
              putStrLn "No changes!"
              exitFailure
          _ -> doRecord repository cfg existing_files unrecorded

-- | Check user specified patch name is not accidentally a command line flag
checkNameIsNotOption :: Maybe String -> Bool -> IO ()
checkNameIsNotOption Nothing     _      = return ()
checkNameIsNotOption _           False  = return ()
checkNameIsNotOption (Just name) True   =
    when (length name == 1 || (length name == 2 && head name == '-')) $ do
        confirmed <- promptYorn $ "You specified " ++ show name ++ " as the patch name. Is that really what you want?"
        unless confirmed $ putStrLn "Okay, aborting the record." >> exitFailure

doRecord :: (RepoPatch p, ApplyState p ~ Tree)
         => Repository 'RW p wU wR -> Config -> Maybe [AnchoredPath]
         -> FL (PrimOf p) wR wU -> IO ()
doRecord repository cfg files unrecorded = do
    debugMessage "I've got unrecorded changes."
    date <- getDate (O.pipe ? cfg)
    my_author <- getAuthor (O.author ? cfg) (O.pipe ? cfg)
    debugMessage "I'm slurping the repository."
    debugMessage "About to select changes..."
    (chs :> _ ) <- runInvertibleSelection unrecorded $
                      selectionConfigPrim
                          First "record" (patchSelOpts cfg)
                          (Just (primSplitter (O.diffAlgorithm ? cfg)))
                          files
    when (not (O.askDeps ? cfg) && nullFL chs) $
              do putStrLn "Ok, if you don't want to record anything, that's fine!"
                 exitSuccess
    handleJust onlySuccessfulExits (\_ -> return ()) $
             do deps <- if O.askDeps ? cfg
                        then do
                          patches <- readPatches repository
                          askAboutDepends patches chs (patchSelOpts cfg) []
                        else return []
                when (O.askDeps ? cfg) $ debugMessage "I've asked about dependencies."
                if nullFL chs && null deps
                  then putStrLn "Ok, if you don't want to record anything, that's fine!"
                  else do setEnvDarcsFiles chs
                          (name, my_log, logf) <-
                            getLog (O.patchname ? cfg) (O.pipe ? cfg)
                              (O.logfile ? cfg) (O.askLongComment ? cfg)
                              Nothing (summaryFL chs)
                          debugMessage ("Patch name as received from getLog: " ++ show (map ord name))
                          doActualRecord repository cfg name date my_author my_log logf deps chs

doActualRecord :: (RepoPatch p, ApplyState p ~ Tree)
               => Repository 'RW p wU wR
               -> Config
               -> String -> String -> String
               -> [String] -> Maybe String
               -> [PatchInfo] -> FL (PrimOf p) wR wX
               -> IO ()
doActualRecord _repository cfg name date my_author my_log logf deps chs = do
    debugMessage "Writing the patch file..."
    myinfo <- patchinfo date name my_author my_log
    let mypatch = infopatch myinfo $ progressFL "Writing changes:" chs
    let pia = n2pia $ adddeps mypatch deps
    _repository <-
      tentativelyAddPatch _repository (O.verbosity ? cfg)
        NoUpdatePending pia
    tp <- readPristine _repository
    testTentativeAndMaybeExit tp cfg
      ("you have a bad patch: '" ++ name ++ "'")
      "record it" (Just failuremessage)
    tentativelyRemoveFromPending _repository chs
    _repository <-
      finalizeRepositoryChanges _repository YesUpdatePending (O.dryRun ? cfg)
      `clarifyErrors` failuremessage
    debugMessage "Syncing timestamps..."
    removeLogFile logf
    unless (O.verbosity ? cfg == O.Quiet) $
      putDocLn $ text $ "Finished recording patch '" ++ name ++ "'"
    setEnvDarcsPatches (pia :>: NilFL)
  where
    removeLogFile :: Maybe String -> IO ()
    removeLogFile Nothing = return ()
    removeLogFile (Just lf)
      | lf == darcsLastMessage = return ()
      | otherwise = removeFile lf
    failuremessage =
      "Failed to record patch '" ++ name ++ "'" ++
        case logf of
          Just lf -> "\nLogfile left in " ++ lf ++ "."
          Nothing -> ""

onlySuccessfulExits :: ExitCode -> Maybe ()
onlySuccessfulExits ExitSuccess = Just ()
onlySuccessfulExits _ = Nothing

patchSelOpts :: Config -> S.PatchSelectionOptions
patchSelOpts cfg = S.PatchSelectionOptions
    { S.verbosity = O.verbosity ? cfg
    , S.matchFlags = []
    , S.interactive = isInteractive cfg
    , S.selectDeps = O.PromptDeps -- option not supported, use default
    , S.withSummary = O.NoSummary -- option not supported, use default
    }

isInteractive :: Config -> Bool
isInteractive cfg = maybe True id (O.interactive ? cfg)
