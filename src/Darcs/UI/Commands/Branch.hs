module Darcs.UI.Commands.Branch ( branchCommand ) where

import Darcs.Prelude
import Control.Monad

import Darcs.Patch.Witnesses.Ordered ( nullFL )
import Darcs.Repository
    ( UpdatePending(..)
    , finalizeRepositoryChanges
    , unrecordedChanges
    )
import Darcs.Repository.Branch
import Darcs.Repository.Identify ( identifyRepository )
import Darcs.Repository.Job ( RepoJob(..), withRepoLock, withRepository )
import Darcs.Repository.Working ( replaceWorking )
import Darcs.UI.Commands
    ( DarcsCommand(..)
    , amInHashedRepository
    , nodefaults
    , normalCommand
    , putFinished
    , putInfo
    , withStdOpts
    )
import Darcs.UI.Completion ( noArgs )
import Darcs.UI.Flags ( diffingOpts )
import Darcs.UI.Options
import qualified Darcs.UI.Options.All as O
import Darcs.Util.Path
import Darcs.Util.Printer
import Darcs.Util.Printer.Color ( fancyPrinters )
import Darcs.Util.SignalHandler ( withSignalsBlocked )

branchArgs :: Bool -> (AbsolutePath, AbsolutePath) -> Config -> [String] -> IO [String]
branchArgs includeCurrent _ opts _ = do
  repo <- identifyRepository (O.useCache ? opts) "."
  current <- readCurrentBranchName repo
  let filter_out_current = filter (/= current)
      purge_names = if includeCurrent then id else filter_out_current
  map showBranchName . purge_names <$> listBranches repo

branchCommand :: DarcsCommand
branchCommand = SuperCommand
    { commandProgramName = "darcs"
    , commandName = "branch"
    , commandHelp =
        text description $+$ help
    , commandDescription = description
    , commandPrereq = amInHashedRepository
    , commandSubCommands =
        [ normalCommand branchList
        , normalCommand branchAdd
        , normalCommand branchRemove
        , normalCommand branchRename
        , normalCommand branchSwitch
        ]
    }
  where
    description = "Light-weight branches a la git."
    help = vsep $ map formatWords
      [ [ "Darcs (nowadays) allows you to keep multiple versions of a project"
        , "inside the same repository. This works similar as in git, although"
        , "the command syntax is a bit different. The idea is that a branch in"
        , "repository behaves very similar to separate clone in another directory."
        , "The difference is that they all share the same working directory,"
        , "including pending unrecorded changes (e.g. the result of `darcs add`)."
        ]
      , [ "Switching between branches (using `darcs branch switch`) is like"
        , "`checkout` in git. It is a cheap operation that replaces the"
        , "current set of patches with a different one and updates the working"
        , "tree accordingly. There are (sub-) commands to add new branches,"
        , "delete or rename existing ones, and to push and pull between branches."
        ]
      , [ "When you run darcs for the first time in a repository created by an"
        , "older version of darcs (that doesn't yet know about branches), darcs"
        , "takes the current repository state and turns it into a new branch,"
        , "named 'master'. If you then change the repository state using an"
        , "older version of darcs, the information stored about the branch"
        , "becomes out of date. Therefore, any darcs command that needs to"
        , "read or write a branch will always first update the current"
        , "branch with the (old style) repository state. This ensures that"
        , "full forward and backward compatibility is retained. It also means"
        , "that it is not guaranteed that a remote repository necessarily has"
        , "any branches."
        ]
      ]

branchList :: DarcsCommand
branchList = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "list"
    , commandHelp = text description
    , commandDescription = description
    , commandPrereq = amInHashedRepository
    , commandExtraArgs = 0
    , commandExtraArgHelp = []
    , commandCommand = branchListCmd
    , commandCompleteArgs = noArgs
    , commandArgdefaults = nodefaults
    , commandOptions = opts
    }
  where
    basicOpts = oid
    advancedOpts = oid
    opts = withStdOpts basicOpts advancedOpts
    description = "List existing branches."

branchListCmd :: (AbsolutePath, AbsolutePath) -> Config -> [String] -> IO ()
branchListCmd _ opts _args =
  withRepository (O.useCache ? opts) $ RepoJob $ \repo -> do
    names <- listBranches repo
    case names of
      [] -> putInfo opts $ text "There are no branches yet."
      _ -> do
        current <- readCurrentBranchName repo
        let format name
              | name == current = text "* " <> magentaText (showBranchName name)
              | otherwise       = text "  " <> text        (showBranchName name)
        putDocLnWith fancyPrinters $ vcat $ map format names

branchAdd :: DarcsCommand
branchAdd = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "add"
    , commandHelp = text description $+$ help
    , commandDescription = description
    , commandPrereq = amInHashedRepository
    , commandExtraArgs = -1
    , commandExtraArgHelp = ["BRANCHNAME", "[REPOSITORY [+BRANCHNAME]]"]
    , commandCommand = branchAddCmd
    , commandCompleteArgs = noArgs
    , commandArgdefaults = nodefaults
    , commandOptions = opts
    }
  where
    basicOpts = oid
    advancedOpts = oid
    opts = withStdOpts basicOpts advancedOpts
    description = "Add a branch."
    help = vsep $ map formatWords
      [ [ "This adds a new branch; the first (required) argument is the name"
        , "of the new branch, which must be different from any existing branches"
        , "in the current repository. The optional second and third arguments"
        , "specify the initial state of the branch. They default to the current"
        , "branch in the current repository. You can specify a different (remote)"
        , "repository (second argument), or even a branch in that remote"
        , "repository (third argument), prefixed with a '+' sign."
        ]
      , [ "Adding a branch from a remote repository works pretty much like"
        , "`darcs clone` and has the same performance characteristics."
        ]
      ]

branchAddCmd :: (AbsolutePath, AbsolutePath) -> Config -> [String] -> IO ()
branchAddCmd _ _ [] = fail $ "Missing required BRANCHNAME argument."
branchAddCmd _ opts (arg:more_args) =
  withRepoLock (O.useCache ? opts) (O.umask ? opts) $ RepoJob $ \repo -> do
    branch <- makeBranchName arg
    -- TODO clone branch from remote repo/branch
    case more_args of
      [] -> do
        addBranch repo branch
      _ -> error "cloning remote branches is not yet implemented"
    withSignalsBlocked $
      void $ finalizeRepositoryChanges repo YesUpdatePending (O.dryRun ? opts)
    putFinished opts $ unwords ["adding branch", arg]

branchRemove :: DarcsCommand
branchRemove = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "remove"
    , commandHelp = text description
    , commandDescription = description
    , commandPrereq = amInHashedRepository
    , commandExtraArgs = 1
    , commandExtraArgHelp = ["BRANCHNAME"]
    , commandCommand = branchRemoveCmd
    , commandCompleteArgs = branchArgs False
    , commandArgdefaults = nodefaults
    , commandOptions = opts
    }
  where
    basicOpts = oid
    advancedOpts = oid
    opts = withStdOpts basicOpts advancedOpts
    description = "Remove a branch."

branchRemoveCmd :: (AbsolutePath, AbsolutePath) -> Config -> [String] -> IO ()
branchRemoveCmd _ opts [arg] =
  withRepoLock (O.useCache ? opts) (O.umask ? opts) $ RepoJob $ \repo -> do
    branch <- makeBranchName arg
    -- TODO check for patches that would be lost and prompt the user
    removeBranch repo branch
    withSignalsBlocked $
      void $ finalizeRepositoryChanges repo YesUpdatePending (O.dryRun ? opts)
    putFinished opts $ unwords ["removing branch", arg]
branchRemoveCmd _ _ _ = error "impossible" -- commandExtraArgs = 1

branchRename :: DarcsCommand
branchRename = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "rename"
    , commandHelp = text description
    , commandDescription = description
    , commandPrereq = amInHashedRepository
    , commandExtraArgs = 2
    , commandExtraArgHelp = ["BRANCHNAME","BRANCHNAME"]
    , commandCommand = branchRenameCmd
    , commandCompleteArgs = branchArgs True
    , commandArgdefaults = nodefaults
    , commandOptions = opts
    }
  where
    basicOpts = oid
    advancedOpts = oid
    opts = withStdOpts basicOpts advancedOpts
    description = "Rename a branch."

branchRenameCmd :: (AbsolutePath, AbsolutePath) -> Config -> [String] -> IO ()
branchRenameCmd _ opts [arg1,arg2] =
  withRepoLock (O.useCache ? opts) (O.umask ? opts) $ RepoJob $ \repo -> do
    old <- makeBranchName arg1
    new <- makeBranchName arg2
    renameBranch repo old new
    withSignalsBlocked $
      void $ finalizeRepositoryChanges repo YesUpdatePending (O.dryRun ? opts)
    putFinished opts $ unwords ["renaming branch", arg1, "to", arg2]
branchRenameCmd _ _ _ = error "impossible" -- commandExtraArgs = 1

branchSwitch :: DarcsCommand
branchSwitch = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "switch"
    , commandHelp = text description
    , commandDescription = description
    , commandPrereq = amInHashedRepository
    , commandExtraArgs = 1
    , commandExtraArgHelp = ["BRANCHNAME"]
    , commandCommand = branchSwitchCmd
    , commandCompleteArgs = branchArgs False
    , commandArgdefaults = nodefaults
    , commandOptions = opts
    }
  where
    basicOpts = O.dryRun
    advancedOpts = oid
    opts = withStdOpts basicOpts advancedOpts
    description = "Select another branch as the one to work with."

branchSwitchCmd :: (AbsolutePath, AbsolutePath) -> Config -> [String] -> IO ()
branchSwitchCmd _ opts [arg] =
  withRepoLock (O.useCache ? opts) (O.umask ? opts) $ RepoJob $ \_repo -> do
    let cmd = "switch branch"
    unrecorded <- unrecordedChanges (diffingOpts opts) _repo Nothing
    unless (nullFL unrecorded) $
      fail $ "Can't "++cmd++" when there are unrecorded changes."
    branch <- makeBranchName arg
    _repo <- switchBranch _repo branch
    withSignalsBlocked $ do
      _repo <-
        finalizeRepositoryChanges _repo YesUpdatePending (O.dryRun ? opts)
      unless (O.yes (O.dryRun ? opts)) $
        void $ replaceWorking _repo (O.useIndex ? opts)
    putFinished opts ("switching to branch " ++ arg)
branchSwitchCmd _ _ _ = error "impossible" -- commandExtraArgs = 1
