{-# LANGUAGE OverloadedStrings #-}
module Darcs.UI.Commands.Compare ( compareCommand ) where

import Darcs.Prelude

import Control.Monad ( when )
import System.Exit ( exitFailure, exitSuccess )

import Darcs.Patch.Depends ( findCommon )
import Darcs.Patch.Witnesses.Ordered ( Fork(..), lengthFL )
import Darcs.Repository
    ( ReadingOrWriting(..)
    , RepoJob(..)
    , identifyRepositoryFor
    , readPatches
    , withRepository
    )
import Darcs.Repository.State ( unrecordedChanges )
import Darcs.UI.Commands
    ( DarcsCommand(..)
    , amInHashedRepository
    , defaultRepo
    , putInfo
    , putWarning
    , withStdOpts
    )
import Darcs.UI.Completion ( noArgs )
import Darcs.UI.Flags ( diffingOpts, fixUrl )
import Darcs.UI.Options ( Config, oid, (?), (^) )
import qualified Darcs.UI.Options.All as O
import Darcs.Util.Exception ( ifDoesNotExistError )
import Darcs.Util.Path ( AbsolutePath, toFilePath )
import Darcs.Util.Printer ( formatWords, text, ($$), (<+>) )

compareCommand :: DarcsCommand
compareCommand = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "compare"
    , commandHelp = help
    , commandDescription = description
    , commandExtraArgs = 1
    , commandExtraArgHelp = ["[FILE or DIRECTORY]..."]
    , commandCommand = command
    , commandPrereq = amInHashedRepository
    , commandCompleteArgs = noArgs
    , commandArgdefaults = defaultRepo
    , commandOptions = withStdOpts basicOpts advancedOpts
    }
  where
    basicOpts = O.repoDir ^ oid
    advancedOpts = oid
    description = "Compare repositories"
    help = formatWords
      [ "This command compares two repositories, usually the one you are in"
      , "with either the default remote repository (as set by clone or by passing"
      , "--set-default to other commands), or any other repository you gave it"
      , "as argument (URL or path)."
      , "It lists the patches we could push to the remote repo,"
      , "the patches we could pull from the remote repo,"
      , "and the number of local unrecorded changes."
      , "If all these values are zero, then the command succeeds,"
      , "otherwise it fails. This means you can use it as equality"
      , "operator in scripts, similar to `diff` or `cmp`."
      ]

command :: (AbsolutePath, AbsolutePath) -> Config -> [String] -> IO ()
command (here, old) cfg args = do
  there <- case args of
    [arg] -> fixUrl old arg
    _ -> fail "too few or too many arguments"
  doCompare cfg here there

doCompare :: Config -> AbsolutePath -> String -> IO ()
doCompare cfg here there =
  withRepository (O.useCache ? cfg) $ RepoJob $ \this -> do
    when (toFilePath here == there) $
      putWarning cfg "Warning: comparing repository with itself!"
    that <- identifyRepositoryFor Reading this (O.useCache ? cfg) there
    us <- readPatches this
    them <- readPatches that
    Fork _ topush topull <- return $ findCommon us them
    our_unrecorded <- unrecordedChanges (diffingOpts cfg) this Nothing
    let num_topull = lengthFL topull
        num_topush = lengthFL topush
        num_unrecorded = lengthFL our_unrecorded
    putInfo cfg $
      text (show num_topull) <+> "patches to pull"
      $$ text (show num_topush) <+> "patches to push"
      $$ text (show num_unrecorded) <+> "local unrecorded changes"
    -- FIXME this gives funny results instead of failing. Looks like it uses
    -- 'here' for the working tree and 'that' for the pristine state
    num_their_unrecorded <-
      ifDoesNotExistError 0 $ do
        their_unrecorded <- unrecordedChanges (diffingOpts cfg) that Nothing
        let num_their_unrecorded = lengthFL their_unrecorded
        putInfo cfg $ text (show num_their_unrecorded) <+> "remote unrecorded changes"
        return num_their_unrecorded
    if num_topull + num_topush + num_unrecorded + num_their_unrecorded == 0 then
      exitSuccess
    else
      exitFailure
