--  Copyright (C) 2003-2004 David Roundy
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

module Darcs.UI.Commands.Tag ( tag ) where

import Darcs.Prelude

import Control.Monad ( when )
import System.IO ( hPutStr, stderr )

import Darcs.Patch ( RepoPatch )
import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch.Depends ( getUncovered )
import Darcs.Patch.Info ( patchinfo )
import Darcs.Patch.Named ( adddeps, infopatch )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, n2pia )
import Darcs.Patch.Set ( appendPSFL, emptyPatchSet, patchSet2FL, patchSetTags )
import Darcs.Patch.Witnesses.Ordered ( (:>)(..), FL(..) )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..), mapSeal )

import Darcs.Repository
    ( AccessType(..)
    , RepoJob(..)
    , Repository
    , finalizeRepositoryChanges
    , readPatches
    , tentativelyAddPatch
    , withRepoLock
    )
import Darcs.Repository.Flags ( UpdatePending(..) )

import Darcs.UI.Commands
    ( DarcsCommand(..)
    , amInHashedRepository
    , nodefaults
    , putFinished
    , withStdOpts
    )
import Darcs.UI.Completion ( noArgs )
import Darcs.UI.Flags
    ( DarcsFlag
    , author
    , compress
    , getAuthor
    , getDate
    , umask
    , useCache
    , verbosity
    )
import Darcs.UI.Options ( (?), (^) )
import qualified Darcs.UI.Options.All as O
import Darcs.UI.PatchHeader ( getLog )
import Darcs.UI.SelectChanges
    ( SelectionConfig(allowSkipAll)
    , WhichChanges(..)
    , runSelection
    , selectionConfig
    )
import qualified Darcs.UI.SelectChanges as S

import Darcs.Util.Path ( AbsolutePath )
import Darcs.Util.Printer ( Doc, formatWords, vsep )
import Darcs.Util.Tree ( Tree )


tagDescription :: String
tagDescription = "Name the current repository state for future reference."

tagHelp :: Doc
tagHelp =
  vsep $ map formatWords
  [ [ "The `darcs tag` command names the current repository state, so that it"
    , "can easily be referred to later. It does so by recording a special kind"
    , "of patch that makes no changes and which explicitly depends on all"
    , "patches currently existing in the repository (except for those which"
    , "are depended upon by other tags already in the repository). In the"
    , "common case of a sequential series of tags, this means that the tag"
    , "depends on all patches since the last tag, plus that tag itself."
    ]
  , [ "Every *important* state should be"
    , "tagged; in particular it is good practice to tag each stable release"
    , "with a number or codename.  Advice on release numbering can be found"
    , "at <http://producingoss.com/en/development-cycle.html>."
    ]
  , [ "To reproduce the state of a repository `R` as at tag `t`, use the"
    , "command `darcs clone --tag t R`. Note however that tags are matched"
    , "as regular expressions, like with `--patch`. To make sure you get the"
    , "right tag it may be better to use `darcs clone --tag '^t$'`."
    , "The command `darcs show tags` lists all tags in the current repository."
    ]
  , [ "Tagging also provides significant performance benefits: when Darcs"
    , "reaches a shared tag that depends on all antecedent patches, it can"
    , "simply stop processing."
    ]
  , [ "Like normal patches, a tag has a name, an author, a timestamp and an"
    , "optional long description, but it does not change the working tree."
    , "A tag can have any name, but it is generally best to pick a naming"
    , "scheme and stick to it."
    ]
  , [ "By default a tag names the entire repository state at the time the tag"
    , "is created. If the --ask-deps option is used, the patches to include"
    , "as part of the tag can be explicitly selected."
    ]
  , [ "The `darcs tag` command accepts the `--pipe` option, which behaves as"
    , "described in `darcs record`."
    ]
  ]

tag :: DarcsCommand
tag = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "tag"
    , commandHelp = tagHelp
    , commandDescription = tagDescription
    , commandExtraArgs = -1
    , commandExtraArgHelp = ["[TAGNAME]"]
    , commandCommand = tagCmd
    , commandPrereq = amInHashedRepository
    , commandCompleteArgs = noArgs
    , commandArgdefaults = nodefaults
    , commandOptions = tagOpts
    }
  where
    tagBasicOpts
      = O.patchname
      ^ O.author
      ^ O.pipe
      ^ O.askLongComment
      ^ O.askDeps
      ^ O.repoDir
    tagAdvancedOpts = O.compress ^ O.umask
    tagOpts = tagBasicOpts `withStdOpts` tagAdvancedOpts

tagCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
tagCmd _ opts args =
  withRepoLock (useCache ? opts) (umask ? opts) $ RepoJob $ \(repository :: Repository 'RW p wU wR) -> do
    date <- getDate hasPipe
    the_author <- getAuthor (author ? opts) hasPipe
    patches <- readPatches repository
    tags <- return $ patchSetTags patches
    Sealed chosenPatches <-
        if O.askDeps ? opts
            then mapSeal (appendPSFL emptyPatchSet) <$> askAboutTagDepends opts (patchSet2FL patches)
            else return $ Sealed patches
    let deps = getUncovered chosenPatches
    (name, long_comment)  <- get_name_log tags
    myinfo <- patchinfo date name the_author long_comment
    let mypatch = infopatch myinfo NilFL
    _ <- tentativelyAddPatch repository (compress ? opts) (verbosity ? opts) YesUpdatePending
             $ n2pia $ adddeps mypatch deps
    _ <- finalizeRepositoryChanges repository (compress ? opts) (O.dryRun ? opts)
    putFinished opts $ "tagging '"++name++"'"
  where
    get_name_log :: [String] -> IO (String, [String])
    get_name_log tags = do
      (name, comment, _) <-
        getLog
          (case O.patchname ? opts of
             Nothing
                | null args -> Nothing
                | otherwise -> Just (unwords args)
             Just s -> Just s)
          hasPipe (O.logfile ? opts) (O.askLongComment ? opts) Nothing mempty
      when (length name < 2) $
        hPutStr stderr $
        "Do you really want to tag '" ++
        name ++ "'? If not type: darcs obliterate --last=1\n"
      when (name `elem` tags) $
        putStrLn $ "WARNING: The tag " ++ "\"" ++ name ++ "\"" ++ " already exists."
      return ("TAG " ++ name, comment)
    hasPipe = O.pipe ? opts

askAboutTagDepends
     :: forall p wX wY . (RepoPatch p, ApplyState p ~ Tree)
     => [DarcsFlag]
     -> FL (PatchInfoAnd p) wX wY
     -> IO (Sealed (FL (PatchInfoAnd p) wX))
askAboutTagDepends flags ps = do
  let opts = S.PatchSelectionOptions
             { S.verbosity = verbosity ? flags
             , S.matchFlags = []
             , S.interactive = True
             , S.selectDeps = O.PromptDeps
             , S.withSummary = O.NoSummary
             , S.withContext = O.NoContext
             }
  (deps:>_) <- runSelection ps $
                     ((selectionConfig FirstReversed "depend on" opts Nothing Nothing)
                          { allowSkipAll = False })
  return $ Sealed deps
