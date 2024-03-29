--  Copyright (C) 2005 Florian Weimer
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

module Darcs.UI.Commands.ShowFiles ( showFiles ) where

import Darcs.Prelude

import Darcs.Patch.Match ( patchSetMatch )
import Darcs.Repository ( RepoJob(..), withRepository )
import Darcs.Repository.Match ( getPristineUpToMatch )
import Darcs.Repository.State ( readPristine, readPristineAndPending )
import Darcs.UI.Commands
    ( DarcsCommand(..)
    , amInRepository
    , nodefaults
    , withStdOpts
    )
import Darcs.UI.Completion ( knownFileArgs )
import Darcs.UI.Flags ( DarcsFlag, pathsFromArgs, useCache )
import Darcs.UI.Options ( oid, parseFlags, (?), (^) )
import qualified Darcs.UI.Options.All as O
import Darcs.Util.Path
    ( AbsolutePath
    , AnchoredPath
    , anchoredRoot
    , displayPath
    , isPrefix
    )
import Darcs.Util.Printer ( Doc, text )
import Darcs.Util.Tree ( Tree, TreeItem(..), expand, list )

showFilesDescription :: String
showFilesDescription = "Show version-controlled files in the working tree."

showFilesHelp :: Doc
showFilesHelp = text $
 "The `darcs show files` command lists those files and directories in\n" ++
 "the working tree that are under version control.  This command is\n" ++
 "primarily for scripting purposes; end users will probably want `darcs\n" ++
 "whatsnew --summary`.\n" ++
 "\n" ++
 "A file is \"pending\" if it has been added but not recorded.  By\n" ++
 "default, pending files (and directories) are listed; the `--no-pending`\n" ++
 "option prevents this.\n" ++
 "\n" ++
 "By default `darcs show files` lists both files and directories, but the\n" ++
 "`--no-files` and `--no-directories` flags modify this behaviour.\n" ++
 "\n" ++
 "By default entries are one-per-line (i.e. newline separated).  This\n" ++
 "can cause problems if the files themselves contain newlines or other\n" ++
 "control characters.  To get around this, the `--null` option uses the\n" ++
 "null character instead.  The script interpreting output from this\n" ++
 "command needs to understand this idiom; `xargs -0` is such a command.\n" ++
 "\n" ++
 "For example, to list version-controlled files by size:\n" ++
 "\n" ++
 "    darcs show files -0 | xargs -0 ls -ldS\n"

showFiles :: DarcsCommand
showFiles = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "files"
    , commandHelp = showFilesHelp
    , commandDescription = showFilesDescription
    , commandExtraArgs = -1
    , commandExtraArgHelp = ["[FILE or DIRECTORY]..."]
    , commandCommand = manifestCmd
    , commandPrereq = amInRepository
    , commandCompleteArgs = knownFileArgs
    , commandArgdefaults = nodefaults
    , commandOptions = showFilesOpts
    }
  where
    showFilesBasicOpts
      = O.files
      ^ O.directories
      ^ O.pending
      ^ O.nullFlag
      ^ O.matchUpToOne
      ^ O.repoDir
    showFilesOpts = showFilesBasicOpts `withStdOpts` oid

manifestCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
manifestCmd fps opts args = do
    paths <- pathsFromArgs fps args
    mapM_ output =<< manifestHelper opts paths
  where
    output_null name = do { putStr name ; putChar '\0' }
    output = if parseFlags O.nullFlag opts then output_null else putStrLn

manifestHelper :: [DarcsFlag] -> [AnchoredPath] -> IO [FilePath]
manifestHelper opts prefixes =
  fmap (map displayPath . onlysubdirs prefixes . listFilesOrDirs) $
    withRepository (useCache ? opts) $ RepoJob $ \r -> do
      case (patchSetMatch matchFlags, parseFlags O.pending opts) of
        (Nothing, False)  -> expand =<< readPristine r
        (Nothing, True)   -> expand =<< readPristineAndPending r
        (Just psm, False) -> getPristineUpToMatch r psm
        (Just _, True)    -> fail "can't mix match and pending flags"
  where
    matchFlags = parseFlags O.matchUpToOne opts

    onlysubdirs [] = id
    onlysubdirs dirs = filter (\p -> any (`isPrefix` p) dirs)

    listFilesOrDirs :: Tree IO -> [AnchoredPath]
    listFilesOrDirs =
        filesDirs (parseFlags O.files opts) (parseFlags O.directories opts)
      where
        filesDirs False False _ = []
        filesDirs False True t = anchoredRoot : [p | (p, SubTree _) <- list t]
        filesDirs True False t = [p | (p, File _) <- list t]
        filesDirs True True t = anchoredRoot : map fst (list t)
