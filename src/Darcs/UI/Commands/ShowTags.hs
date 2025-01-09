--  Copyright (C) 2007 Florian Weimer
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

module Darcs.UI.Commands.ShowTags
    ( showTags
    ) where

import Darcs.Prelude

import Control.Monad ( unless )
import Data.Maybe ( fromMaybe, maybeToList )
import System.IO ( stderr, hPutStrLn )

import Darcs.Patch.Match
    ( MatchFlag(OnePattern)
    , MatchableRP
    , checkMatchSyntax
    , matchAPatch
    )
import Darcs.Patch.Set ( PatchSet(..), patchSetTags, tagsCovering )
import Darcs.Repository ( readPatches, withRepositoryLocation, RepoJob(..) )
import Darcs.UI.Commands ( DarcsCommand(..), withStdOpts, nodefaults, findRepository )
import Darcs.UI.Completion ( noArgs )
import Darcs.UI.Flags ( DarcsFlag, useCache, getRepourl )
import Darcs.UI.Options ( oid, (?), (^) )
import qualified Darcs.UI.Options.All as O
import Darcs.Util.Path ( AbsolutePath )
import Darcs.Util.Printer ( Doc, formatText )

showTagsDescription :: String
showTagsDescription = "Show all tags in the repository."

showTagsHelp :: Doc
showTagsHelp = formatText 80
    [ "The tags command writes a list of all tags in the repository to "
      ++ "standard output."
    , "Tab characters (ASCII character 9) in tag names are changed to spaces "
      ++ "for better interoperability with shell tools. A warning is printed "
      ++ "if this happens."
    ]

showTags :: DarcsCommand
showTags = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "tags"
    , commandHelp = showTagsHelp
    , commandDescription = showTagsDescription
    , commandExtraArgs = 0
    , commandExtraArgHelp = []
    , commandCommand = tagsCmd
    , commandPrereq = findRepository
    , commandCompleteArgs = noArgs
    , commandArgdefaults = nodefaults
    , commandOptions = showTagsOpts
    }
  where
    showTagsBasicOpts = O.possiblyRemoteRepo ^ O.covering
    showTagsOpts = showTagsBasicOpts `withStdOpts` oid

tagsCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
tagsCmd _ opts _ = let repodir = fromMaybe "." (getRepourl opts) in
    withRepositoryLocation (useCache ? opts) repodir $ RepoJob $ \repo ->
        readPatches repo >>= printTags
  where
    printTags :: MatchableRP p => PatchSet p wW wZ -> IO ()
    printTags ps = do
      checkMatchSyntax (maybeToList (fmap OnePattern (O.covering ? opts)))
      case findTags ps of
        Nothing -> fail "No patches matching the pattern have been found."
        Just ts -> mapM_ process ts
    process :: String -> IO ()
    process t = normalize t t False >>= putStrLn
    normalize :: String -> String -> Bool -> IO String
    normalize _ [] _ = return []
    normalize t (x : xs) flag =
        if x == '\t' then do
            unless flag $
                hPutStrLn stderr $ "warning: tag with TAB character: " ++ t
            rest <- normalize t xs True
            return $ ' ' : rest
        else do
            rest <- normalize t xs flag
            return $ x : rest
    findTags :: MatchableRP p => PatchSet p wX wY -> Maybe [String]
    findTags =
      case O.covering ? opts of
        Nothing -> Just . patchSetTags
        Just pat -> tagsCovering (matchAPatch [OnePattern pat])
