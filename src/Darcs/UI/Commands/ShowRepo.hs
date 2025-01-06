--  Copyright (C) 2007 Kevin Quick
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

module Darcs.UI.Commands.ShowRepo ( showRepo ) where

import Darcs.Prelude

import Data.List ( intercalate )
import Data.Maybe ( catMaybes )
import qualified Text.XML.Light as XML

import Darcs.Util.Path ( AbsolutePath )
import Darcs.UI.Flags ( DarcsFlag, useCache, hasXmlOutput, enumeratePatches )
import Darcs.UI.Options ( (^), oid, (?) )
import qualified Darcs.UI.Options.All as O
import Darcs.UI.Commands ( DarcsCommand(..), withStdOpts, nodefaults, amInRepository )
import Darcs.UI.Completion ( noArgs )
import Darcs.Repository
    ( Repository
    , RepoFormat
    , Cache
    , repoFormat
    , repoLocation
    , repoCache
    , withRepository
    , RepoJob(..)
    , readPatches )
import Darcs.Repository.Hashed( repoXor )
import Darcs.Repository.PatchIndex ( isPatchIndexDisabled, doesPatchIndexExist )
import Darcs.Repository.Prefs
    ( Pref(Author, Defaultrepo)
    , getMotd
    , getPreflist
    , getPrefval
    )
import Darcs.Patch ( RepoPatch )
import Darcs.Patch.Set ( patchSet2RL )
import Darcs.Patch.Witnesses.Ordered ( lengthRL )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Util.ByteString ( decodeLocale )
import Darcs.Util.Printer ( Doc, text )
import Darcs.Util.Tree ( Tree )

showRepoHelp :: Doc
showRepoHelp = text $
 "The `darcs show repo` command displays statistics about the current\n" ++
 "repository, allowing third-party scripts to access this information\n" ++
 "without inspecting `_darcs` directly (and without breaking when the\n" ++
 "`_darcs` format changes).\n" ++
 "\n" ++
 "The 'Weak Hash' identifies the set of patches of a repository independently\n" ++
 "of ordering. It can be used to easily compare two repositories of a same\n" ++
 "project. It is not cryptographically secure.\n" ++
 "\n" ++
 "By default, output includes statistics that require walking through the patches\n" ++
 "recorded in the repository, namely the 'Weak Hash' and the count of patches.\n" ++
 "If this data isn't needed, use `--no-enum-patches` to accelerate this command\n" ++
 "from O(n) to O(1).\n" ++
 "\n" ++
 "By default, output is in a human-readable format.  The `--xml-output`\n" ++
 "option can be used to generate output for machine postprocessing.\n"

showRepoDescription :: String
showRepoDescription = "Show repository summary information"

showRepo :: DarcsCommand
showRepo = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "repo"
    , commandHelp = showRepoHelp
    , commandDescription = showRepoDescription
    , commandExtraArgs = 0
    , commandExtraArgHelp = []
    , commandCommand = repoCmd
    , commandPrereq = amInRepository
    , commandCompleteArgs = noArgs
    , commandArgdefaults = nodefaults
    , commandOptions = showRepoOpts
    }
  where
    showRepoBasicOpts = O.repoDir ^ O.xmlOutput ^ O.enumPatches
    showRepoOpts = showRepoBasicOpts `withStdOpts` oid

repoCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
repoCmd _ opts _ = do
  withRepository (useCache ? opts) $
    RepoJob $ \r ->
      getRepoInfo r opts >>= putStr .
        if hasXmlOutput opts then
          XML.ppElement . XML.unode "repository"
        else
          showRepoInfo

data RepoInfo = RepoInfo
  { riFormat :: RepoFormat
  , riRoot :: String
  , riCache :: Cache
  , riPatchIndex :: String
  , riTestPref :: Maybe String
  , riBinariesfilePref :: Maybe String
  , riBoringfilePref :: Maybe String
  , riPredistPref :: Maybe String
  , riAuthor :: Maybe String
  , riDefaultRemote :: Maybe String
  , riNumPatches :: Maybe Int
  , riWeakHash :: Maybe String
  , riMotd :: Maybe String
  }

getRepoInfo
  :: (RepoPatch p, ApplyState p ~ Tree)
  => Repository rt p wU wR -> [DarcsFlag] -> IO RepoInfo
getRepoInfo r opts = do
  let riFormat = repoFormat r
  let riRoot = repoLocation r
  let riCache = repoCache r
  piExists <- doesPatchIndexExist riRoot
  piDisabled <- isPatchIndexDisabled riRoot
  let riPatchIndex = showPatchIndexInfo (piExists, piDisabled)
  riBinariesfilePref <- getPrefval "binariesfile"
  riBoringfilePref <- getPrefval "boringfile"
  riPredistPref <- getPrefval "predist"
  riTestPref <- getPrefval "test"
  let unlessnull x = if null x then Nothing else Just x
  riAuthor <- showPrefList <$> getPreflist Author
  riDefaultRemote <- showPrefList <$> getPreflist Defaultrepo
  riNumPatches <-
    if enumeratePatches opts then
      Just . lengthRL . patchSet2RL <$> readPatches r
    else
      return Nothing
  riWeakHash <-
    if enumeratePatches opts then Just . show <$> repoXor r else return Nothing
  riMotd <- unlessnull . decodeLocale <$> getMotd riRoot
  return $ RepoInfo {..}

instance XML.Node RepoInfo where
  node qn RepoInfo {..} =
    XML.node qn $
      [ XML.unode "format" $ showInOneLine riFormat
      , XML.unode "root" riRoot
      , XML.unode "cache" (showInOneLine riCache)
      , XML.unode "patchindex" riPatchIndex
      ]
      ++
      catMaybes
      [ XML.unode "testpref" <$> riTestPref
      , XML.unode "binariesfilepref" <$> riBinariesfilePref
      , XML.unode "boringfilepref" <$> riBoringfilePref
      , XML.unode "predistpref" <$> riPredistPref
      , XML.unode "author" <$> riAuthor
      , XML.unode "defaultremote" <$> riDefaultRemote
      , XML.unode "numpatches" . show <$> riNumPatches
      , XML.unode "weakhash" <$> riWeakHash
      , XML.unode "motd" <$> riMotd
      ]

showRepoInfo :: RepoInfo -> String
showRepoInfo RepoInfo{..} =
  unlines $
    [ out "Format" $ showInOneLine riFormat
    , out "Root" riRoot
    , out "Cache" $ showInOneLine $ riCache
    , out "PatchIndex" $ riPatchIndex
    ]
    ++ catMaybes
    [ out "test Pref" <$> riTestPref
    , out "binariesfile Pref" <$> riBinariesfilePref
    , out "boringfile Pref" <$> riBoringfilePref
    , out "predist Pref" <$> riPredistPref
    , out "Author" <$> riAuthor
    , out "Default Remote" <$> riDefaultRemote
    , out "Num Patches" . show <$> riNumPatches
    , out "Weak Hash" <$> riWeakHash
    , out "MOTD" <$> riMotd
    ]
  where
    -- labelled strings: labels are right-aligned at 15 characters;
    -- subsequent lines in multi-line output are indented accordingly.
    out t i =
      replicate (15 - length t) ' ' ++ t ++ ": " ++
        intercalate ('\n' : replicate 17 ' ') (lines i)

showPatchIndexInfo :: (Bool, Bool) -> String
showPatchIndexInfo pi =
  case pi of
    (_, True) -> "disabled"
    (True, False) -> "enabled"
    (False, False) -> "enabled, but not yet created"

showInOneLine :: Show a => a -> String
showInOneLine = intercalate ", " . lines . show

showPrefList :: [String] -> Maybe String
showPrefList [] = Nothing
showPrefList ss = Just $ intercalate ", " ss
