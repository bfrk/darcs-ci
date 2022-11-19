--  Copyright (C) 2003 David Roundy
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
-- Module      : Darcs.UI.Commands.Dist
-- Copyright   : 2003 David Roundy
-- License     : GPL
-- Maintainer  : darcs-devel@darcs.net
-- Stability   : experimental
-- Portability : portable

module Darcs.UI.Commands.Dist
    (
      dist
    , doFastZip -- libdarcs export
    , doFastZip'
    ) where

import Darcs.Prelude

import Control.Monad ( forM, unless, when )
import System.Process ( system )
import System.Exit ( ExitCode(..), exitWith )
import System.FilePath.Posix ( takeFileName, (</>) )

import Darcs.Util.Workaround ( getCurrentDirectory )
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import Codec.Compression.GZip ( compress )

import qualified Codec.Archive.Zip as Zip
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Darcs.UI.Flags as F ( DarcsFlag, useCache )
import Darcs.UI.Options ( oid, parseFlags, (?), (^) )
import qualified Darcs.UI.Options.All as O

import Darcs.UI.Commands
    ( DarcsCommand(..), withStdOpts, nodefaults, amInHashedRepository
    , putVerbose, putInfo
    )
import Darcs.UI.Completion ( noArgs )
import Darcs.Util.Lock ( withDelayedDir )
import Darcs.Patch.Match ( patchSetMatch )
import Darcs.Repository.Match ( getPristineUpToMatch )
import Darcs.Repository ( RepoJob(..), withRepository, withRepositoryLocation )
import Darcs.Repository.Prefs ( getPrefval )
import Darcs.Repository.Pristine ( readPristine )

import Darcs.Util.DateTime ( getCurrentTime, toSeconds )
import Darcs.Util.Path ( AbsolutePath, realPath, toFilePath )
import Darcs.Util.Printer ( Doc, text, vcat )
import qualified Darcs.Util.Tree as T
import Darcs.Util.Tree.Plain ( readPlainTree, writePlainTree )


distDescription :: String
distDescription = "Create a distribution archive."

distHelp :: Doc
distHelp = text $ unlines
  [ "`darcs dist` creates a compressed archive in the repository's root"
  , "directory, containing the recorded state of the working tree"
  , "(unrecorded changes and the `_darcs` directory are excluded)."
  , "The command accepts matchers to create an archive of some past"
  , "repository state, for instance `--tag`."
  , ""
  , "By default, the archive (and the top-level directory within the"
  , "archive) has the same name as the repository, but this can be"
  , "overridden with the `--dist-name` option."
  , ""
  , "If a predist command is set (see `darcs setpref`), that command will"
  , "be run on the recorded state prior to archiving.  For example,"
  , "autotools projects would set it to `autoconf && automake`."
  , ""
  , "If `--zip` is used, matchers and the predist command are ignored."
  ]

dist :: DarcsCommand
dist = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "dist"
    , commandHelp = distHelp
    , commandDescription = distDescription
    , commandExtraArgs = 0
    , commandExtraArgHelp = []
    , commandCommand = distCmd
    , commandPrereq = amInHashedRepository
    , commandCompleteArgs = noArgs
    , commandArgdefaults = nodefaults
    , commandOptions = distOpts
    }
  where
    distBasicOpts
      = O.distname
      ^ O.distzip
      ^ O.repoDir
      ^ O.matchUpToOne
      ^ O.setScriptsExecutable
      ^ O.storeInMemory
    distOpts = distBasicOpts `withStdOpts` oid

distCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
distCmd _ opts _ | O.distzip ? opts = doFastZip opts
distCmd _ opts _ = withRepository (useCache ? opts) $ RepoJob $ \repository -> do
  let matchFlags = parseFlags O.matchUpToOne opts
  formerdir <- getCurrentDirectory
  let distname = getDistName formerdir (O.distname ? opts)
  predist <- getPrefval "predist"
  let resultfile = formerdir </> distname ++ ".tar.gz"
  raw_tree <-
    case patchSetMatch matchFlags of
      Just psm -> getPristineUpToMatch repository psm
      Nothing -> readPristine repository
  tree <- case predist of
    Nothing -> T.expand raw_tree
    Just pd -> do
      withDelayedDir "dist" $ \d -> do
        writePlainTree raw_tree "."
        ec <- system pd
        unless (ec == ExitSuccess) $ do
          putStrLn "Dist aborted due to predist failure"
          exitWith ec
        T.expand =<< readPlainTree (toFilePath d)
  entries <- createEntries distname tree
  putVerbose opts $ vcat $ map (text . Tar.entryPath) entries
  BL.writeFile resultfile $ compress $ Tar.write entries
  putInfo opts $ text $ "Created dist as " ++ resultfile
  where
    createEntries top tree = do
      topentry <- Tar.directoryEntry <$> either fail return (Tar.toTarPath True top)
      rest <- forM (T.list tree) go
      return $ topentry : rest
      where
        go (_, T.Stub _ _) = error "impossible"
        go (path, T.SubTree _) = do
          tarpath <- either fail return $ Tar.toTarPath True (top </> realPath path)
          return $ Tar.directoryEntry tarpath
        go (path, T.File b) = do
          content <- T.readBlob b
          tarpath <- either fail return $ Tar.toTarPath False (top </> realPath path)
          let entry = Tar.fileEntry tarpath content
          return $
            if O.yes (O.setScriptsExecutable ? opts) &&
               executablePrefix `BL.isPrefixOf` content
              then entry {Tar.entryPermissions = Tar.executableFilePermissions}
              else entry
    executablePrefix = BLC.pack "#!"

getDistName :: FilePath -> Maybe String -> FilePath
getDistName _ (Just dn) = takeFileName dn
getDistName currentDirectory _ = takeFileName currentDirectory

doFastZip :: [DarcsFlag] -> IO ()
doFastZip opts = do
  currentdir <- getCurrentDirectory
  let distname = getDistName currentdir (O.distname ? opts)
  let resultfile = currentdir </> distname ++ ".zip"
  doFastZip' opts currentdir (BL.writeFile resultfile)
  putInfo opts $ text $ "Created " ++ resultfile

doFastZip' :: [DarcsFlag]              -- ^ Flags/options
           -> FilePath                 -- ^ The path to the repository
           -> (BL.ByteString -> IO a)  -- ^ An action to perform on the archive contents
           -> IO a
doFastZip' opts path act = withRepositoryLocation (useCache ? opts) path $ RepoJob $ \repo -> do
  when (O.setScriptsExecutable ? opts == O.YesSetScriptsExecutable) $
    putStrLn "WARNING: Zip archives cannot store executable flag."  
  let distname = getDistName path (O.distname ? opts)
  pristine <-
    T.expand =<<
    case patchSetMatch (O.matchUpToOne ? opts) of
      Just psm -> getPristineUpToMatch repo psm
      Nothing -> readPristine repo
  pathsAndContents <-
    forM (T.list pristine) $ \(p,i) -> do
      case i of
        T.Stub _ _ -> error "tree is not expanded"
        T.SubTree _ -> return (distname </> realPath p ++ "/", BL.empty)
        T.File b -> do
          content <- T.readBlob b
          return (distname </> realPath p, content)
  epochtime <- toSeconds `fmap` getCurrentTime
  let entries = [ Zip.toEntry filepath epochtime contents | (filepath,contents) <- pathsAndContents ]
  let archive = foldr Zip.addEntryToArchive Zip.emptyArchive entries
  act (Zip.fromArchive archive)
