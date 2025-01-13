--  Copyright (C) 2003-2005 David Roundy
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
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module Darcs.UI.Commands.Optimize ( optimize ) where

import Darcs.Prelude

import Control.Monad ( when, unless, forM_ )
import System.Directory
    ( listDirectory
    , doesDirectoryExist
    , renameFile
    , createDirectoryIfMissing
    , removeFile
    , removeDirectoryRecursive
    , withCurrentDirectory
    )
import Darcs.UI.Commands ( DarcsCommand(..), nodefaults, noPrereq
                         , amInHashedRepository, amInRepository, putInfo
                         , normalCommand, withStdOpts )
import Darcs.UI.Completion ( noArgs )
import Darcs.Repository.Prefs ( Pref(Defaultrepo), getPreflist, globalCacheDir )
import Darcs.Repository
    ( Repository
    , AccessType(RW)
    , repoLocation
    , withRepoLock
    , RepoJob(..)
    , readPatches
    , reorderInventory
    , cleanRepository
    , writePristine
    )
import Darcs.Repository.Job ( withOldRepoLock )
import Darcs.Repository.Traverse ( specialPatches )
import Darcs.Repository.Paths
    ( formatPath
    , inventoriesDir
    , inventoriesDirPath
    , oldCheckpointDirPath
    , oldCurrentDirPath
    , oldInventoryPath
    , oldPristineDirPath
    , oldTentativeInventoryPath
    , patchesDir
    , patchesDirPath
    , pristineDir
    , pristineDirPath
    )
import Darcs.Repository.Packs ( createPacks )
import Darcs.Patch.Witnesses.Ordered ( lengthRL )
import Darcs.Patch ( RepoPatch )
import Darcs.Patch.Invertible ( mkInvertible )
import Darcs.Patch.Set
    ( patchSet2RL
    , patchSet2FL
    , progressPatchSet
    )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Util.ByteString ( gzReadFilePS )
import Darcs.Util.Printer ( Doc, formatWords, wrapText, ($+$) )
import Darcs.Util.Lock
    ( maybeRelink
    , gzWriteAtomicFilePS
    , writeAtomicFilePS
    , removeFileMayNotExist
    )
import Darcs.Util.File ( doesDirectoryReallyExist )
import Darcs.Util.Exception ( catchall )
import Darcs.Util.Progress
    ( beginTedious
    , endTedious
    , tediousSize
    , debugMessage
    )

import System.FilePath.Posix
    ( takeExtension
    , (</>)
    , joinPath
    )
import Text.Printf ( printf )
import Darcs.UI.Flags
    (  DarcsFlag, useCache, umask )
import Darcs.UI.Options ( DarcsOption, (?), (^) )
import qualified Darcs.UI.Options.All as O
import Darcs.Repository.Flags
    ( PatchFormat(PatchFormat1)
    , UMask(..)
    , WithWorkingDir(WithWorkingDir)
    )
import Darcs.Patch.Progress ( progressFL )
import Darcs.Util.Cache ( allHashedDirs, bucketFolder, cleanCaches, mkDirCache )
import Darcs.Repository.Format
    ( identifyRepoFormat
    , createRepoFormat
    , unsafeWriteRepoFormat
    , formatHas
    , RepoProperty ( HashedInventory )
    )
import Darcs.Repository.PatchIndex
import Darcs.Repository.Hashed
    ( writeTentativeInventory
    , finalizeTentativeChanges
    )
import Darcs.Repository.InternalTypes ( unsafeCoerceR )
import Darcs.Repository.Pristine
    ( applyToTentativePristine
    )

import Darcs.Util.Tree
    ( Tree
    , TreeItem(..)
    , list
    , expand
    , emptyTree
    )
import Darcs.Util.Path ( AbsolutePath, realPath, toFilePath )
import Darcs.Util.Tree.Plain( readPlainTree )

optimizeDescription :: String
optimizeDescription = "Optimize the repository."

optimizeHelp :: Doc
optimizeHelp = formatWords
  [ "The `darcs optimize` command modifies internal data structures of"
  , "the current repository in an attempt to reduce its resource requirements."
  ]
  $+$ "For further details see the descriptions of the subcommands."

optimize :: DarcsCommand
optimize = SuperCommand {
      commandProgramName = "darcs"
    , commandName = "optimize"
    , commandHelp = optimizeHelp
    , commandDescription = optimizeDescription
    , commandPrereq = amInRepository
    , commandSubCommands = [  normalCommand optimizeClean,
                              normalCommand optimizeHttp,
                              normalCommand optimizeReorder,
                              normalCommand optimizeEnablePatchIndex,
                              normalCommand optimizeDisablePatchIndex,
                              normalCommand optimizeCompress,
                              normalCommand optimizeUncompress,
                              normalCommand optimizeRelink,
                              normalCommand optimizeUpgrade,
                              normalCommand optimizeGlobalCache
                           ]
    }

commonBasicOpts :: DarcsOption a (Maybe String -> a)
commonBasicOpts = O.repoDir

commonAdvancedOpts :: DarcsOption a (UMask -> a)
commonAdvancedOpts = O.umask

common :: DarcsCommand
common = DarcsCommand
    { commandProgramName = "darcs"
    , commandExtraArgs = 0
    , commandExtraArgHelp = []
    , commandPrereq =  amInHashedRepository
    , commandArgdefaults = nodefaults
    , commandName = undefined
    , commandHelp = undefined
    , commandDescription = undefined
    , commandCommand =  undefined
    , commandCompleteArgs = noArgs
    , commandOptions = commonOpts
    }
  where
    commonOpts = commonBasicOpts `withStdOpts` commonAdvancedOpts


optimizeClean :: DarcsCommand
optimizeClean = common
    { commandName = "clean"
    , commandDescription = "Garbage collect pristine, inventories and patches"
    , commandHelp = optimizeHelpClean
    , commandCommand = optimizeCleanCmd
    }

optimizeCleanCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
optimizeCleanCmd _ opts _ =
    withRepoLock (useCache ? opts) (umask ? opts) $
    RepoJob $ \repository -> do
      cleanRepository repository
      putInfo opts "Done cleaning repository!"

optimizeUpgrade :: DarcsCommand
optimizeUpgrade = common
    { commandName = "upgrade"
    , commandHelp = wrapText 80
        "Convert old-fashioned repositories to the current default hashed format."
    , commandDescription = "Upgrade repository to latest compatible format"
    , commandPrereq = amInRepository
    , commandCommand = optimizeUpgradeCmd
    , commandOptions =
        withStdOpts commonBasicOpts commonAdvancedOpts
    }

optimizeHttp :: DarcsCommand
optimizeHttp = common
    { commandName = "http"
    , commandHelp = optimizeHelpHttp
    , commandDescription = "Optimize repository for getting over network"
    , commandCommand = optimizeHttpCmd
    }

optimizeHttpCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
optimizeHttpCmd _ opts _ =
    withRepoLock (useCache ? opts) (umask ? opts) $
    RepoJob $ \repository -> do
      cleanRepository repository
      createPacks repository
      putInfo opts "Done creating packs!"

optimizeCompress :: DarcsCommand
optimizeCompress = common
    { commandName = "compress"
    , commandHelp = optimizeHelpCompression
    , commandDescription = "Compress hashed files"
    , commandCommand = optimizeCompressCmd
    }

optimizeUncompress :: DarcsCommand
optimizeUncompress = common
    { commandName = "uncompress"
    , commandHelp = optimizeHelpCompression
    , commandDescription = "Uncompress hashed files (for debugging)"
    , commandCommand = optimizeUncompressCmd
    }

optimizeCompressCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
optimizeCompressCmd _ opts _ =
    withRepoLock (useCache ? opts) (umask ? opts) $
    RepoJob $ \repository -> do
      cleanRepository repository
      optimizeCompression O.GzipCompression opts
      putInfo opts "Done optimizing by compression!"

optimizeUncompressCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
optimizeUncompressCmd _ opts _ =
    withRepoLock (useCache ? opts) (umask ? opts) $
    RepoJob $ \repository -> do
      cleanRepository repository
      optimizeCompression O.NoCompression opts
      putInfo opts "Done uncompressing hashed files."

optimizeCompression :: O.Compression -> [DarcsFlag] -> IO ()
optimizeCompression compression opts = do
    putInfo opts "Optimizing (un)compression of patches..."
    do_compress patchesDirPath
    putInfo opts "Optimizing (un)compression of inventories..."
    do_compress inventoriesDirPath
    putInfo opts "Optimizing (un)compression of pristine..."
    do_compress pristineDirPath
    where
      do_compress f = do
        isd <- doesDirectoryExist f
        if isd
          then withCurrentDirectory f $ do
                 fs <- filter (`notElem` specialPatches) <$> listDirectory "."
                 mapM_ do_compress fs
          else gzReadFilePS f >>=
               case compression of
                 O.GzipCompression -> gzWriteAtomicFilePS f
                 O.NoCompression -> writeAtomicFilePS f

optimizeEnablePatchIndex :: DarcsCommand
optimizeEnablePatchIndex = common
    { commandName = "enable-patch-index"
    , commandHelp = formatWords
        [ "Build the patch index, an internal data structure that accelerates"
        , "commands that need to know what patches touch a given file. Such as"
        , "annotate and log."
        ]
    , commandDescription = "Enable patch index"
    , commandCommand = optimizeEnablePatchIndexCmd
    }

optimizeDisablePatchIndex :: DarcsCommand
optimizeDisablePatchIndex = common
    { commandName = "disable-patch-index"
    , commandHelp = wrapText 80
        "Delete and stop maintaining the patch index from the repository."
    , commandDescription = "Disable patch index"
    , commandCommand = optimizeDisablePatchIndexCmd
    }

optimizeEnablePatchIndexCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
optimizeEnablePatchIndexCmd _ opts _ =
    withRepoLock (useCache ? opts) (umask ? opts) $
    RepoJob $ \repository -> do
      ps <- readPatches repository
      createOrUpdatePatchIndexDisk repository ps
      putInfo opts "Done enabling patch index!"

optimizeDisablePatchIndexCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
optimizeDisablePatchIndexCmd _ opts _ =
    withRepoLock (useCache ? opts) (umask ? opts) $
    RepoJob $ \repo -> do
      deletePatchIndex (repoLocation repo)
      putInfo opts "Done disabling patch index!"

optimizeReorder :: DarcsCommand
optimizeReorder = common
    { commandName = "reorder"
    , commandHelp = formatWords
        [ "This command moves recent patches (those not included in"
        , "the latest tag) to the \"front\", reducing the amount that a typical"
        , "remote command needs to download. It should also reduce the CPU time"
        , "needed for some operations. This is the behavior with --shallow"
        , "which is the default."
        ]
        $+$ formatWords
        [ "With the --deep option it tries to optimize all tags in the whole"
        , "repository. This breaks the history of patches into smaller"
        , "bunches, which can further improve efficiency, but requires all"
        , "patches to be present. It is therefore less suitable for lazy clones."
        ]
    , commandDescription = "Reorder the patches in the repository"
    , commandCommand = optimizeReorderCmd
    , commandOptions =
        withStdOpts basicOpts commonAdvancedOpts
    }
  where
    basicOpts = commonBasicOpts ^ O.optimizeDeep

optimizeReorderCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
optimizeReorderCmd _ opts _ =
    withRepoLock (useCache ? opts) (umask ? opts) $
    RepoJob $ \repository -> do
      reorderInventory repository (O.optimizeDeep ? opts)
      putInfo opts "Done reordering!"

optimizeRelink :: DarcsCommand
optimizeRelink = common
    { commandName = "relink"
    , commandHelp = optimizeHelpRelink 
    , commandDescription = "Replace copies of hashed files with hard links"
    , commandCommand = optimizeRelinkCmd
    , commandOptions = optimizeRelinkOpts
    }
  where
    optimizeRelinkBasicOpts = commonBasicOpts ^ O.siblings
    optimizeRelinkOpts = optimizeRelinkBasicOpts `withStdOpts` commonAdvancedOpts

optimizeRelinkCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
optimizeRelinkCmd _ opts _ =
    withRepoLock (useCache ? opts) (umask ? opts) $
    RepoJob $ \repository -> do
      cleanRepository repository
      doRelink opts
      putInfo opts "Done relinking!"

optimizeHelpHttp :: Doc
optimizeHelpHttp = formatWords
  [ "Using this option creates 'repository packs' that can dramatically"
  , "speed up performance when a user does a `darcs clone` of the repository"
  , "over HTTP. To make use of packs, the clients must have a darcs of at"
  , "least version 2.10."
  ]

optimizeHelpClean :: Doc
optimizeHelpClean = formatWords
  [ "Darcs normally does not delete hashed files that are no longer"
  , "referenced by the current repository state. This command can be"
  , "use to get rid of these files to save some disk space."
  ]

optimizeHelpCompression :: Doc
optimizeHelpCompression =
  formatWords
  [ "Patches, inventories, and pristine files are compressed with zlib"
  , "(RFC 1951) to reduce storage (and download) size."
  , "Older darcs versions allowed to store them"
  , "uncompressed, and darcs is still able to"
  , "read those files if they are not compressed."
  ]
  $+$ formatWords
  [ "The `darcs optimize uncompress` and `darcs optimize compress`"
  , "commands can be used to ensure existing patches in the current"
  , "repository are respectively uncompressed or compressed."
  ]

optimizeHelpRelink :: Doc
optimizeHelpRelink = 
  formatWords
  [ "The `darcs optimize relink` command hard-links patches that the"
  , "current repository has in common with its peers.  Peers are those"
  , "repositories listed in `_darcs/prefs/sources`, or defined with the"
  , "`--sibling` option (which can be used multiple times)."
  ]
  $+$ formatWords
  [ "Darcs uses hard-links automatically, so this command is rarely needed."
  , "It is most useful if you used `cp -r` instead of `darcs clone` to copy a"
  , "repository, or if you pulled the same patch from a remote repository"
  , "into multiple local repositories."
  ]

doRelink :: [DarcsFlag] -> IO ()
doRelink opts =
    do let some_siblings = O.siblings ? opts
       defrepolist <- getPreflist Defaultrepo
       let siblings = map toFilePath some_siblings ++ defrepolist
       if null siblings
          then putInfo opts "No siblings -- no relinking done."
          else do debugMessage "Relinking patches..."
                  patch_tree <- expand =<< readPlainTree patchesDirPath
                  let patches = [ realPath p | (p, File _) <- list patch_tree ]
                  maybeRelinkFiles siblings patches patchesDirPath
                  debugMessage "Done relinking."

maybeRelinkFiles :: [String] -> [String] -> String -> IO ()
maybeRelinkFiles src dst dir =
    mapM_ (maybeRelinkFile src . ((dir ++ "/") ++)) dst

maybeRelinkFile :: [String] -> String -> IO ()
maybeRelinkFile [] _ = return ()
maybeRelinkFile (h:t) f =
    do done <- maybeRelink (h ++ "/" ++ f) f
       unless done $
           maybeRelinkFile t f

-- Only 'optimize' commands that works on old-fashionned repositories
optimizeUpgradeCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
optimizeUpgradeCmd _ opts _ = do
  rf <- identifyRepoFormat "."
  debugMessage "Found our format"
  if formatHas HashedInventory rf
     then putInfo opts "No action taken because this repository already is hashed."
     else do putInfo opts "Upgrading to hashed..."
             withOldRepoLock $ RepoJob $ actuallyUpgradeFormat opts

actuallyUpgradeFormat
  :: (RepoPatch p, ApplyState p ~ Tree)
  => [DarcsFlag] -> Repository 'RW p wU wR -> IO ()
actuallyUpgradeFormat _opts _repository = do
  -- convert patches/inventory
  patches <- readPatches _repository
  let k = "Hashing patch"
  beginTedious k
  tediousSize k (lengthRL $ patchSet2RL patches)
  let patches' = progressPatchSet k patches
  writeTentativeInventory _repository patches'
  endTedious k
  -- convert pristine by applying patches
  -- the faster alternative would be to copy pristine, but the apply method
  -- is more reliable
  -- TODO we should do both and then comapre them
  let patchesToApply = progressFL "Applying patch" $ patchSet2FL patches'
  createDirectoryIfMissing False pristineDirPath
  -- We ignore the returned root hash, we don't use it.
  _ <- writePristine _repository emptyTree
  -- we must coerce here because we just emptied out pristine
  applyToTentativePristine (unsafeCoerceR _repository) (mkInvertible patchesToApply)
  -- now make it official
  finalizeTentativeChanges _repository
  unsafeWriteRepoFormat (createRepoFormat PatchFormat1 WithWorkingDir) formatPath
  -- clean out old-fashioned junk
  debugMessage "Cleaning out old-fashioned repository files..."
  removeFileMayNotExist oldInventoryPath
  removeFileMayNotExist oldTentativeInventoryPath
  removeDirectoryRecursive oldPristineDirPath
    `catchall` removeDirectoryRecursive oldCurrentDirPath
  rmGzsIn patchesDirPath
  rmGzsIn inventoriesDirPath
  hasCheckPoints <- doesDirectoryExist oldCheckpointDirPath
  when hasCheckPoints $ removeDirectoryRecursive oldCheckpointDirPath
 where
  rmGzsIn dir =
    withCurrentDirectory dir $ do
      gzs <- filter ((== ".gz") . takeExtension) `fmap` listDirectory "."
      mapM_ removeFile gzs

optimizeBucketed :: [DarcsFlag] -> IO ()
optimizeBucketed opts = do
  putInfo opts "Migrating global cache to bucketed format."
  gCacheDir <- globalCacheDir

  case gCacheDir of
    Nothing -> fail "New global cache doesn't exist."
    Just gCacheDir' -> do
      let gCachePristineDir = joinPath [gCacheDir', pristineDir]
          gCacheInventoriesDir = joinPath [gCacheDir', inventoriesDir]
          gCachePatchesDir = joinPath [gCacheDir', patchesDir]
      debugMessage "Making bucketed cache from new cache."
      toBucketed gCachePristineDir gCachePristineDir
      toBucketed gCacheInventoriesDir gCacheInventoriesDir
      toBucketed gCachePatchesDir gCachePatchesDir
      putInfo opts "Done making bucketed cache!"
  where
    toBucketed :: FilePath -> FilePath -> IO ()
    toBucketed src dest = do
      srcExist <- doesDirectoryExist src
      if srcExist
        then  do
                debugMessage $ "Making " ++ src ++ " bucketed in " ++ dest
                forM_ subDirSet $ \subDir ->
                  createDirectoryIfMissing True (dest </> subDir)
                fileNames <- listDirectory src
                forM_ fileNames $ \file -> do
                  exists <- doesDirectoryReallyExist (src </> file)
                  if not $ exists
                    then renameFile' src dest file
                    else return ()
        else do
          debugMessage $ show src ++ " didn't exist, doing nothing."
          return ()

    renameFile' :: FilePath -> FilePath -> FilePath -> IO ()
    renameFile' s d f = renameFile (s </> f) (joinPath [d, bucketFolder f, f])

    subDirSet :: [String]
    subDirSet = map toStrHex [0..255]

    toStrHex :: Int -> String
    toStrHex = printf "%02x"


optimizeGlobalCache :: DarcsCommand
optimizeGlobalCache = common
    { commandName = "cache"
    , commandExtraArgs = 0
    , commandExtraArgHelp = []
    , commandHelp = optimizeHelpGlobalCache
    , commandDescription = "Garbage collect global cache"
    , commandCommand = optimizeGlobalCacheCmd
    , commandPrereq = noPrereq
    }

optimizeHelpGlobalCache :: Doc
optimizeHelpGlobalCache = formatWords
  [ "This command deletes obsolete files within the global cache."
  ]
  $+$ formatWords
  [ "It also automatically migrates the global cache to the (default)"
  , "bucketed format."
  ]

optimizeGlobalCacheCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
optimizeGlobalCacheCmd _ opts _ = do
  optimizeBucketed opts
  globalCacheDir >>= \case
    Just dir -> mapM_ (cleanCaches (mkDirCache dir)) allHashedDirs
    Nothing -> return ()
  putInfo opts "Done cleaning global cache!"
