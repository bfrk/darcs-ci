{-|
License : GPL-2

Packs are an optimization that enable faster repository cloning over HTTP.
A pack is actually a @tar.gz@ file that contains many files that would otherwise
have to be transfered one by one (which is much slower over HTTP).

Two packs are created at the same time by 'createPacks':

  1. The basic pack, contains the pristine tree.
  2. The patches pack, contains the set of patches of the repository.

The paths of these files are @_darcs\/packs\/basic.tar.gz@ and
@_darcs\/packs\/patches.tar.gz@. There is also @_darcs\/packs\/pristine@ which
indicates the pristine hash at the moment of the creation of the packs. This
last file is useful to determine whether the basic pack is in sync with the
current pristine of the repository.
-}

module Darcs.Repository.Packs
    ( fetchAndUnpackBasic
    , fetchAndUnpackPatches
    , packsDir
    , createPacks
    ) where

import qualified Codec.Archive.Tar as Tar
import Codec.Archive.Tar.Entry ( fileEntry, toTarPath )
import Codec.Compression.GZip as GZ ( compress, decompress )
import Control.Concurrent.Async ( withAsync )
import Control.Exception ( Exception, IOException, throwIO, catch, finally )
import Control.Monad ( forM_, when )
import System.IO.Error ( isAlreadyExistsError )
import System.IO.Unsafe ( unsafeInterleaveIO )

import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.List ( isPrefixOf, sort )

import System.Directory ( createDirectoryIfMissing
                        , renameFile
                        , removeFile
                        , doesFileExist
                        , getModificationTime
                        , listDirectory
                        )
import System.FilePath ( (</>)
                       , (<.>)
                       , takeFileName
                       , splitPath
                       , joinPath
                       , takeDirectory
                       )
import System.Posix.Files ( createLink )

import Darcs.Prelude

import Darcs.Util.ByteString ( gzReadFilePS )
import Darcs.Util.Cache
    ( Cache
    , bucketFolder
    , closestWritableDirectory
    , fetchFileUsingCache
    , relinkUsingCache
    )
import Darcs.Util.File ( Cachable(..), fetchFileLazyPS, withTemp )
import Darcs.Util.Global ( darcsdir )
import Darcs.Util.Progress
    ( debugMessage
    , finishedOneIO
    , progressList
    , withProgress
    )
import Darcs.Util.ValidHash ( encodeValidHash )

import Darcs.Patch ( RepoPatch )
import Darcs.Patch.PatchInfoAnd ( extractHash )
import Darcs.Patch.Progress ( progressFL )
import Darcs.Patch.Set
    ( Origin
    , PatchSet
    , patchSet2FL
    , patchSet2RL
    , patchSetInventoryHashes
    )
import Darcs.Patch.Witnesses.Ordered ( mapFL, mapRL )

import Darcs.Repository.Hashed ( readPatches )
import Darcs.Repository.InternalTypes
    ( AccessType(RW)
    , Repository
    , repoCache
    )
import Darcs.Repository.Paths
    ( hashedInventoryPath
    , patchesDirPath
    , pristineDirPath
    )
import Darcs.Repository.Pristine ( readHashedPristineRoot )
import Darcs.Repository.Traverse ( listInventories )

packsDir, basicPack, patchesPack :: String
packsDir     = "packs"
basicPack    = "basic.tar.gz"
patchesPack  = "patches.tar.gz"

fetchAndUnpack :: FilePath -> Cache -> FilePath -> IO ()
fetchAndUnpack filename cache remote = do
  unpackTar cache . Tar.read . GZ.decompress =<<
    fetchFileLazyPS (remote </> darcsdir </> packsDir </> filename) Uncachable

fetchAndUnpackPatches :: PatchSet p Origin wR -> Cache -> FilePath -> IO ()
fetchAndUnpackPatches ps cache remote =
  -- Patches pack can be outdated and thus miss some new patches of the
  -- repository. So we download pack asynchonously and always do a complete
  -- pass of individual patch and inventory files. This is efficient, since for
  -- files that already exist (because they were unpacked from the pack),
  -- fetchFileUsingCache completes very quickly.
  withAsync (fetchAndUnpack patchesPack cache remote) $ \_ -> do
    withProgress "Getting inventories" $
      forM_ (patchSetInventoryHashes ps) .
        maybe (fail "unexpected unhashed inventory") . fetch
    withProgress "Getting patches" $
      forM_ (mapRL hashedPatchHash $ patchSet2RL ps) .
        maybe (fail "unexpected unhashed patch") . fetch
  where
    fetch k h = fetchFileUsingCache cache h >> finishedOneIO k (encodeValidHash h)
    hashedPatchHash = either (const Nothing) Just . extractHash

fetchAndUnpackBasic :: Cache -> FilePath -> IO ()
fetchAndUnpackBasic = fetchAndUnpack basicPack

unpackTar :: Exception e => Cache -> Tar.Entries e -> IO ()
unpackTar _ Tar.Done = return ()
unpackTar _ (Tar.Fail e) = throwIO e
unpackTar c (Tar.Next e es) = case Tar.entryContent e of
  Tar.NormalFile bs _ -> do
    let p = Tar.entryPath e
    if "meta-" `isPrefixOf` takeFileName p
      then unpackTar c es -- just ignore them
      else do
        ex <- doesFileExist p
        if ex
          then debugMessage $ "TAR thread: exists " ++ p ++ "\nStopping TAR thread."
          else do
            if p == hashedInventoryPath
              then writeFile' Nothing p bs
              else writeFile' (closestWritableDirectory c) p $ GZ.compress bs
            debugMessage $ "TAR thread: GET " ++ p
            unpackTar c es
  _ -> fail "Unexpected non-file tar entry"
 where
  writeFile' Nothing path content = withTemp $ \tmp -> do
    BLC.writeFile tmp content
    renameFile tmp path
  writeFile' (Just ca) path content = do
    let fileFullPath = case splitPath path of
          _:hDir:hFile:_  -> joinPath [ca, hDir, bucketFolder hFile, hFile]
          _               -> fail "Unexpected file path"
    createDirectoryIfMissing True $ takeDirectory path
    createLink fileFullPath path `catch` (\(ex :: IOException) -> do
      if isAlreadyExistsError ex then
        return () -- so much the better
      else
        -- ignore cache if we cannot link
        writeFile' Nothing path content)

-- | Create packs from the current recorded version of the repository.
createPacks :: RepoPatch p => Repository 'RW p wU wR -> IO ()
createPacks repo =
  flip finally (mapM_ removeFileIfExists
  [ darcsdir </> "meta-filelist-inventories"
  , darcsdir </> "meta-filelist-pristine"
  , basicTar <.> "part"
  , patchesTar <.> "part"
  ]) $ do
  -- pristine hash
  hash <- readHashedPristineRoot repo
  createDirectoryIfMissing False (darcsdir </> packsDir)
  writeFile ( darcsdir </> packsDir </> "pristine" ) $ encodeValidHash hash
  -- pack patchesTar
  ps <- progressFL "Reading patches" . patchSet2FL <$> readPatches repo
  phs <- sequence $ mapFL patchHash ps
  forM_ phs $ relinkUsingCache (repoCache repo)
  let pfs = map (patchesDirPath </>) $ map encodeValidHash phs
  is <- listInventories repo
  writeFile (darcsdir </> "meta-filelist-inventories") . unlines $
    map takeFileName is
  -- Note: tinkering with zlib's compression parameters does not make
  -- any noticeable difference in generated archive size;
  -- switching to bzip2 would provide ~25% gain OTOH.
  BLC.writeFile (patchesTar <.> "part") . GZ.compress . Tar.write =<<
    mapM fileEntry' ((darcsdir </> "meta-filelist-inventories") : pfs ++ reverse is)
  renameFile (patchesTar <.> "part") patchesTar
  -- pack basicTar
  pr <- sortByMTime =<< dirContents pristineDirPath
  writeFile (darcsdir </> "meta-filelist-pristine") . unlines $
    map takeFileName pr
  BLC.writeFile (basicTar <.> "part") . GZ.compress . Tar.write =<< mapM fileEntry' (
    [ darcsdir </> "meta-filelist-pristine"
    -- unclean: we should not access the non-tentative version here;
    -- will work because we do not modify the tentative state
    , hashedInventoryPath
    ] ++ progressList "Packing pristine" (reverse pr))
  renameFile (basicTar <.> "part") basicTar
 where
  basicTar = darcsdir </> packsDir </> basicPack
  patchesTar = darcsdir </> packsDir </> patchesPack
  fileEntry' x = unsafeInterleaveIO $ do
    content <- BLC.fromChunks . return <$> gzReadFilePS x
    tp <- either fail return $ toTarPath False x
    return $ fileEntry tp content
  dirContents dir = map (dir </>) <$> listDirectory dir
  patchHash x = case extractHash x of
    Left _ -> fail "Unexpected unhashed patch"
    Right h -> return h
  sortByMTime xs = map snd . sort <$> mapM (\x -> (\t -> (t, x)) <$>
    getModificationTime x) xs
  removeFileIfExists x = do
    ex <- doesFileExist x
    when ex $ removeFile x
