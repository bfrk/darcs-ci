module Darcs.Util.File
    ( -- * Files and directories
      getFileStatus
    , doesDirectoryReallyExist
    , removeFileMayNotExist
    , getRecursiveContents
    , getRecursiveContentsFullPath
    , copyTree
      -- * Fetching files
    , fetchFilePS
    , fetchMmapFilePS
    , fetchFileLazyPS
    , gzFetchFilePS
    , gzFetchMmapFilePS
    , speculateFileOrUrl
    , copyFileOrUrl
    , Cachable(..)
      -- * Backup
    , backupByRenaming
    , backupByCopying
      -- * Temporary files
    , withTemp
    , withOpenTemp
    ) where

import Darcs.Prelude
import Darcs.Util.ByteString ( gzReadFilePS, gzReadMmapFilePS, mmapFilePS )
import Darcs.Util.Exception ( catchall, ifDoesNotExistError )
import Darcs.Util.Global ( defaultRemoteDarcsCmd )
import Darcs.Util.HTTP ( Cachable(..) )
import qualified Darcs.Util.HTTP as HTTP
import Darcs.Util.Path ( FilePathLike, toFilePath )
import Darcs.Util.Ssh ( copySSH )
import Darcs.Util.URL ( isHttpUrl, isSshUrl, isValidLocalPath, splitSshUrl )

import Control.Exception ( IOException, bracket, catch )
import Control.Monad ( forM, unless, when, zipWithM_ )
import qualified Data.ByteString as B ( ByteString, readFile )
import qualified Data.ByteString.Lazy as BL
import Network.URI ( parseURI, uriScheme )
import System.Directory
    ( copyFile
    , createDirectory
    , doesDirectoryExist
    , doesFileExist
    , listDirectory
    , removeFile
    , renameDirectory
    , renameFile
    )
import System.FilePath.Posix ( normalise, (</>) )
import System.IO ( Handle, hClose, openBinaryTempFile )
import System.IO.Error ( catchIOError, isDoesNotExistError )
import System.Posix.Files
    ( FileStatus
    , createLink
    , getSymbolicLinkStatus
    , isDirectory
    , isRegularFile
    )

-- | Badly named, since it is actually 'getSymbolicLinkStatus', with all
-- 'IOError's turned into 'Nothing'.
getFileStatus :: FilePath -> IO (Maybe FileStatus)
getFileStatus f =
  Just `fmap` getSymbolicLinkStatus f `catchIOError` (\_-> return Nothing)

-- | Whether a path is an existing directory, but not a symlink to one.
doesDirectoryReallyExist :: FilePath -> IO Bool
doesDirectoryReallyExist f =
    ifDoesNotExistError False (isDirectory `fmap` getSymbolicLinkStatus f)

-- | Variant of 'removeFile' that doesn't throw exception when file does not exist.
removeFileMayNotExist :: FilePathLike p => p -> IO ()
removeFileMayNotExist f = ifDoesNotExistError () (removeFile $ toFilePath f)

-- | Return all files under given directory that aren't directories.
getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  entries <- listDirectory topdir
  paths <- forM entries $ \name -> do
    let path = topdir </> name
    isDir <- doesDirectoryExist path
    if isDir
      then getRecursiveContents path
      else return [name]
  return (concat paths)

-- | Return all files under given directory that aren't directories.
-- Unlike 'getRecursiveContents' this function returns the full path.
getRecursiveContentsFullPath :: FilePath -> IO [FilePath]
getRecursiveContentsFullPath topdir = do
  entries <- listDirectory topdir
  paths <- forM entries $ \name -> do
    let path = topdir </> name
    isDir <- doesDirectoryExist path
    if isDir
      then getRecursiveContentsFullPath path
      else return [path]
  return (concat paths)

-- | Very much darcs-specific copying procedure. For local files it tries
-- to hard-link, falling back to normal copy if it fails. Remote URLs are
-- downloaded using either HTTP or SSH. For SSH, this tries to use the
-- given remote darcs command to invoke it's transfer-mode command.
copyFileOrUrl :: String    -- ^ remote darcs executable
              -> String    -- ^ path representing the origin file or URL
              -> FilePath  -- ^ destination path
              -> Cachable  -- ^ tell whether file to copy is cachable
              -> IO ()
copyFileOrUrl _    fou out _     | isValidLocalPath fou = copyLocal fou out
copyFileOrUrl _    fou out cache | isHttpUrl fou = HTTP.copyRemote fou out cache
copyFileOrUrl rd   fou out _     | isSshUrl fou = copySSH rd (splitSshUrl fou) out
copyFileOrUrl _    fou _   _     = fail $ "unknown transport protocol: " ++ fou

-- | Hard-link file, falling back to normal copying it that fails.
copyLocal  :: String -> FilePath -> IO ()
copyLocal fou out = createLink fou out `catchall` copyFile fou out

-- | Recursively copy a directory, where the target directory is supposed to
-- already exist.
copyTree :: FilePath -> FilePath -> IO ()
copyTree source dest =
 do fs <- getSymbolicLinkStatus source
    if isDirectory fs then do
        fps <- listDirectory source
        zipWithM_ copySubTree (map (source </>) fps) (map (dest </>) fps)
     else fail ("copyTree: Bad source " ++ source)
   `catch` \(_ :: IOException) -> fail ("copyTree: Bad source " ++ source)

-- | Recursively copy a directory, where the target directory does not yet
-- exist but it's parent does.
copySubTree :: FilePath -> FilePath -> IO ()
copySubTree source dest =
 do fs <- getSymbolicLinkStatus source
    if isDirectory fs then do
        createDirectory dest
        fps <- listDirectory source
        zipWithM_ copySubTree (map (source </>) fps) (map (dest </>) fps)
     else if isRegularFile fs then
        copyFile source dest
     else fail ("copySubTree: Bad source "++ source)
    `catch` (\e -> unless (isDoesNotExistError e) $ ioError e)

backupByRenaming :: FilePath -> IO ()
backupByRenaming = backupBy rename
 where rename x y = do
         isD <- doesDirectoryExist x
         if isD then renameDirectory x y else renameFile x y

backupByCopying :: FilePath -> IO ()
backupByCopying = backupBy copy
 where
  copy x y = do
    isD <- doesDirectoryExist x
    if isD then do createDirectory y
                   copyTree (normalise x) (normalise y)
           else copyFile x y

backupBy :: (FilePath -> FilePath -> IO ()) -> FilePath -> IO ()
backupBy backup f =
           do hasBF <- doesFileExist f
              hasBD <- doesDirectoryExist f
              when (hasBF || hasBD) $ helper 0
  where
  helper :: Int -> IO ()
  helper i = do existsF <- doesFileExist next
                existsD <- doesDirectoryExist next
                if existsF || existsD
                   then helper (i + 1)
                   else do putStrLn $ "Backing up " ++ f ++ "(" ++ suffix ++ ")"
                           backup f next
             where next = f ++ suffix
                   suffix = ".~" ++ show i ++ "~"

-- | Generic file fetching support function that takes care of downloading
-- remote files to a temporary location if necessary before invoking the actual
-- reading procedure.
copyAndReadFile :: (FilePath -> IO a) -> String -> Cachable -> IO a
copyAndReadFile readfn fou _ | isValidLocalPath fou = readfn fou
copyAndReadFile readfn fou cache = withTemp $ \t -> do
  copyFileOrUrl defaultRemoteDarcsCmd fou t cache
  readfn t

-- | @fetchFilePS fileOrUrl cache@ returns the content of its argument (either a
-- file or an URL). If it has to download an url, then it will use a cache as
-- required by its second argument.
--
-- We always use default remote darcs, since it is not fatal if the remote
-- darcs does not exist or is too old -- anything that supports transfer-mode
-- should do, and if not, we will fall back to SFTP or SCP.
fetchFilePS :: String -> Cachable -> IO B.ByteString
fetchFilePS = copyAndReadFile B.readFile

-- | Like 'fetchFilePS' but uses mmap, so use this only for hashed files.
fetchMmapFilePS :: String -> Cachable -> IO B.ByteString
fetchMmapFilePS = copyAndReadFile mmapFilePS

-- | @fetchFileLazyPS fileOrUrl cache@ lazily reads the content of its argument
-- (either a file or an URL). Warning: this function may constitute a fd leak;
-- make sure to force consumption of file contents to avoid that. See
-- "fetchFilePS" for details.
fetchFileLazyPS :: String -> Cachable -> IO BL.ByteString
fetchFileLazyPS x c =
  case parseURI x of
    Just x'
      | let s = uriScheme x'
      , s == "http:" || s == "https:" -> HTTP.copyRemoteLazy x c
    _ -> copyAndReadFile BL.readFile x c

-- | Like 'fetchFilePS' but transparently handle gzip compressed files.
gzFetchFilePS :: String -> Cachable -> IO B.ByteString
gzFetchFilePS = copyAndReadFile gzReadFilePS

-- | Like 'fetchFilePS' but transparently handle gzip compressed files.
-- Uses mmap, so use this only for hashed files.
gzFetchMmapFilePS :: String -> Cachable -> IO B.ByteString
gzFetchMmapFilePS = copyAndReadFile gzReadMmapFilePS

-- | Initiate background file download for the given file path or URL
-- to the given location.
speculateFileOrUrl :: String -> FilePath -> IO ()
speculateFileOrUrl fou out
  | isHttpUrl fou = HTTP.speculateRemote fou out
  | otherwise = return ()

-- | Invoke the given action on a file that is temporarily created
-- in the current directory, and removed afterwards.
withTemp :: (FilePath -> IO a) -> IO a
withTemp = bracket get_empty_file removeFileMayNotExist
  where
    get_empty_file = do
      (f, h) <- openBinaryTempFile "." "darcs"
      hClose h `catchall` return ()
      return f

-- | Invoke the given action on a file that is temporarily created and opened
-- in the current directory, and closed and removed afterwards.
withOpenTemp :: ((Handle, FilePath) -> IO a) -> IO a
withOpenTemp = bracket get_empty_file cleanup
  where
    cleanup (h, f) = do
      hClose h `catchall` return ()
      removeFileMayNotExist f
    get_empty_file = swap `fmap` openBinaryTempFile "." "darcs"
    swap (a, b) = (b, a)
