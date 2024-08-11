--  Copyright (C) 2009-2011 Petr Rockai
--            (C) 2013 Jose Neder
--  BSD3
{-# LANGUAGE MultiParamTypeClasses #-}

-- | This module contains plain tree indexing code. The index itself is a
-- CACHE: you should only ever use it as an optimisation and never as a primary
-- storage. In practice, this means that when we change index format, the
-- application is expected to throw the old index away and build a fresh
-- index. Please note that tracking index validity is out of scope for this
-- module: this is responsibility of your application. It is advisable that in
-- your validity tracking code, you also check for format validity (see
-- 'indexFormatValid') and scrap and re-create index when needed.
--
-- The index is a binary file that overlays a hashed tree over the working
-- copy. This means that every working file and directory has an entry in the
-- index, that contains its name and hash and validity data. The validity data
-- is a timestamp plus the file size. The file hashes are sha256's of the
-- file's content. It also contains the fileid to track moved files.
--
-- There are two entry types, a file entry and a directory entry. Both have a
-- common binary format (see 'Item'). The on-disk format is described by
-- the section /Index format/ below.
--
-- For each file, the index has a copy of the file's last modification
-- timestamp taken at the instant when the hash has been computed. This means
-- that when file size and timestamp of a file in working tree matches those in
-- the index, we assume that the hash stored in the index for given file is
-- valid. These hashes are then exposed in the resulting 'Tree' object, and can
-- be leveraged by eg. 'diffTrees' to compare many files quickly.
--
-- You may have noticed that we also keep hashes of directories. These are
-- assumed to be valid whenever the complete subtree has been valid. At any
-- point, as soon as a size or timestamp mismatch is found, the working file in
-- question is opened, its hash (and timestamp and size) is recomputed and
-- updated in-place in the index file (everything lives at a fixed offset and
-- is fixed size, so this isn't an issue). This is also true of directories:
-- when a file in a directory changes hash, this triggers recomputation of all
-- of its parent directory hashes; moreover this is done efficiently -- each
-- directory is updated at most once during an update run.
--
-- /Endianness/
--
-- Since version 6 (magic == "HSI6"), the file format depends on the endianness
-- of the architecture. To account for the (rare) case where darcs executables
-- from different architectures operate on the same repo, we make an additional
-- check in indexFormatValid to detect whether the file's endianness differs
-- from what we expect. If this is detected, the file is considered invalid and
-- will be re-created.
--
-- /Index format/
--
-- The index starts with a header consisting of a 4 bytes magic word, followed
-- by a 4 byte word to indicate the endianness of the encoding. This word
-- should, when read directly from the mmapped file, be equal to 1.
--
-- After the header comes the actual content of the index, which is a
-- sequence of 'Item's. An 'Item' consists of:
--
-- * size: item size, 8 bytes
-- * aux: timestamp (for file) or offset to sibling (for dir), 8 bytes
-- * fileid: inode or fhandle of the item, 8 bytes
-- * hash: sha256 of content, 32 bytes
-- * name length: 4 bytes
-- * type: 'D' or 'F', 1 byte
-- * name: filename, variable >= 0
-- * null: terminating null byte
-- * alignment padding: 0 to 3 bytes
--
-- Each 'Item' is 4 byte aligned. Thus the length must be
-- rounded up to get the position of the next item using 'align'. Similar,
-- when determining the aux (offset to sibling) for dir items.
--
-- With directories, the aux holds the offset of the next sibling item in the
-- index, so we can efficiently skip reading the whole subtree starting at a
-- given directory (by just seeking aux bytes forward). The items are
-- pre-ordered with respect to directory structure -- the directory comes first
-- and after it come all its items. Cf. 'openIndex'.
--
-- For files, the aux field holds a timestamp.
--
-- Internally, the item is stored as a pointer to the first field (iBase) which
-- we directly use to read/write the first four fields (size, aux, fileid, hash),
-- and the item type and name as separate fields since they are never going to
-- be modified.
--
-- TODO
--
-- The null byte terminator seems useless.
--
-- We could as well use a single plain pointer for the item. The dumpIndex
-- function demonstrates how this could be done.

module Darcs.Util.Index
    ( openIndex
    , updateIndexFrom
    , indexFormatValid
    , treeFromIndex
    , listFileIDs
    , Index
    , filter
    , getFileID
    , IndexEntry(..)
    , dumpIndex
    -- for testing
    , align
    ) where

import Darcs.Prelude hiding ( readFile, writeFile, filter )

import Darcs.Util.ByteString ( readSegment, decodeLocale )
import qualified Darcs.Util.File ( getFileStatus )
import Darcs.Util.Global ( debugMessage )
import Darcs.Util.Hash ( Hash(..), sha256 )
import Darcs.Util.Tree
import Darcs.Util.Tree.Hashed ( darcsTreeHash )
import Darcs.Util.Path
    ( AnchoredPath(..)
    , realPath
    , anchoredRoot
    , Name
    , unName
    , rawMakeName
    , appendPath
    , flatten
    )
import Darcs.Util.Progress ( beginTedious, endTedious, finishedOneIO )

import Control.Monad( when )
import Control.Exception( catch, SomeException )

import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Internal
    ( c2w, w2c
    , nullForeignPtr
    )
import qualified Data.ByteString.Short.Internal as BS

import Data.Int( Int64, Int32 )
import Data.Word( Word8 )
import Data.IORef( )
import Data.Maybe( fromJust, isNothing )

import Foreign.Storable
import Foreign.ForeignPtr( ForeignPtr, withForeignPtr, castForeignPtr )
import Foreign.Ptr( Ptr, plusPtr )

import System.IO ( hPutStrLn, stderr )
import System.IO.MMap( mmapFileForeignPtr, Mode(..) )
import System.Directory( doesFileExist, getCurrentDirectory )
import System.Directory( renameFile )
import System.FilePath( (<.>) )

import qualified System.Posix.Files as F ( fileID )
import System.FilePath ( (</>) )
import qualified System.Posix.Files as F
    ( modificationTimeHiRes, fileSize, isDirectory, isRegularFile, isSymbolicLink
    , FileStatus
    )
import System.Posix.Types ( FileID, FileOffset )

--------------------------
-- Indexed trees
--

-- | Description of a a single indexed item. The structure itself does not
-- contain any data, just pointers to the underlying mmap (bytestring is a
-- pointer + offset + length).
--
-- The structure is recursive-ish (as opposed to flat-ish structure, which is
-- used by git...) It turns out that it's hard to efficiently read a flat index
-- with our internal data structures -- we need to turn the flat index into a
-- recursive Tree object, which is rather expensive... As a bonus, we can also
-- efficiently implement subtree queries this way (cf. 'openIndex').
data Item = Item { iBase :: !(Ptr ())
                 , iType :: !ItemType
                 , iName :: !(Maybe Name)
                 } deriving Show

index_version :: BS.ShortByteString
index_version = BS.toShort (BC.pack "HSI9")

-- | Stored to the index to verify we are on the same endianness when reading
-- it back. We will treat the index as invalid in this case so user code will
-- regenerate it.
index_endianness_indicator :: Int32
index_endianness_indicator = 1

size_header, size_magic, size_endianness_indicator :: Int
size_magic = 4 -- the magic word, first 4 bytes of the index
size_endianness_indicator = 4 -- second 4 bytes of the index
size_header = size_magic + size_endianness_indicator

size_size, size_aux, size_fileid, size_hash, size_namelen :: Int
size_size = 8 -- file/directory size (Int64)
size_aux = 8 -- aux (Int64)
size_fileid = 8 -- fileid (inode or fhandle FileID)
size_hash = 32 -- hash representation
size_namelen = 4 -- this many bytes store the length of the name
size_type, size_null :: Int
size_type = 1 -- ItemType: 'D' for directory, 'F' for file
size_null = 1 -- null byte at the end of name

off_size, off_aux, off_fileid, off_hash, off_namelen, off_type, off_name :: Int
off_size = 0
off_aux = off_size + size_size
off_fileid = off_aux + size_aux
off_hash = off_fileid + size_fileid
off_namelen = off_hash + size_hash
off_type = off_namelen + size_namelen
off_name = off_type + size_type

itemAllocSize :: Maybe Name -> Int
itemAllocSize mname =
  align 4 $
    size_size + size_aux + size_fileid + size_hash + size_namelen +
    size_type + nameLength mname + size_null

nameLength :: Maybe Name -> Int
nameLength Nothing = 0
nameLength (Just name) = BS.length (unName name)

itemSize :: Item -> Int
itemSize i =
  size_size + size_aux + size_fileid + size_hash + size_namelen +
  size_type + (nameLength $ iName i) + size_null

itemNext :: Item -> Int
itemNext i = align 4 (itemSize i)

iSize, iAux :: Item -> Ptr Int64
iSize i = plusPtr (iBase i) off_size
iAux i = plusPtr (iBase i) off_aux

iFileID :: Item -> Ptr FileID
iFileID i = plusPtr (iBase i) off_fileid

peekHash :: Item -> IO (Maybe Hash)
peekHash i = do
  -- Note that BS.createFromPtr copies the data
  h <- BS.createFromPtr (iBase i `plusPtr` off_hash) size_hash
  return $ if h == nullHash then Nothing else Just (SHA256 h)

nullHash :: BS.ShortByteString
nullHash = BS.replicate size_hash 0

type FileStatus = Maybe F.FileStatus

-- We deal with hi res timestamps by noting that the actual resolution is in
-- nanoseconds. If we count the nanoseconds since the epoch we will overflow
-- (1<<63)/(1e9*60*60*24*366) =~ 290 years after the epoch. Comfortable.
modificationTime :: FileStatus -> Int64
modificationTime = maybe 0 (truncate . (*1e9) . F.modificationTimeHiRes)

fileSize :: FileStatus -> FileOffset
fileSize = maybe 0 F.fileSize

isDirectory :: FileStatus -> Bool
isDirectory = maybe False F.isDirectory

isRegularFile :: FileStatus -> Bool
isRegularFile = maybe False F.isRegularFile

fileID :: FileStatus -> FileID
fileID = maybe 0 F.fileID

-- | Lay out the basic index item structure in memory. The memory location is
-- given by a ForeignPointer () and an offset. The path and type given are
-- written out, and a corresponding Item is given back. The remaining bits of
-- the item can be filled out using the various update functions.
createItem :: ItemType -> Maybe Name -> ForeignPtr () -> Int -> IO Item
createItem typ mname fp off = do
  let namelen = nameLength mname
  withForeignPtr fp $ \p -> do
    pokeByteOff p (off + off_namelen) (fromIntegral namelen :: Int32)
    pokeByteOff p (off + off_type) $
      c2w $ case typ of TreeType -> 'D'; BlobType -> 'F'
    case mname of
      Just name ->
        BS.copyToPtr (unName name) 0 (p `plusPtr` (off + off_name)) namelen
      Nothing -> return ()
    pokeByteOff p (off + off_name + namelen) (0 :: Word8)
  peekItem fp off

-- | Read the on-disk representation into internal data structure.
--
-- See the module-level section /Index format/ for details on how the index
-- is structured.
peekItem :: ForeignPtr () -> Int -> IO Item
peekItem fp off =
  withForeignPtr fp $ \p -> do
    let iBase = p `plusPtr` off
    namelen :: Int32 <- peekByteOff iBase off_namelen
    iType <- do
      tc <- peekByteOff iBase off_type
      case w2c tc of
        'D' -> return TreeType
        'F' -> return BlobType
        _ -> fail $ "Corrupt index: item type is " ++ show tc ++ ", must be 'D' or 'F'"
    iName <-
      if namelen == 0 then
        return Nothing
      else do
        rname <-
          BS.createFromPtr
            (iBase `plusPtr` off_name) (fromIntegral namelen)
        case rawMakeName rname of
          Left e -> fail e
          Right n -> return (Just n)
    return $! Item {..}

-- | Update an existing 'Item' with new size and hash. The hash must be
-- not be 'Nothing'.
updateItem :: Item -> Int64 -> Hash -> IO ()
updateItem item size (SHA256 hash) = do
  poke (iSize item) size
  BS.copyToPtr hash 0 (iBase item `plusPtr` off_hash) (BS.length hash)

updateFileID :: Item -> FileID -> IO ()
updateFileID item fileid = poke (iFileID item) fileid

updateAux :: Item -> Int64 -> IO ()
updateAux item aux = poke (iAux item) aux

updateTime :: Item -> Int64 -> IO ()
updateTime item mtime = updateAux item mtime

-- | Gives a ForeignPtr to mmapped index, which can be used for reading and
-- updates. The req_size parameter, if non-0, expresses the requested size of
-- the index file. mmapIndex will grow the index if it is smaller than this.
mmapIndex :: forall a. FilePath -> Int -> IO (ForeignPtr a, Int)
mmapIndex indexpath req_size = do
  act_size <- fromIntegral . fileSize <$> Darcs.Util.File.getFileStatus indexpath
  let size = case req_size > 0 of
        True -> req_size
        False | act_size >= size_header -> act_size - size_header
              | otherwise -> 0
  -- TODO Use WriteCopy mode as a fallback if ReadWriteEx fails with a
  -- permission error. This would let us handle read-only repos more
  -- transparently.
  case size of
    0 -> return (castForeignPtr nullForeignPtr, size)
    _ -> do (x, _, _) <- mmapFileForeignPtr indexpath
                                            ReadWriteEx (Just (0, size + size_header))
            return (x, size)

data IndexM m = Index { mmap :: (ForeignPtr ())
                      , basedir :: FilePath
                      , predicate :: AnchoredPath -> TreeItem m -> Bool }
              | EmptyIndex

type Index = IndexM IO

-- FIXME This is not really a state: we modify it only when we recurse
-- down into a dir item, so this is rather more like an environment.
-- Instead of passing it explicitly we could use ReaderT.

-- | When we traverse the index, we keep track of some data about the
-- current parent directory.
data State = State
  { path :: !AnchoredPath -- ^ path of the current directory
  , start :: !Int         -- ^ offset of current directory in the index
  }

data Result a = Result
  { next :: !Int
  -- ^ Position of the next item, in bytes.
  , resitem :: !Item
  -- ^ The item we traversed.
  , goal :: a
  -- ^ The actual ultimate result.
  }

-- * Reading items from the index

data ResultR = ResultR
  { changed :: !Bool
  -- ^ Whether item has changed since the last update to the index.
  , treeitem :: !(Maybe (TreeItem IO))
  -- ^ Nothing in case of the item doesn't exist in the tree
  -- or is filtered by a FilterTree. Or a TreeItem otherwise.
  }

fsPath :: AnchoredPath -> FilePath
fsPath = decodeLocale . flatten

-- | 'Maybe' append a 'Name' to an 'AnchoredPath'.
maybeAppendName :: AnchoredPath -> Maybe Name -> AnchoredPath
maybeAppendName parent = maybe parent (parent `appendPath`)

-- | Calculate the next 'State' when entering an 'Item'.
substateof :: Item -> State -> State
substateof item state =
  state
    { start = start state + itemNext item
    , path = path state `maybeAppendName` iName item
    }

-- * Reading (only) file IDs from the index

-- | Return a list containing all the file/folder names in an index, with
-- their respective ItemType and FileID.
listFileIDs :: Index -> IO ([((AnchoredPath, ItemType), FileID)])
listFileIDs = traverseIndex job []
  where
    job path item subgoals = do
      fileid <- peek $ iFileID item
      let mygoal = ((path, iType item), fileid)
      return $ mygoal : concatMap snd subgoals

traverseIndex
  :: (AnchoredPath -> Item -> [(Name,a)] -> IO a) -> a -> Index -> IO a
traverseIndex _ z EmptyIndex = return z
traverseIndex job _ index = goal <$> traverseItem initial
  where
    initial = State {start = size_header, path = anchoredRoot}
    traverseItem state = do
      item <- peekItem (mmap index) (start state)
      let substate = substateof item state
      case iType item of
       TreeType -> do
        following <- fromIntegral <$> peek (iAux item)
        -- FIXME
        -- The way we feed an undefined Stub to the predicate here looks fishy
        -- but works in practice, since the predicate is a pure function and
        -- thus would normally not evaluate the action that creates the tree. A
        -- somewhat safer version would use @return emptyTree@ instead.
        --
        -- However, it /is/ a problem that we pass it a Stub in the first place
        -- and not the actual item. Which means the predicate goes wrong if its
        -- decision is based on the actual tree item and not only on the path.
        -- This is pretty (w)hacky if you ask me. It works only because in
        -- practice, the predicates we use are all based only on the path. We
        -- should make this clear by removing its TreeItem argument.
        let want = (predicate index) (path substate) (Stub undefined Nothing)
            subs off =
              case compare off following of
                LT -> do
                  result <- traverseItem (substate {start = off})
                  rest <- subs $ next result
                  return $! (iName (resitem result), result) : rest
                EQ -> return []
                GT -> do
                  fail $
                    "Offset mismatch at " ++ show off ++ " (ends at " ++
                        show following ++ ")"
        -- FIXME If the item does not exist as a file or directory on disk,
        -- we still traverse all its subitems recursively. Find a way to
        -- avoid that w/o calling getFileStatus here and again in the job.
        subgoals <- if want then subs (start substate) else return []
        -- Note the partial pattern match on 'Just n' below is justified
        -- as we are traversing sub items here, which means 'Nothing' is
        -- impossible, see 'substateof' for details.
        --
        -- FIXME Why do we even wrap the returned item in a Maybe? It seems
        -- we never actually distinguish between Just item and Nothing anywhere.
        goal' <- job (path substate) item [(n,goal s) | (Just n, s) <- subgoals]
        return $ Result
          { next = following
          , resitem = item
          , goal = goal'
          }
       BlobType -> do
        goal' <- job (path substate) item []
        return $ Result
          { next = start state + itemNext item
          , resitem = item
          , goal = goal'
          }

-- * Reading and writing 'Tree's from/to the index

-- | Initialize an 'Index' from the given index file.
openIndex :: FilePath -> IO Index
openIndex indexpath = do
  (mmap_ptr, mmap_size) <- mmapIndex indexpath 0
  base <- getCurrentDirectory
  return $ if mmap_size == 0 then EmptyIndex
                             else Index { mmap = mmap_ptr
                                        , basedir = base
                                        , predicate = \_ _ -> True }

-- | Traverse the 2nd (@reference@) 'Tree' and write its items to a new index.
-- However, item hashes are taken from the 1st (@old@) 'Tree' (which actually
-- comes from the old index, but this is not made clear by the API), while
-- all other fields are taken from the current file status. So @reference@
-- merely provides the tree shape i.e. the items it has.
--
-- Precondition: The @old@ Tree must have hashes for all its items.
--
-- TODO This is probably less efficient than directly copying items from the
-- old index. See the TODO below in the code.
formatIndex :: ForeignPtr () -> Tree IO -> Tree IO -> IO ()
formatIndex mmap_ptr old reference =
    do _ <- create (SubTree reference) (State anchoredRoot size_header)
       withForeignPtr mmap_ptr $ \ptr -> do
         BS.copyToPtr index_version 0 ptr (BS.length index_version)
         pokeByteOff ptr size_magic index_endianness_indicator
    where basename (AnchoredPath ns) =
            case ns of
              [] -> Nothing
              _ -> Just (last ns)
          create (File _) State{..} =
               do i <- createItem BlobType (basename path) mmap_ptr start
                  -- TODO calling getFileStatus here is both slightly
                  -- inefficient and slightly race-prone
                  st <- getFileStatus (fsPath path)
                  updateFileID i (fileID st)
                  case find old path of
                    Nothing -> return ()
                    Just ti -> do let hash = itemHash ti
                                      mtime = modificationTime st
                                      size = fileSize st
                                  -- isNothing hash is impossible by precondition
                                  updateItem i (fromIntegral size) (fromJust hash)
                                  updateTime i mtime
                  return $ start + itemNext i
          create (SubTree t) State{..} =
               do i <- createItem TreeType (basename path) mmap_ptr start
                  st <- getFileStatus (fsPath path)
                  updateFileID i (fileID st)
                  case find old path of
                    Nothing -> return ()
                    Just ti ->
                      case itemHash ti of
                        Nothing -> return ()
                        Just h -> updateItem i 0 h
                  let subs [] = return $ start + itemNext i
                      subs ((name,x):xs) = do
                        let subpath = path `appendPath` name
                        substart <- subs xs
                        create x State{path=subpath,start=substart}
                  lastOff <- subs (listImmediate t)
                  poke (iAux i) (fromIntegral lastOff)
                  return lastOff
          create (Stub _ _) State{..} =
               fail $ "Cannot create index from stubbed Tree at " ++ show path

-- | Add and remove entries in the given 'Index' to make it match the given
-- 'Tree'. If an object in the 'Tree' does not exist in the current working
-- directory, its index entry will have zero hash, size, aux, and fileID. For
-- the hash this translates to 'Nothing', see 'peekHash'.
updateIndexFrom :: FilePath -> Tree IO -> IO Index
updateIndexFrom indexpath ref =
    do debugMessage "Updating the index ..."
       old_tree <- treeFromIndex =<< openIndex indexpath
       reference <- expand ref
       let len_root = itemAllocSize Nothing
           len = len_root + sum [ itemAllocSize (Just (last p)) | (AnchoredPath p, _) <- list reference ]
       exist <- doesFileExist indexpath
       -- Note that the file is still open via the mmaped pointer in
       -- the open index, and we /are/ going to write the index using
       -- that pointer. If we could rely on posix semantics,
       -- we would just delete the file. However, on windows this
       -- would fail, so instead we rename it.
       when exist $ renameFile indexpath (indexpath <.> "old")
       (mmap_ptr, _) <- mmapIndex indexpath len
       -- Precondition of formatIndex is given by the postcondition of treeFromIndex.
       formatIndex mmap_ptr old_tree reference
       debugMessage "Done updating the index, reopening it ..."
       openIndex indexpath

-- | Read an 'Index', starting with the root, to create a 'Tree'.
-- It is guaranteed that the resulting Tree has hashes for all its items.
treeFromIndex :: Index -> IO (Tree IO)
treeFromIndex index = do
    beginTedious progressKey
    let emptyR = ResultR { changed = False, treeitem = Just (SubTree emptyTree)}
    res <- traverseIndex processItem emptyR index
    endTedious progressKey
    case treeitem res of
      Just (SubTree tree) -> return $ filter (predicate index) tree
      _ -> fail "Unexpected failure in treeFromIndex!"

  where
    -- This is not a typo! As a side-effect of reading a tree from the
    -- index, it also gets updated and this is what can take a long time
    -- since it may involve reading all files in the working tree that
    -- are also in pristine+pending (to compute their hashes)
    progressKey = "Updating the index"

    processItem :: AnchoredPath -> Item -> [(Name, ResultR)] -> IO ResultR
    processItem path item subgoals = do
      let fpath = fsPath path
      finishedOneIO progressKey fpath
      st <- getFileStatus fpath
      fileid <- peek $ iFileID item
      when (fileid == 0) $ updateFileID item (fileID st)
      oldhash <- peekHash item
      case iType item of
        TreeType -> do
          let exists = isDirectory st
              want = exists && (predicate index) path (Stub undefined Nothing)
              inferiors = if want then subgoals else []
              we_changed = isNothing oldhash || or [changed x | (_, x) <- inferiors]
              tree' = makeTree [(n, s) | (n, ResultR _ (Just s)) <- inferiors]
              -- fromJust is justified because isNothing oldhash implies we_changed
              treehash = if we_changed then darcsTreeHash tree' else fromJust oldhash
              tree = tree' {treeHash = Just treehash}
          when (exists && we_changed) $ updateItem item 0 treehash
          return $ ResultR
            { changed = not exists || we_changed
            , treeitem = if want then Just $ SubTree tree else Nothing
            }
        BlobType -> do
          mtime <- fromIntegral <$> (peek $ iAux item)
          size <- peek $ iSize item
          let mtime' = modificationTime st
              size' = fromIntegral $ fileSize st
              -- This is the only place where basedir is used.
              -- It is needed because --external-merge changes to a
              -- temporary directory before writing the Tree there.
              --
              -- TODO We should save it as a hashed file and redirect the
              -- readblob accordingly. This would automatically make it
              -- independent of the current directory, providing a hashed
              -- snapshot of the (known) working tree state.
              readblob = readSegment (basedir index </> fpath, Nothing)
              exists = isRegularFile st
              we_changed = isNothing oldhash || mtime /= mtime' || size /= size'
          when (exists && we_changed) $ do
            hash' <- sha256 `fmap` readblob
            updateItem item size' hash'
            updateTime item mtime'
          newhash <- peekHash item
          return $ ResultR
            { changed = not exists || we_changed
            , treeitem =
                if exists then Just $ File $ Blob readblob newhash else Nothing
            }

-- | Check that a given file is an index file with a format we can handle. You
-- should remove and re-create the index whenever this is not true.
indexFormatValid :: FilePath -> IO Bool
indexFormatValid path' =
  do
    (start, _, _) <- mmapFileForeignPtr path' ReadOnly (Just (0, size_header))
    withForeignPtr start $ \ptr -> do
      magic <- BS.createFromPtr ptr 4
      endianness_indicator <- peekByteOff ptr 4
      return $
        index_version == magic && index_endianness_indicator == endianness_indicator
  `catch` \(_::SomeException) -> return False

checkIndexFormat :: Index -> IO Bool
checkIndexFormat EmptyIndex = return True
checkIndexFormat Index{..} =
  withForeignPtr mmap $ \ptr -> do
    magic <- BS.createFromPtr ptr 4
    endianness_indicator <- peekByteOff ptr 4
    return $
      index_version == magic && index_endianness_indicator == endianness_indicator

instance FilterTree IndexM IO where
    filter _ EmptyIndex = EmptyIndex
    filter p index = index { predicate = \a b -> predicate index a b && p a b }


-- * Getting the file ID from a path

-- | For a given path, get the corresponding fileID from the filesystem.
getFileID :: AnchoredPath -> IO (Maybe FileID)
getFileID p = fmap F.fileID <$> getFileStatus (realPath p)

-- * Low-level utilities

align :: Integral a => a -> a -> a
align boundary i = case i `rem` boundary of
                     0 -> i
                     x -> i + boundary - x
{-# INLINE align #-}

getFileStatus :: FilePath -> IO FileStatus
getFileStatus path = do
  mst <- Darcs.Util.File.getFileStatus path
  case mst of
    Just st
      | F.isSymbolicLink st -> do
          hPutStrLn stderr $ "Warning: ignoring symbolic link " ++ path
          return Nothing
    _ -> return mst

data IndexEntry = IndexEntry
  { ieSize :: Int64
  , ieAux :: Int64
  , ieFileID :: FileID
  , ieHash :: Maybe Hash
  , ieType :: Char
  , iePath :: AnchoredPath
  }

dumpIndex :: FilePath -> IO [IndexEntry]
dumpIndex indexpath = do
    index <- openIndex indexpath
    valid <- checkIndexFormat index
    if valid then
      traverseIndex processItem [] index
    else
      fail "Index has invalid format"
  where
    processItem path item subgoals = do
      ieSize <- peek $ iSize item
      ieAux <- peek $ iAux item
      ieFileID <- peek $ iFileID item
      ieHash <- peekHash item
      let ieType = case iType item of TreeType -> 'D'; BlobType -> 'F'
          iePath = path
          thisEntry = IndexEntry{..}
          subEntries = concatMap snd subgoals
      return $ thisEntry : subEntries
