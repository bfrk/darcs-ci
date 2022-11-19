module Darcs.Util.Tree.Apply
    ( ApplyTree
    , initTree
    , virtualTree
    , flush
      -- * Read actions
    , readFile
    , exists
    , directoryExists
    , fileExists
      -- * Write actions
    , writeFile
    , createDirectory
    , unlink
    , rename
    , copy
      -- * Other actions
    , findM, findFileM, findTreeM
    ) where

import Darcs.Prelude hiding ( readFile, writeFile )

import Darcs.Util.Path ( AnchoredPath, anchoredRoot, displayPath, movedirfilename )
import Darcs.Util.Tree

import Data.List( sortBy )
import Data.Int( Int64 )
import Data.Maybe( isNothing, isJust )

import qualified Data.ByteString.Lazy as BL
import Control.Monad.Catch ( MonadThrow(..) )
import Control.Monad.State.Strict
import qualified Data.Map as M

import System.IO.Error ( ioeSetErrorString, mkIOError )
import GHC.IO.Exception ( IOErrorType(..) )

-- | Size and age of changes to a 'TreeItem'
data Change = Change { size :: Int64, age :: Int64 }

-- | A 'Tree', augmented with information about changes that haven't been
-- flushed to disk yet.
data ApplyTree m wX = ApplyTree
  { tree :: !(Tree m)
  , changed :: !(M.Map AnchoredPath Change)
  , changesize :: !Int64
  , maxage :: !Int64
  , dump :: DumpItem m
  }

-- | A procedure for dumping a single 'TreeItem' to disk. If the implementation
-- uses item 'Hash'es, it is also responsible to ensure that its 'Hash' is
-- up-to-date. It is not allowed to make any changes to the actual content of
-- the 'TreeItem'.
type DumpItem m = AnchoredPath -> TreeItem m -> m (TreeItem m)

initTree :: Tree m -> DumpItem m -> ApplyTree m wX
initTree t d =
  ApplyTree {tree = t, changed = M.empty, changesize = 0, maxage = 0, dump = d}

virtualTree :: Monad m => Tree m -> ApplyTree m wX
virtualTree t = initTree t (const return)

-- | Completely flush an ApplyTree it to disk, returning the tree within.
flush :: Monad m => ApplyTree m wX -> m (Tree m)
flush t = do
  let changedPaths = M.keys (changed t)
      dirs' = [path | (path, SubTree _) <- list (tree t)]
  tree <$> foldM (flip flushItem) t (changedPaths ++ dirs' ++ [anchoredRoot])

-- | Modifies an item in the current Tree. This action keeps an account of the
-- modified data, in changed and changesize, for subsequent flush
-- operations. Any modifications (as in "modifyTree") are allowed.
modifyItem
  :: Monad m => AnchoredPath -> Maybe (TreeItem m) -> ApplyTree m wX -> m (ApplyTree m wY)
modifyItem path item applyTree@ApplyTree {..} = do
  let getsize (Just (File b)) = BL.length <$> readBlob b
      getsize _ = return 0
  newsize <- getsize item
  let diff =
        case M.lookup path changed of
          Nothing -> newsize
          Just Change{size = oldsize} ->
            -- modifyTree only ever increases the size of
            -- the accumulated changes! Even if we apply
            -- a large hunk and its inverse, we still have
            -- to account for both changes (i.e. twice).
            abs (oldsize - newsize)
  return
    applyTree
      { tree = modifyTree tree path item
      , changed = M.insert path (Change newsize maxage) changed
      , maxage = maxage + 1
      , changesize = changesize + diff
      }

renameChanged
  :: Monad m => AnchoredPath -> AnchoredPath -> ApplyTree m wX -> ApplyTree m wY
renameChanged from to t@ApplyTree {..} = t {changed = rename' changed}
  where
    rename' = M.mapKeys (movedirfilename from to)

-- | Replace an item with a new version without modifying the content of the
-- tree. This does not do any change tracking. Ought to be only used from a
-- 'sync' implementation for a particular storage format. The presumed use-case
-- is that an existing in-memory Blob is replaced with a one referring to an
-- on-disk file.
replaceItem :: Monad m => AnchoredPath -> TreeItem m -> ApplyTree m wX -> ApplyTree m wX
replaceItem path item applyTree@ApplyTree {..} =
  applyTree {tree = modifyTree tree path (Just item)}

-- | Flush a single item to disk.
flushItem :: Monad m => AnchoredPath -> ApplyTree m wX -> m (ApplyTree m wX)
flushItem path applyTree@ApplyTree {..} =
  case find tree path of
    Nothing -> return applyTree -- vanished, do nothing
    Just item -> flip (replaceItem path) applyTree <$> dump path item

-- | If buffers are becoming large, sync, otherwise do nothing.
flushSome :: Monad m => ApplyTree m wX -> m (ApplyTree m wX)
flushSome applyTree@ApplyTree {..} = do
  if changesize > flushSomeSize
    then do
      (remaining, applyTree') <-
        flushOne applyTree $ sortBy cmpAge $ M.toList changed
      return applyTree' {changed = M.fromList remaining}
    else return applyTree
  where
    cmpAge (_, a) (_, b) = compare (age a) (age b)

flushOne
  :: Monad m
  => ApplyTree m wX
  -> [(AnchoredPath, Change)]
  -> m ([(AnchoredPath, Change)], ApplyTree m wX)
flushOne applyTree [] = return ([], applyTree)
flushOne applyTree@ApplyTree {..} ((path, Change{..}):chs) = do
  let changesize' = subtract size changesize
  applyTree' <- flushItem path applyTree
  let applyTree'' = applyTree' {changesize = changesize'}
  if changesize' > flushOneSize
    then flushOne applyTree'' chs
    else return (chs, applyTree'')

megaByte :: Int64
megaByte = 0x100000

flushOneSize :: Int64
flushOneSize = 50 * megaByte

flushSomeSize :: Int64
flushSomeSize = 100 * megaByte

-- read only actions

expandTo :: Monad m => AnchoredPath -> ApplyTree m wX -> m (ApplyTree m wY)
expandTo p t = do
  tree' <- expandPath (tree t) p
  return t {tree = tree'}

findItem :: Monad m => AnchoredPath -> ApplyTree m wX -> m (Maybe (TreeItem m), ApplyTree m wY)
findItem path t = do
  t' <- expandTo path t
  return (find (tree t) path, t')

-- | Check for existence of a file.
fileExists :: Monad m => AnchoredPath -> ApplyTree m wX -> m Bool
fileExists p t = do
  item <- fst <$> findItem p t
  case item of
    Just (File{}) -> return True
    _ -> return False

-- | Check for existence of a directory.
directoryExists :: Monad m => AnchoredPath -> ApplyTree m wX -> m Bool
directoryExists p t = do
  item <- fst <$> findItem p t
  case item of
    Just (SubTree{}) -> return True
    _ -> return False

-- | Check for existence of a node (file or directory, doesn't matter).
exists :: MonadThrow m => AnchoredPath -> ApplyTree m wX -> m Bool
exists p t = isJust . fst <$> findItem p t

-- | Grab content of a file in the current Tree at the given path.
readFile :: MonadThrow m => AnchoredPath -> ApplyTree m wX -> m (BL.ByteString, ApplyTree m wY)
readFile p t = do
  (f, t') <- findItem p t
  case f of
    Just (File x) -> (,t') <$> readBlob x
    Just _ ->
      throwM $
        flip ioeSetErrorString "is a directory" $
        mkIOError InappropriateType "readFile" Nothing (Just (displayPath p))
    Nothing ->
      throwM $
        mkIOError NoSuchThing "readFile" Nothing (Just (displayPath p))

-- | Change content of a file at a given path. The change will be
-- eventually flushed to disk, but might be buffered for some time.
writeFile :: MonadThrow m => AnchoredPath -> BL.ByteString -> ApplyTree m wX -> m (ApplyTree m wY)
writeFile p con _t = do
  (item, _t) <- findItem p _t
  _t <-
    case item of
      Just (SubTree _) ->
        throwM $
        flip ioeSetErrorString "is a directory" $
        mkIOError InappropriateType "writeFile" Nothing (Just (displayPath p))
      _ ->
        -- note that writing to a non-existing file is allowed,
        -- in fact there is no primitive for creating a file
        modifyItem p (Just blob) _t
  flushSome _t
  where
    blob = File $ Blob (return con) Nothing
    -- we would like to say "sha256 con" here, but due to strictness of Hash in
    -- Blob, this would often lead to unnecessary computation which would then
    -- be discarded anyway; we rely on the sync implementation to fix up any
    -- Nothing occurrences

-- | Create a directory.
createDirectory :: Monad m => AnchoredPath -> ApplyTree m wX -> m (ApplyTree m wY)
createDirectory p t = do
  t' <- expandTo p t
  modifyItem p (Just $ SubTree emptyTree) t'

-- | Remove the item at a path.
unlink :: Monad m => AnchoredPath -> ApplyTree m wX -> m (ApplyTree m wY)
unlink p t = do
  t' <- expandTo p t
  modifyItem p Nothing t'

-- | Rename the item at a path.
rename :: MonadThrow m => AnchoredPath -> AnchoredPath -> ApplyTree m wX -> m (ApplyTree m wY)
rename from to _t = do
  (item, _t) <- findItem from _t
  (found_to, _t) <- findItem to _t
  unless (isNothing found_to) $
    throwM $
      mkIOError NoSuchThing "rename" Nothing (Just (displayPath from))
  unless (isJust item) $
    throwM $
      mkIOError AlreadyExists "rename" Nothing (Just (displayPath to))
  _t <- modifyItem from Nothing _t
  _t <- modifyItem to item _t
  return $ renameChanged from to _t

-- | Copy an item from some path to another path.
-- Doing this with a SubTree is weird... it means copy recursively,
-- but with lazy copy-on-write semantics. What happens when we flush that?
-- Seems to work, though, as it is used in Darcs.UI.Commands.Convert.Import
copy :: MonadThrow m => AnchoredPath -> AnchoredPath -> ApplyTree m wX -> m (ApplyTree m wY)
copy from to t = do
  (item, t') <- findItem from =<< expandTo to t
  when (isNothing item) $
    throwM $
    mkIOError NoSuchThing "copy" Nothing (Just (displayPath from))
  modifyItem to item t'

findM' :: forall m a . Monad m
       => (Tree m -> AnchoredPath -> a) -> Tree m -> AnchoredPath -> m a
findM' what t path = flip what path <$> expandPath t path

findM :: Monad m => Tree m -> AnchoredPath -> m (Maybe (TreeItem m))
findM = findM' find

findTreeM :: Monad m => Tree m -> AnchoredPath -> m (Maybe (Tree m))
findTreeM = findM' findTree

findFileM :: Monad m => Tree m -> AnchoredPath -> m (Maybe (Blob m))
findFileM = findM' findFile
