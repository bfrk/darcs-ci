--  Copyright (C) 2009-2011 Petr Rockai
--
--  BSD3

{- | A monad transformer for 'Tree' mutation. The main idea is to simulate
IO-ish manipulation of real filesystem (that's the state part of the monad),
and to keep memory usage down by reasonably often dumping the intermediate
data to disk and forgetting it.

The implementation is configured by passing a procedure of type 'DumpItem'
to 'runTreeMonad'.

This module provides the readily configured 'virtualTreeIO' that never
writes any changes, but may trigger filesystem reads as appropriate. -}

module Darcs.Util.Tree.Monad
    ( -- * 'TreeMonad'
      TreeMonad
    , TreeState(tree)
    , runTreeMonad
    , virtualTreeMonad
      -- * Specializing to 'IO'
    , TreeIO
    , virtualTreeIO
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
import Control.Monad ( forM_, unless, when )
import Control.Monad.Catch ( MonadThrow(..) )
import Control.Monad.RWS.Strict ( RWST(..), ask, lift, gets, modify )
import qualified Data.Map as M

import System.IO.Error ( ioeSetErrorString, mkIOError )
import GHC.IO.Exception ( IOErrorType(..) )

-- | Keep track of the size and age of changes to the tree.
type Changed = M.Map AnchoredPath (Int64, Int64) -- size, age

-- | Internal state of the 'TreeMonad'. Keeps track of the current 'Tree'
-- content and unsync'd changes.
data TreeState m = TreeState
  { tree :: !(Tree m)
  , changed :: !Changed
  , changesize :: !Int64
  , maxage :: !Int64
  }

-- | A procedure for dumping a single 'TreeItem' to disk. If the implementation
-- uses item 'Hash'es, it is also responsible to ensure that its 'Hash' is
-- up-to-date. It is not allowed to make any changes to the actual content of
-- the 'TreeItem'.
type DumpItem m = AnchoredPath -> TreeItem m -> m (TreeItem m)

-- | A monad transformer that adds state of type 'TreeState' and an environment
-- of type 'DumpItem'.
type TreeMonad m = RWST (DumpItem m) () (TreeState m) m

-- | 'TreeMonad' specialized to 'IO'
type TreeIO = TreeMonad IO

initialState :: Tree m -> TreeState m
initialState t =
  TreeState {tree = t, changed = M.empty, changesize = 0, maxage = 0}

flush :: Monad m => TreeMonad m ()
flush = do changed' <- map fst . M.toList <$> gets changed
           dirs' <- gets tree >>= \t -> return [ path | (path, SubTree _) <- list t ]
           modify $ \st -> st { changed = M.empty, changesize = 0 }
           forM_ (changed' ++ dirs' ++ [anchoredRoot]) flushItem

runTreeMonad' :: Monad m => TreeMonad m a -> DumpItem m -> TreeState m -> m (a, Tree m)
runTreeMonad' action initEnv initState = do
  (out, final, _) <- runRWST action initEnv initState
  return (out, tree final)

runTreeMonad :: Monad m => TreeMonad m a -> Tree m -> DumpItem m -> m (a, Tree m)
runTreeMonad action t dump = do
  let action' = do x <- action
                   flush
                   return x
  runTreeMonad' action' dump (initialState t)

-- | Run a 'TreeMonad' action without storing any changes. This is useful for
-- running monadic tree mutations for obtaining the resulting 'Tree' (as opposed
-- to their effect of writing a modified tree to disk). The actions can do both
-- read and write -- reads are passed through to the actual filesystem, but the
-- writes are held in memory in the form of a modified 'Tree'.
virtualTreeMonad :: Monad m => TreeMonad m a -> Tree m -> m (a, Tree m)
virtualTreeMonad action t = runTreeMonad action t (const return)

-- | 'virtualTreeMonad' specialized to 'IO'
virtualTreeIO :: TreeIO a -> Tree IO -> IO (a, Tree IO)
virtualTreeIO = virtualTreeMonad

-- | Modifies an item in the current Tree. This action keeps an account of the
-- modified data, in changed and changesize, for subsequent flush
-- operations. Any modifications (as in "modifyTree") are allowed.
modifyItem :: Monad m
            => AnchoredPath -> Maybe (TreeItem m) -> TreeMonad m ()
modifyItem path item = do
  age <- gets maxage
  changed' <- gets changed
  let getsize (Just (File b)) = lift (BL.length <$> readBlob b)
      getsize _ = return 0
  size <- getsize item
  let change = case M.lookup path changed' of
        Nothing -> size
        Just (oldsize, _) -> size - oldsize

  modify $ \st -> st { tree = modifyTree (tree st) path item
                     , changed = M.insert path (size, age) (changed st)
                     , maxage = age + 1
                     , changesize = changesize st + change }

renameChanged :: Monad m
              => AnchoredPath -> AnchoredPath -> TreeMonad m ()
renameChanged from to = modify $ \st -> st {changed = rename' $ changed st}
  where
    rename' = M.mapKeys (movedirfilename from to)

-- | Replace an item with a new version without modifying the content of the
-- tree. This does not do any change tracking. Ought to be only used from a
-- 'sync' implementation for a particular storage format. The presumed use-case
-- is that an existing in-memory Blob is replaced with a one referring to an
-- on-disk file.
replaceItem :: Monad m
            => AnchoredPath -> TreeItem m -> TreeMonad m ()
replaceItem path item = do
  modify $ \st -> st { tree = modifyTree (tree st) path (Just item) }

-- | Flush a single item to disk. This is the only procedure that uses the
-- Reader part of the environment (the procedure of type @'DumpItem' m@).
flushItem :: forall m . Monad m => AnchoredPath -> TreeMonad m ()
flushItem path = do
  current <- gets tree
  dumpItem <- ask
  case find current path of
    Nothing -> return () -- vanished, do nothing
    Just item -> lift (dumpItem path item) >>= replaceItem path

-- | If buffers are becoming large, sync, otherwise do nothing.
flushSome :: Monad m => TreeMonad m ()
flushSome = do x <- gets changesize
               when (x > megs 100) $ do
                 remaining <- go =<< sortBy age . M.toList <$> gets changed
                 modify $ \s -> s { changed = M.fromList remaining }
  where go [] = return []
        go ((path, (size, _)):chs) = do
          x <- subtract size <$> gets changesize
          flushItem path
          modify $ \s -> s { changesize = x }
          if  x > megs 50  then go chs
                           else return chs
        megs = (* (1024 * 1024))
        age (_, (_, a)) (_, (_, b)) = compare a b

-- read only actions

expandTo :: Monad m => AnchoredPath -> TreeMonad m ()
expandTo p =
    do t <- gets tree
       t' <- lift $ expandPath t p
       modify $ \st -> st { tree = t' }

findItem :: Monad m => AnchoredPath -> TreeMonad m (Maybe (TreeItem m))
findItem path = do
  expandTo path
  tr <- gets tree
  return $ find tr path

-- | Check for existence of a file.
fileExists :: Monad m => AnchoredPath -> TreeMonad m Bool
fileExists p = do
  item <- findItem p
  case item of
    Just (File{}) -> return True
    _ -> return False

-- | Check for existence of a directory.
directoryExists :: Monad m => AnchoredPath -> TreeMonad m Bool
directoryExists p = do
  item <- findItem p
  case item of
    Just (SubTree{}) -> return True
    _ -> return False

-- | Check for existence of a node (file or directory, doesn't matter).
exists :: MonadThrow m => AnchoredPath -> TreeMonad m Bool
exists p = isJust <$> findItem p

-- | Grab content of a file in the current Tree at the given path.
readFile :: MonadThrow m => AnchoredPath -> TreeMonad m BL.ByteString
readFile p = do
  f <- findItem p
  case f of
    Just (File x) -> lift (readBlob x)
    Just _ ->
      throwM $
        flip ioeSetErrorString "is a directory" $
        mkIOError InappropriateType "readFile" Nothing (Just (displayPath p))
    Nothing ->
      throwM $
        mkIOError NoSuchThing "readFile" Nothing (Just (displayPath p))

-- | Change content of a file at a given path. The change will be
-- eventually flushed to disk, but might be buffered for some time.
writeFile :: MonadThrow m => AnchoredPath -> BL.ByteString -> TreeMonad m ()
writeFile p con = do
  item <- findItem p
  case item of
    Just (SubTree _) ->
      throwM $
      flip ioeSetErrorString "is a directory" $
      mkIOError InappropriateType "writeFile" Nothing (Just (displayPath p))
    _ ->
      -- note that writing to a non-existing file is allowed,
      -- in fact there is no primitive for creating a file
      modifyItem p (Just blob)
  flushSome
  where
    blob = File $ Blob (return con) Nothing
    -- we would like to say "sha256 con" here, but due to strictness of Hash in
    -- Blob, this would often lead to unnecessary computation which would then
    -- be discarded anyway; we rely on the sync implementation to fix up any
    -- Nothing occurrences

-- | Create a directory.
createDirectory :: Monad m => AnchoredPath -> TreeMonad m ()
createDirectory p =
    do expandTo p
       modifyItem p $ Just $ SubTree emptyTree

-- | Remove the item at a path.
unlink :: Monad m => AnchoredPath -> TreeMonad m ()
unlink p =
    do expandTo p
       modifyItem p Nothing

-- | Rename the item at a path.
rename :: MonadThrow m => AnchoredPath -> AnchoredPath -> TreeMonad m ()
rename from to = do
  item <- findItem from
  found_to <- findItem to
  unless (isJust item) $
    throwM $
      mkIOError NoSuchThing "rename" Nothing (Just (displayPath from))
  unless (isNothing found_to) $
    throwM $
      mkIOError AlreadyExists "rename" Nothing (Just (displayPath to))
  modifyItem from Nothing
  modifyItem to item
  renameChanged from to

-- | Copy an item from some path to another path.
-- Doing this with a SubTree is weird... it means copy recursively,
-- but with lazy copy-on-write semantics. What happens when we flush that?
-- Seems to work, though, as it is used in Darcs.UI.Commands.Convert.Import
copy :: MonadThrow m => AnchoredPath -> AnchoredPath -> TreeMonad m ()
copy from to = do
  expandTo to
  item <- findItem from
  when (isNothing item) $
    throwM $
    mkIOError NoSuchThing "copy" Nothing (Just (displayPath from))
  modifyItem to item

findM' :: forall m a . Monad m
       => (Tree m -> AnchoredPath -> a) -> Tree m -> AnchoredPath -> m a
findM' what t path = fst <$> virtualTreeMonad (look path) t
  where look :: AnchoredPath -> TreeMonad m a
        look p = expandTo p >> flip what p <$> gets tree

findM :: Monad m => Tree m -> AnchoredPath -> m (Maybe (TreeItem m))
findM = findM' find

findTreeM :: Monad m => Tree m -> AnchoredPath -> m (Maybe (Tree m))
findTreeM = findM' findTree

findFileM :: Monad m => Tree m -> AnchoredPath -> m (Maybe (Blob m))
findFileM = findM' findFile
