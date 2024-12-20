--  Copyright (C) 2009-2011 Petr Rockai
--
--  BSD3
{-# LANGUAGE CPP, ScopedTypeVariables #-}

-- | Mostly internal utilities for use by the rest of the library. Subject to
-- removal without further notice.
module Darcs.Util.ObjectStore.Utils
    ( FileSegment
    , readSegment
    , reachable
    ) where

import Darcs.Prelude

import Control.Exception ( SomeException(..), catch )

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Int ( Int64 )
import qualified Data.Map as M
import Data.Maybe ( catMaybes )
import qualified Data.Set as S

import System.IO ( IOMode(ReadMode), SeekMode(AbsoluteSeek), hSeek, withFile )
import System.Mem ( performGC )
import System.Posix.Files ( fileSize, getFileStatus )

-- | Pointer to a file, possibly with start/end offsets. Supposed to be
-- fed to (uncurry mmapFileByteString) or similar.
type FileSegment = (FilePath, Maybe (Int64, Int))

-- | Read in a FileSegment into a Lazy ByteString. Implemented using mmap.
readSegment :: FileSegment -> IO BL.ByteString
readSegment (f, range) = do
  bs <-
    tryToRead `catch` \(_ :: SomeException) -> do
      size <- fileSize `fmap` getFileStatus f
      if size == 0
        then return BS8.empty
        else performGC >> tryToRead
  return $ BL.fromChunks [bs]
  where
    tryToRead = do
      case range of
        Nothing -> BS.readFile f
        Just (off, size) ->
          withFile f ReadMode $ \h -> do
            hSeek h AbsoluteSeek $ fromIntegral off
            BS.hGet h size

{-# INLINE readSegment #-}
-- | Find a monadic fixed point of @f@ that is the least above @i@. (Will
-- happily diverge if there is none.)
mfixFrom :: (Eq a, Monad m) => (a -> m a) -> a -> m a
mfixFrom f i = do
  x <- f i
  if x == i
    then return i
    else mfixFrom f x

-- | For a @refs@ function, a @map@ (@key@ -> @value@) and a @rootSet@, find a
-- submap of @map@ such that all items in @map@ are reachable, through @refs@
-- from @rootSet@.
reachable
  :: forall monad key value
   . (Monad monad, Ord key, Eq value)
  => (value -> monad [key])
  -> (key -> monad (Maybe (key, value)))
  -> S.Set key
  -> monad (M.Map key value)
reachable refs lookup rootSet = do
  lookupSet rootSet >>= mfixFrom expand
  where
    lookupSet :: S.Set key -> monad (M.Map key value)
    expand :: M.Map key value -> monad (M.Map key value)
    lookupSet s = do
      list <- mapM lookup (S.toAscList s)
      return $ M.fromAscList (catMaybes list)
    expand from = do
      refd <- concat <$> mapM refs (M.elems from)
      M.union from <$> lookupSet (S.fromList refd)
