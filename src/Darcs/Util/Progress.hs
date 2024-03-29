-- |
-- Module      : Darcs.Util.Progress
-- Copyright   : 2008 David Roundy
-- License     : GPL
-- Maintainer  : darcs-devel@darcs.net
-- Stability   : experimental
-- Portability : portable
--
-- Utility functions for tracking progress of long-running actions.

module Darcs.Util.Progress
    (
      beginTedious
    , endTedious
    , tediousSize
    , withProgress
    , withSizedProgress
    , debugMessage
    , withoutProgress
    , progress
    , progressKeepLatest
    , finishedOne
    , finishedOneIO
    , progressList
    , minlist
    , setProgressMode
    ) where


import Darcs.Prelude

import Control.Arrow ( second )
import Control.Exception ( bracket )
import Control.Monad ( when, void )
import Control.Concurrent ( forkIO, threadDelay )

import Data.Char ( toLower )
import Data.Map ( Map, empty, adjust, insert, delete, lookup )
import Data.Maybe ( isJust )
import Data.IORef ( IORef, newIORef, readIORef, writeIORef, modifyIORef )

import qualified System.Console.Terminal.Size as TS ( size, width )
import System.IO ( hFlush, stdout )
import System.IO.Unsafe ( unsafePerformIO )

import Darcs.Util.Global ( debugMessage )


data ProgressData = ProgressData
    { sofar   :: !Int
    , latest  :: !(Maybe String)
    , total   :: !(Maybe Int)
    }

progressRate :: Int
progressRate = 1000000

handleProgress :: IO ()
handleProgress = do
    threadDelay progressRate
    handleMoreProgress "" 0


handleMoreProgress :: String -> Int -> IO ()
handleMoreProgress k n = withProgressMode $ \m ->
    if m then do s <- getProgressLast
                 mp <- getProgressData s
                 case mp of
                   Nothing -> do
                      threadDelay progressRate
                      handleMoreProgress k n
                   Just p -> do
                      when (k /= s || n < sofar p) $ whenProgressMode $ printProgress s p
                      threadDelay progressRate
                      handleMoreProgress s (sofar p)
         else do threadDelay progressRate
                 handleMoreProgress k n


printProgress :: String
              -> ProgressData
              -> IO ()
printProgress k (ProgressData {sofar=s, total=Just t, latest=Just l}) =
    putCr (k ++ " ... " ++ show s ++ " done, " ++ show (t - s) ++ " queued. " ++ l)
printProgress k (ProgressData {latest=Just l}) =
    putCr (k ++ " ... " ++ l)
printProgress k (ProgressData {sofar=s, total=Just t}) | t >= s =
    putCr (k ++ " ... " ++ show s ++ " done, " ++ show (t - s) ++ " queued")
printProgress k (ProgressData {sofar=s}) =
    putCr (k ++ " ... " ++ show s)

putCr :: String -> IO ()
putCr = unsafePerformIO mkPutCr
{-# NOINLINE putCr #-}

withProgress :: String -> (String -> IO a) -> IO a
withProgress k = bracket (beginTedious k >> return k) endTedious

withSizedProgress :: String -> Int -> (String -> IO a) -> IO a
withSizedProgress k n =
  bracket (beginTedious k >> tediousSize k n >> return k) endTedious

-- | @beginTedious k@ starts a tedious process and registers it in
-- '_progressData' with the key @k@. A tedious process is one for which we want
-- a progress indicator.
--
-- Wouldn't it be safer if it had type String -> IO ProgressDataKey, so that we
-- can ensure there is no collision? What happens if you call beginTedious twice
-- with the same string, without calling endTedious in the meantime?
beginTedious :: String -> IO ()
beginTedious k = do
    debugMessage $ "Beginning " ++ map toLower k
    setProgressData k ProgressData
                        { sofar = 0
                        , latest = Nothing
                        , total = Nothing
                        }


-- | @endTedious k@ unregisters the tedious process with key @k@, printing
-- "Done" if such a tedious process exists.
endTedious :: String -> IO ()
endTedious k = whenProgressMode $ do
    p <- getProgressData k
    modifyIORef _progressData (second $ delete k)
    when (isJust p) $ putCr $ k ++ " ... done"


tediousSize :: String
            -> Int
            -> IO ()
tediousSize k s = updateProgressData k uptot
  where
    uptot p = case total p of
                  Just t -> seq ts $ p { total = Just ts }
                    where ts = t + s
                  Nothing -> p { total = Just s }


-- | XXX: document this constant
minlist :: Int
minlist = 4


progressList :: String
             -> [a]
             -> [a]
progressList _ [] = []
progressList k (x:xs) = if l < minlist
                          then x:xs
                          else startit x : pl xs
  where
    l = length (x:xs)

    startit y = unsafePerformIO $ do
        beginTedious k
        tediousSize k l
        return y

    pl [] = []
    pl [y] = unsafePerformIO $ do
        endTedious k
        return [y]
    pl (y:ys) = progress k y : pl ys


progress :: String
         -> a
         -> a
progress k a = unsafePerformIO $ progressIO k >> return a


progressIO :: String -> IO ()
progressIO "" = return ()
progressIO k = do
    updateProgressData k $ \p ->
        p { sofar = sofar p + 1, latest = Nothing }

progressKeepLatest :: String
                   -> a
                   -> a
progressKeepLatest k a = unsafePerformIO $ progressKeepLatestIO k >> return a


progressKeepLatestIO :: String -> IO ()
progressKeepLatestIO "" = return ()
progressKeepLatestIO k = do
    updateProgressData k (\p -> p {sofar = sofar p + 1})

finishedOne :: String -> String -> a -> a
finishedOne k l a = unsafePerformIO $ finishedOneIO k l >> return a


finishedOneIO :: String -> String -> IO ()
finishedOneIO "" _ = return ()
finishedOneIO k l = do
    updateProgressData k (\p -> p { sofar = sofar p + 1,
                                    latest = Just l })


_progressMode :: IORef Bool
_progressMode = unsafePerformIO $ newIORef True
{-# NOINLINE _progressMode #-}

_progressData :: IORef (String, Map String ProgressData)
_progressData = unsafePerformIO $ do
    _ <- forkIO handleProgress
    newIORef ("", empty)
{-# NOINLINE _progressData #-}

mkPutCr :: IO (String -> IO ())
mkPutCr =
  TS.size >>= \case
    Nothing ->
      -- stdout is not a terminal
      return $ \_ -> return ()
    Just window -> do
      let limitToWidth = take (TS.width window - 1)
      return $ \s -> do
        putStr $ '\r':limitToWidth s ++ "\r"
        hFlush stdout
        putStr $ '\r':limitToWidth ((replicate (length s)) ' ') ++ "\r"

setProgressMode :: Bool -> IO ()
setProgressMode = writeIORef _progressMode

withoutProgress :: IO a -> IO a
withoutProgress job = bracket off restore (const job) where
  off = withProgressMode $ \m -> do
    debugMessage "Disabling progress reports..."
    setProgressMode False
    return m
  restore m = do
    if m then debugMessage "Reenabling progress reports."
    else debugMessage "Leaving progress reports off."
    setProgressMode m

updateProgressData :: String
                   -> (ProgressData -> ProgressData)
                   -> IO ()
updateProgressData k f =
    whenProgressMode $ modifyIORef _progressData (\(_,m) -> (k,adjust f k m))

setProgressData :: String
                -> ProgressData
                -> IO ()
setProgressData k p =
    whenProgressMode $ modifyIORef _progressData (second $ insert k p)

getProgressData :: String -> IO (Maybe ProgressData)
getProgressData k = withProgressMode $ \p ->
    if p
      then (lookup k . snd) `fmap` readIORef _progressData
      else return Nothing

getProgressLast :: IO String
getProgressLast = withProgressMode $ \p ->
    if p
      then fst `fmap` readIORef _progressData
      else return ""

whenProgressMode :: IO a -> IO ()
whenProgressMode j = withProgressMode $ const $ void j

withProgressMode :: (Bool -> IO a) -> IO a
withProgressMode job = (readIORef _progressMode) >>= job
