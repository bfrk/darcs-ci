{-# LANGUAGE CPP #-}

-- |
-- Module      : Darcs.Util.Workaround
-- Copyright   : 2008 David Roundy <droundy@darcs.net>
-- License     : GPL
-- Maintainer  : darcs-devel@darcs.net
-- Stability   : experimental
-- Portability : portable

module Darcs.Util.Workaround
    ( setExecutable
    , getCurrentDirectory
    , installHandler
    , raiseSignal
    , Handler(..)
    , Signal
    , sigINT
    , sigHUP
    , sigABRT
    , sigALRM
    , sigTERM
    , sigPIPE
    , copyFile
    ) where

import Darcs.Prelude

#ifdef WIN32

import qualified System.Directory ( getCurrentDirectory, canonicalizePath, copyFile )
import System.IO (openFile, FileMode(..) )

#else

import System.Posix.Signals(installHandler, raiseSignal, Handler(..), Signal,
                            sigINT, sigHUP, sigABRT, sigALRM, sigTERM, sigPIPE)
import System.Directory ( getCurrentDirectory, copyFile )
import System.Posix.Files (fileMode,getFileStatus, setFileMode,
                           setFileCreationMask,
                           ownerReadMode, ownerWriteMode, ownerExecuteMode,
                           groupReadMode, groupWriteMode, groupExecuteMode,
                           otherReadMode, otherWriteMode, otherExecuteMode)
import Data.Bits ( (.&.), (.|.), complement )

#endif

#ifdef WIN32
-- Dummy implementation of POSIX signals
data Handler = Default
             | Ignore
             | Catch (IO ())

type Signal = Int

installHandler :: Signal
               -> Handler
               -> Maybe ()
               -> IO ()
installHandler _ _ _ = return ()


raiseSignal :: Signal -> IO ()
raiseSignal _ = return ()


sigINT :: Signal
sigINT = 0

-- not used: sigKILL = 0

sigHUP :: Signal
sigHUP = 0

-- not used: sigQUIT = 0

sigABRT :: Signal
sigABRT = 0

sigTERM :: Signal
sigTERM = 0

sigPIPE :: Signal
sigPIPE = 0

sigALRM :: Signal
sigALRM = 0


setExecutable :: FilePath
              -> Bool
              -> IO ()
setExecutable _ _ = return ()


-- | System.Directory.getCurrentDirectory returns a path with backslashes in it
-- under windows, and some of the code gets confused by that, so we override
-- getCurrentDirectory and translates '\\' to '/'
getCurrentDirectory :: IO FilePath
getCurrentDirectory = do
    d <- System.Directory.getCurrentDirectory >>= System.Directory.canonicalizePath
    return $ map rb d
  where
    rb '\\' = '/'
    rb c = c


-- | 'System.Directory.copyFile' is broken on Windows from ghc-9.6 onwards,
-- in that it does not fail when the source is non-existent.
copyFile :: FilePath -> FilePath -> IO ()
copyFile src dst = do
  -- slightly inefficient but an easy way to make it throw the
  -- appropriate exception
  h <- openFile src ReadMode
  hClose h
  System.Directory.copyFile src dst


#else


setExecutable :: FilePath
              -> Bool
              -> IO ()
setExecutable f ex = do
    st <- getFileStatus f
    umask <- setFileCreationMask 0
    _ <- setFileCreationMask umask
    let rw = fileMode st .&.
             (ownerReadMode .|. ownerWriteMode .|.
              groupReadMode .|. groupWriteMode .|.
              otherReadMode .|. otherWriteMode)
        total = if ex then rw .|.
                          ((ownerExecuteMode .|. groupExecuteMode .|. otherExecuteMode)
                           .&. complement umask)
                      else rw
    setFileMode f total

#endif
