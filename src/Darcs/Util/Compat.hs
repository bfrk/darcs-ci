{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Darcs.Util.Compat
    ( stdoutIsAPipe
    , atomicCreate
    , sloppyAtomicCreate
    ) where

import Darcs.Prelude

import Control.Monad ( unless )
import Foreign.C.Types ( CInt(..) )
import Foreign.C.String ( CString, withCString )
import Foreign.C.Error ( throwErrno, eEXIST, getErrno )
import System.Directory ( getCurrentDirectory )
import System.IO.Error ( mkIOError, alreadyExistsErrorType )
import System.Posix.Files ( stdFileMode )
import System.Posix.IO ( openFd, closeFd,
                         defaultFileFlags, exclusive,
                         OpenMode(WriteOnly) )

import Darcs.Util.SignalHandler ( stdoutIsAPipe )

sloppyAtomicCreate :: FilePath -> IO ()
sloppyAtomicCreate fp
    = do fd <- openFd fp WriteOnly (Just stdFileMode) flags
         closeFd fd
  where flags = defaultFileFlags { exclusive = True }

atomicCreate :: FilePath -> IO ()
atomicCreate fp = withCString fp $ \cstr -> do
    rc <- c_atomic_create cstr
    unless (rc >= 0) $
           do errno <- getErrno
              pwd <- getCurrentDirectory
              if errno == eEXIST
                 then ioError $ mkIOError alreadyExistsErrorType
                                          ("atomicCreate in "++pwd)
                                          Nothing (Just fp)
                 else throwErrno $ "atomicCreate "++fp++" in "++pwd

foreign import ccall unsafe "atomic_create.h atomic_create" c_atomic_create
    :: CString -> IO CInt
