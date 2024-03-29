{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Darcs.Util.Compat
    ( stdoutIsAPipe
    , maybeRelink
    , atomicCreate
    , sloppyAtomicCreate
    ) where

#ifdef WIN32
#define USE_CREAT
#else
#if MIN_VERSION_unix(2,8,0)
#define USE_CREAT
#endif
#endif

import Darcs.Prelude

import Control.Monad ( unless )
import Foreign.C.Types ( CInt(..) )
import Foreign.C.String ( CString, withCString )
import Foreign.C.Error ( throwErrno, eEXIST, getErrno )
import System.Directory ( getCurrentDirectory )
import System.IO.Error ( mkIOError, alreadyExistsErrorType )
import System.Posix.Files ( stdFileMode )
import System.Posix.IO ( openFd, closeFd,
#ifdef USE_CREAT
                         creat,
#endif
                         defaultFileFlags, exclusive,
                         OpenMode(WriteOnly) )

import Darcs.Util.SignalHandler ( stdoutIsAPipe )

foreign import ccall unsafe "maybe_relink.h maybe_relink" maybe_relink
    :: CString -> CString -> CInt -> IO CInt

-- Checks whether src and dst are identical.  If so, makes dst into a
-- link to src.  Returns True if dst is a link to src (either because
-- we linked it or it already was).  Safe against changes to src if
-- they are not in place, but not to dst.
maybeRelink :: String -> String -> IO Bool
maybeRelink src dst =
    withCString src $ \csrc ->
    withCString dst $ \cdst ->
    do rc <- maybe_relink csrc cdst 1
       case rc of
        0 -> return True
        1 -> return True
        -1 -> throwErrno ("Relinking " ++ dst)
        -2 -> return False
        -3 -> do putStrLn ("Relinking: race condition avoided on file " ++
                            dst)
                 return False
        _ -> fail ("Unexpected situation when relinking " ++ dst)

sloppyAtomicCreate :: FilePath -> IO ()
sloppyAtomicCreate fp
#ifdef USE_CREAT
    = do fd <- openFd fp WriteOnly flags {creat = Just stdFileMode}
#else
    = do fd <- openFd fp WriteOnly (Just stdFileMode) flags
#endif
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
