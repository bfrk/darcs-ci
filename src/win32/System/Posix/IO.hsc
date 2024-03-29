{-# LANGUAGE ForeignFunctionInterface #-}
module System.Posix.IO where

import Darcs.Prelude

#if mingw32_HOST_OS
import Foreign.C.String( withCWString )
#else
import Foreign.C.String ( withCString )
#endif

import Foreign.C.Error ( throwErrnoIfMinus1, throwErrnoIfMinus1_ )

import System.Posix.Internals ( c_open, c_close )
import System.Posix.Types ( Fd(..), FileMode )

import Data.Bits ( (.|.) )


stdOutput :: Fd
stdOutput = Fd 1

stdError :: Fd
stdError = Fd 2

data OpenFileFlags = 
 OpenFileFlags {
  append :: Bool,
  exclusive :: Bool,
  noctty :: Bool,
  nonBlock :: Bool,
  trunc :: Bool,
  creat :: Maybe FileMode
 }


-- Adapted from System.Posix.IO in ghc
#include <fcntl.h>

openFd :: FilePath -> OpenMode -> OpenFileFlags -> IO Fd
openFd name how off = do
#if mingw32_HOST_OS
  withCWString name $ \s -> do
#else
  withCString name $ \s -> do
#endif
   fd <- throwErrnoIfMinus1 "openFd" (c_open s all_flags mode_w)
   return (Fd fd)
 where
   all_flags = binary .|. creat_ .|. flags .|. open_mode
   flags =
    (if append off    then (#const O_APPEND)   else 0) .|.
    (if exclusive off then (#const O_EXCL)     else 0) .|.
    (if trunc off     then (#const O_TRUNC)    else 0)
   binary = (#const O_BINARY)
   (creat_, mode_w) = maybe (0,0) (\x->((#const O_CREAT),x)) (creat off)
   open_mode = case how of
                ReadOnly  -> (#const O_RDONLY)
                WriteOnly -> (#const O_WRONLY)
                ReadWrite -> (#const O_RDWR)

closeFd :: Fd -> IO ()
closeFd (Fd fd) = throwErrnoIfMinus1_ "closeFd" (c_close fd)

data OpenMode = ReadOnly | WriteOnly | ReadWrite

defaultFileFlags :: OpenFileFlags
defaultFileFlags = OpenFileFlags False False False False False Nothing


