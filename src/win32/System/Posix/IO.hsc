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

-- OpenFileFlags and defaultFileFlags copied literally from unix-2.8.1.1

-- |Correspond to some of the int flags from C's fcntl.h.
data OpenFileFlags =
 OpenFileFlags {
    append    :: Bool,           -- ^ O_APPEND
    exclusive :: Bool,           -- ^ O_EXCL, result is undefined if O_CREAT is False
                                 --
                                 -- __NOTE__: Result is undefined if 'creat' is 'Nothing'.
    noctty    :: Bool,           -- ^ O_NOCTTY
    nonBlock  :: Bool,           -- ^ O_NONBLOCK
    trunc     :: Bool,           -- ^ O_TRUNC
    nofollow  :: Bool,           -- ^ O_NOFOLLOW
                                 --
                                 -- @since 2.8.0.0
    creat     :: Maybe FileMode, -- ^ O_CREAT
                                 --
                                 -- @since 2.8.0.0
    cloexec   :: Bool,           -- ^ O_CLOEXEC
                                 --
                                 -- @since 2.8.0.0
    directory :: Bool,           -- ^ O_DIRECTORY
                                 --
                                 -- @since 2.8.0.0
    sync      :: Bool            -- ^ O_SYNC
                                 --
                                 -- @since 2.8.0.0
 }
 deriving (Read, Show, Eq, Ord)

-- | Default values for the 'OpenFileFlags' type.
--
-- Each field of 'OpenFileFlags' is either 'False' or 'Nothing'
-- respectively.
defaultFileFlags :: OpenFileFlags
defaultFileFlags =
 OpenFileFlags {
    append    = False,
    exclusive = False,
    noctty    = False,
    nonBlock  = False,
    trunc     = False,
    nofollow  = False,
    creat     = Nothing,
    cloexec   = False,
    directory = False,
    sync      = False
  }


-- Adapted from System.Posix.IO.Common.openat_ in unix-2.8.1.1
#include <fcntl.h>

openFd :: FilePath -> OpenMode -> OpenFileFlags -> IO Fd
openFd name how OpenFileFlags{..} =
#if mingw32_HOST_OS
  withCWString name $ \s -> do
#else
  withCString name $ \s -> do
#endif
    Fd <$> throwErrnoIfMinus1 "openFd" (c_open s all_flags mode_w)
  where
    all_flags  = creat .|. flags .|. open_mode

    flags =
       (if append       then (#const O_APPEND)    else 0) .|.
       (if exclusive    then (#const O_EXCL)      else 0) .|.
       (if noctty       then (#const O_NOCTTY)    else 0) .|.
       (if nonBlock     then (#const O_NONBLOCK)  else 0) .|.
       (if truncate     then (#const O_TRUNC)     else 0) .|.
       (if nofollow     then (#const O_NOFOLLOW)  else 0) .|.
       (if cloexec      then (#const O_CLOEXEC)   else 0) .|.
       (if directory    then (#const O_DIRECTORY) else 0) .|.
       (if sync         then (#const O_SYNC)      else 0)

    (creat, mode_w) = case creat of
                        Nothing -> (0,0)
                        Just x  -> ((#const O_CREAT), x)

    open_mode = case how of
                   ReadOnly  -> (#const O_RDONLY)
                   WriteOnly -> (#const O_WRONLY)
                   ReadWrite -> (#const O_RDWR)

{-
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
-}

closeFd :: Fd -> IO ()
closeFd (Fd fd) = throwErrnoIfMinus1_ "closeFd" (c_close fd)

data OpenMode = ReadOnly | WriteOnly | ReadWrite

