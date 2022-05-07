module System.Posix.Files
    ( isNamedPipe, isDirectory, isRegularFile, isSymbolicLink
    , getFdStatus, getFileStatus, getSymbolicLinkStatus
    , modificationTimeHiRes, setFileMode, fileSize, fileMode, fileOwner
    , stdFileMode, FileStatus, fileID
    , linkCount, createLink
    ) where

import System.PosixCompat.Files
{-
    ( isNamedPipe, isDirectory, isRegularFile, isSymbolicLink
    , getFdStatus, getFileStatus
    , modificationTimeHiRes, setFileMode, fileSize, fileMode, fileOwner
    , stdFileMode, FileStatus(..), fileID
    , linkCount, createLink
    )
-}

import System.Win32.File
import System.PosixCompat.Types ( FileMode )

regularFileMode      :: FileMode
directoryMode        :: FileMode
symbolicLinkMode     :: FileMode

regularFileMode      = 0o0100000
directoryMode        = 0o0040000
symbolicLinkMode     = 0o0120000

getSymbolicLinkStatus :: FilePath -> IO FileStatus
getSymbolicLinkStatus path = do
    perm  <- liftM permsToMode (getPermissions path)
    info  <- bracket openPath closeHandle getFileInformationByHandle
    let atime = windowsToPosixTime (bhfiLastAccessTime info)
        mtime = windowsToPosixTime (bhfiLastWriteTime info)
        ctime = windowsToPosixTime (bhfiCreationTime info)
        attr = bhfiFileAttributes info
        test x y = x .&. y == x
        -- Contrary to Posix systems, directory symlinks on Windows have both
        -- fILE_ATTRIBUTE_REPARSE_POINT and fILE_ATTRIBUTE_DIRECTORY bits set.
        -- Generally, the file type values in Posix should be understood as an
        -- enumeration, not as a bitset.
        fileType
          | test fILE_ATTRIBUTE_REPARSE_POINT attr = symbolicLinkMode
          | test fILE_ATTRIBUTE_DIRECTORY attr = directoryMode
          | otherwise = regularFileMode -- it's a lie but what can we do?
    return $ FileStatus
             { deviceID         = fromIntegral (bhfiVolumeSerialNumber info)
             , fileID           = fromIntegral (bhfiFileIndex info)
             , fileMode         = fileType .|. perm
             , linkCount        = fromIntegral (bhfiNumberOfLinks info)
             , fileOwner        = 0
             , fileGroup        = 0
             , specialDeviceID  = 0
             , fileSize         = fromIntegral (bhfiSize info)
             , accessTime       = posixTimeToEpochTime atime
             , modificationTime = posixTimeToEpochTime mtime
             , statusChangeTime = posixTimeToEpochTime mtime
             , accessTimeHiRes       = atime
             , modificationTimeHiRes = mtime
             , statusChangeTimeHiRes = ctime
             }
  where
    openPath = createFile path
                 fILE_READ_EA
                 (fILE_SHARE_READ .|. fILE_SHARE_WRITE .|. fILE_SHARE_DELETE)
                 Nothing
                 oPEN_EXISTING
                 (sECURITY_ANONYMOUS .|. fILE_FLAG_BACKUP_SEMANTICS .|.
                  fILE_FLAG_OPEN_REPARSE_POINT)
                 Nothing
    -- not yet defined in Win32 package:
    fILE_FLAG_OPEN_REPARSE_POINT :: FileAttributeOrFlag
    fILE_FLAG_OPEN_REPARSE_POINT = 0x00200000
