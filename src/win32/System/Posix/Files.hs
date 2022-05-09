module System.Posix.Files
    ( isNamedPipe, isDirectory, isRegularFile, isSymbolicLink
    , getFdStatus, getFileStatus, getSymbolicLinkStatus
    , modificationTimeHiRes, setFileMode, fileSize, fileMode, fileOwner
    , stdFileMode, FileStatus, fileID
    , linkCount, createLink, ownerModes
    ) where

import System.PosixCompat.Files
    ( isNamedPipe, isDirectory, isRegularFile, isSymbolicLink
    , getFdStatus, getFileStatus, getSymbolicLinkStatus
    , modificationTimeHiRes, setFileMode, fileSize, fileMode, fileOwner
    , stdFileMode, FileStatus, fileID
    , linkCount, createLink, ownerModes
    )
