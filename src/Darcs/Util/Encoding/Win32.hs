-- Copyright 2007-2009, Judah Jacobson.
-- All Rights Reserved.

-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:

-- - Redistribution of source code must retain the above copyright notice,
-- this list of conditions and the following disclaimer.

-- - Redistribution in binary form must reproduce the above copyright notice,
-- this list of conditions and the following disclaimer in the documentation
-- and/or other materials provided with the distribution.

-- THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS "AS IS" AND ANY
-- EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR THE CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
-- USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
module Darcs.Util.Encoding.Win32
    ( encode, decode
    ) where

import Darcs.Prelude

import qualified Data.ByteString as B ( ByteString, useAsCStringLen )
import Data.ByteString.Internal ( createAndTrim )
import Foreign ( allocaArray0, castPtr )
import Foreign.C ( peekCWStringLen, withCWStringLen )
import System.Win32 ( CodePage, nullPtr )
import System.Win32.Encoding
    ( getCurrentCodePage
    , multiByteToWideChar
    , wideCharToMultiByte
    )

-- | Encode a Unicode 'String' into a 'ByteString' suitable for the current
-- console.
encode :: String -> IO B.ByteString
encode str = getCurrentCodePage >>= flip unicodeToCodePage str

-- | Convert a 'ByteString' from the console's encoding into a Unicode 'String'.
decode :: B.ByteString -> IO String
decode str = getCurrentCodePage >>= flip codePageToUnicode str

------------------------
-- Multi-byte conversion

unicodeToCodePage :: CodePage -> String -> IO B.ByteString
unicodeToCodePage cp wideStr = withCWStringLen wideStr $ \(wideBuff, wideLen) -> do
    -- first, ask for the length without filling the buffer.
    outSize <- wideCharToMultiByte cp 0 wideBuff (toEnum wideLen)
                    nullPtr 0 nullPtr nullPtr
    -- then, actually perform the encoding.
    createAndTrim (fromEnum outSize) $ \outBuff ->
        fmap fromEnum $ wideCharToMultiByte cp 0 wideBuff (toEnum wideLen)
                    (castPtr outBuff) outSize nullPtr nullPtr

codePageToUnicode :: CodePage -> B.ByteString -> IO String
codePageToUnicode cp bs = B.useAsCStringLen bs $ \(inBuff, inLen) -> do
    -- first ask for the size without filling the buffer.
    outSize <- multiByteToWideChar cp 0 inBuff (toEnum inLen) nullPtr 0
    -- then, actually perform the decoding.
    allocaArray0 (fromEnum outSize) $ \outBuff -> do
        outSize' <- multiByteToWideChar cp 0 inBuff (toEnum inLen) outBuff outSize
        peekCWStringLen (outBuff, fromEnum outSize')
