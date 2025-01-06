module Darcs.Patch.Prim.V1.Read (readPrim) where

import Darcs.Prelude

import Darcs.Patch.Prim.Class ( hunk, binary )
import Darcs.Patch.Prim.V1.Core
    ( Prim(..)
    , DirPatchType(..)
    , FilePatchType(..)
    )
import Darcs.Patch.Prim.V1.Apply ()

import Darcs.Util.Parser
    ( Parser, takeTillChar, string, int
    , option, choice, anyChar, char, lexWord
    , skipSpace, skipWhile, linesStartingWith
    )

import Darcs.Patch.Witnesses.Sealed ( Sealed(..) )

import Darcs.Util.ByteString ( decodeLocale, fromHex2PS )
import Darcs.Util.Path ( AnchoredPath )

import qualified Data.ByteString       as B  ( ByteString, init, tail, concat )
import qualified Data.ByteString.Char8 as BC ( unpack, pack, stripPrefix )


type DecodePath = B.ByteString -> Either String AnchoredPath

readFilePath :: DecodePath -> Parser AnchoredPath
readFilePath decodePath = do
  raw <- lexWord
  case BC.stripPrefix (BC.pack "./") raw of
    Nothing -> fail $ "invalid file path"
    Just raw' ->
      case decodePath raw' of
        Left e -> fail e
        Right r -> return r

readPrim :: DecodePath -> Parser (Sealed (Prim wX))
readPrim fmt =
  skipSpace >> choice
    [ Sealed <$> readHunk fmt
    , Sealed <$> readAddFile fmt
    , Sealed <$> readAddDir fmt
    , Sealed <$> readMove fmt
    , Sealed <$> readRmFile fmt
    , Sealed <$> readRmDir fmt
    , Sealed <$> readTok fmt
    , Sealed <$> readBinary fmt
    , Sealed <$> readChangePref
    ]

hunk' :: B.ByteString
hunk' = BC.pack "hunk"

replace :: B.ByteString
replace = BC.pack "replace"

binary' :: B.ByteString
binary' = BC.pack "binary"

addfile :: B.ByteString
addfile = BC.pack "addfile"

adddir :: B.ByteString
adddir = BC.pack "adddir"

rmfile :: B.ByteString
rmfile = BC.pack "rmfile"

rmdir :: B.ByteString
rmdir = BC.pack "rmdir"

move :: B.ByteString
move = BC.pack "move"

changepref :: B.ByteString
changepref = BC.pack "changepref"

readHunk :: DecodePath -> Parser (Prim wX wY)
readHunk fmt = do
  string hunk'
  fi <- readFilePath fmt
  l <- int
  have_nl <- skipNewline
  if have_nl
    then do
      _ <- linesStartingWith ' ' -- skipping context
      old <- linesStartingWith '-'
      new <- linesStartingWith '+'
      _ <- linesStartingWith ' ' -- skipping context
      return $ hunk fi l old new
    else return $ hunk fi l [] []

skipNewline :: Parser Bool
skipNewline = option False (char '\n' >> return True)

readTok :: DecodePath -> Parser (Prim wX wY)
readTok fmt = do
  string replace
  f <- readFilePath fmt
  regstr <- lexWord
  o <- lexWord
  n <- lexWord
  return $ FP f $ TokReplace (BC.unpack (drop_brackets regstr))
                             (BC.unpack o) (BC.unpack n)
    where drop_brackets = B.init . B.tail


-- * Binary file modification
--
-- | Modify a binary file
--
-- > binary FILENAME
-- > oldhex
-- > *HEXHEXHEX
-- > ...
-- > newhex
-- > *HEXHEXHEX
-- > ...
readBinary :: DecodePath -> Parser (Prim wX wY)
readBinary fmt = do
  string binary'
  fi <- readFilePath fmt
  _ <- lexWord
  skipSpace
  old <- linesStartingWith '*'
  r_old <- either fail return $ fromHex2PS $ B.concat old
  _ <- lexWord
  skipSpace
  new <- linesStartingWith '*'
  r_new <- either fail return $ fromHex2PS $ B.concat new
  return $ binary fi r_old r_new

readAddFile :: DecodePath -> Parser (Prim wX wY)
readAddFile fmt = do
  string addfile
  f <- readFilePath fmt
  return $ FP f AddFile

readRmFile :: DecodePath -> Parser (Prim wX wY)
readRmFile fmt = do
  string rmfile
  f <- readFilePath fmt
  return $ FP f RmFile

readMove :: DecodePath -> Parser (Prim wX wY)
readMove fmt = do
  string move
  d <- readFilePath fmt
  d' <- readFilePath fmt
  return $ Move d d'

readChangePref :: Parser (Prim wX wY)
readChangePref = do
  string changepref
  p <- lexWord
  skipWhile (== ' ')
  _ <- anyChar -- skip newline
  f <- takeTillChar '\n'
  _ <- anyChar -- skip newline
  t <- takeTillChar '\n'
  return $ ChangePref (BC.unpack p) (decodeLocale f) (decodeLocale t)

readAddDir :: DecodePath -> Parser (Prim wX wY)
readAddDir fmt = do
  string adddir
  f <- readFilePath fmt
  return $ DP f AddDir

readRmDir :: DecodePath -> Parser (Prim wX wY)
readRmDir fmt = do
  string rmdir
  f <- readFilePath fmt
  return $ DP f RmDir
