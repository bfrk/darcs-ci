module Darcs.Util.Parser
    ( Parser
    , anyChar
    , char
    , checkConsumes
    , choice
    , endOfInput
    , int
    , lexChar
    , lexString
    , linesStartingWith
    , linesStartingWithEndingWith
    , lexWord
    , A.lookAhead
    , many
    , option
    , optional
    , parse
    , parseAll
    , skipSpace
    , skipWhile
    , string
    , take
    , takeTill
    , takeTillChar
    , unsigned
    , withPath
    , (<|>)
    ) where

import Control.Applicative ( empty, many, optional, (<|>) )

import Darcs.Prelude hiding ( lex, take )

import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.Combinator as A
import Data.Attoparsec.ByteString.Char8 hiding ( parse, char, string )
import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.ByteString as B

withPath :: FilePath -> Either String a -> Either String a
withPath fp (Left s) = Left ("in file: "++fp++": "++s)
withPath _ r = r

parseAll :: Parser a -> B.ByteString -> Either String a
parseAll p bs =
  case parse p bs of
    Left e -> Left e
    Right (r, leftover)
      | B.null (B.dropWhile isSpace_w8 leftover) -> Right r
      | otherwise -> Left $ "leftover: " ++ show leftover

parse :: Parser a -> B.ByteString -> Either String (a, B.ByteString)
parse p bs =
  case AC.parse p bs of
    Fail _ ss s -> Left $ unlines (s:ss)
    Partial k ->
      case k B.empty of
        Fail _ ss s -> Left $ unlines (s:ss)
        Partial _ -> error "impossible"
        Done i r -> Right (r, i)
    Done i r -> Right (r, i)

{-# INLINE skip #-}
skip :: Parser a -> Parser ()
skip p = p >> return ()

{-# INLINE lex #-}
lex :: Parser a -> Parser a
lex p = skipSpace >> p

{-# INLINE lexWord #-}
lexWord :: Parser B.ByteString
lexWord = lex (A.takeWhile1 (not . isSpace_w8))

{-# INLINE lexChar #-}
lexChar :: Char -> Parser ()
lexChar c = lex (char c)

{-# inline lexString #-}
lexString :: B.ByteString -> Parser ()
lexString s = lex (string s)

{-# INLINE char #-}
char :: Char -> Parser ()
char = skip . AC.char

{-# INLINE string #-}
string :: B.ByteString -> Parser ()
string = skip . AC.string

{-# INLINE int #-}
int :: Parser Int
int = lex (signed decimal)

{-# INLINE unsigned #-}
unsigned :: Integral a => Parser a
unsigned = lex decimal

{-# INLINE takeTillChar #-}
-- | This function as well as 'takeTill' (on which it is based) both
-- succeed when we hit end-of-input. So a more correct name would
-- be 'takeTillCharOrEOF' (and 'takeTillOrEOF', respectively).
takeTillChar :: Char -> Parser B.ByteString
takeTillChar c = takeTill (== c)

{-# INLINE checkConsumes #-}
checkConsumes :: Parser a -> Parser a
checkConsumes parser = do
  (consumed, result) <- match parser
  if B.null consumed
    then empty
    else return result

{-# INLINE linesStartingWith #-}
linesStartingWith :: Char -> Parser [B.ByteString]
linesStartingWith c = many $ do
  char c
  r <- takeTillChar '\n'
  skip (char '\n') <|> endOfInput
  return r

{-# INLINE linesStartingWithEndingWith #-}
linesStartingWithEndingWith :: Char -> Char -> Parser [B.ByteString]
linesStartingWithEndingWith st en = do
  ls <- linesStartingWith st
  char en
  return ls
