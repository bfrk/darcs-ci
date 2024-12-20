-- Copyright (C) 2002-2003 David Roundy
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; see the file COPYING.  If not, write to
-- the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
-- Boston, MA 02110-1301, USA.

module Darcs.Patch.Read
    ( ReadPatch(..)
    , ReadPatches(..)
    , readPatch
    , readPatchFL
    , standardReadPatchFL'
    , legacyReadPatchFL'
    , readPatchPartial
    , readBracketedFL
    ) where

import Darcs.Prelude

import Control.Applicative ( (<|>) )
import qualified Data.ByteString as B ( ByteString )

import Darcs.Patch.Bracketed ( Bracketed(..), unBracketedFL )
import Darcs.Util.Parser
    ( Parser
    , checkConsumes
    , lexChar
    , parse
    , parseAll
    )
import Darcs.Patch.Witnesses.Ordered ( FL(..) )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..), mapSeal )


-- | This class is used to decode patches from their binary representation.
class ReadPatch p where
  readPatch' :: Parser (Sealed (p wX))

class ReadPatch p => ReadPatches p where
  readPatchFL' :: Parser (Sealed (FL p wX))
  readPatchFL' = standardReadPatchFL'

-- | Read a sequence of patches with optional (arbitrarily nested, round or
-- curly) bracketing.
legacyReadPatchFL' :: ReadPatch p => Parser (Sealed (FL p wX))
-- This is slightly tricky. Note how the standardReadPatchFL' here reads
-- a plain (unbracketed) sequence of zero or more Bracketed patches.
legacyReadPatchFL' = mapSeal unBracketedFL <$> standardReadPatchFL'

standardReadPatchFL' :: ReadPatch p => Parser (Sealed (FL p wX))
standardReadPatchFL' = do
  -- checkConsumes is needed to make sure that something is read,
  -- to avoid stack overflow when parsing FL (FL p)
  mp <- (Just <$> checkConsumes readPatch') <|> return Nothing
  case mp of
    Just (Sealed p) -> do
      Sealed ps <- standardReadPatchFL'
      return $ Sealed (p :>: ps)
    Nothing -> return $ Sealed NilFL

readPatchPartial :: ReadPatch p => B.ByteString -> Either String (Sealed (p wX), B.ByteString)
readPatchPartial = parse readPatch'

readPatch :: ReadPatch p => B.ByteString -> Either String (Sealed (p wX))
readPatch = parseAll readPatch'

readPatchFL :: ReadPatches p => B.ByteString -> Either String (Sealed (FL p wX))
readPatchFL = parseAll readPatchFL'

instance ReadPatch p => ReadPatch (Bracketed p) where
    readPatch' = mapSeal Braced <$> readBracketedFL readPatch' '{' '}'
                   <|>
                 mapSeal Parens <$> readBracketedFL readPatch' '(' ')'
                   <|>
                 mapSeal Singleton <$> readPatch'

-- | This instance is needed to parse patch bundles
instance ReadPatch p => ReadPatches (Bracketed p)

-- | Given a 'Parser' for single patch, parse a (plain) sequence of such
-- patches, nested between the given (required) start and end tokens.
{-# INLINE readBracketedFL #-}
readBracketedFL
  :: forall p wX
   . (forall wY . Parser (Sealed (p wY)))
  -> Char
  -> Char
  -> Parser (Sealed (FL p wX))
readBracketedFL single pre post = lexChar pre >> go
  where
    go :: forall wZ. Parser (Sealed (FL p wZ))
    go = none <|> some
    none = lexChar post >> pure (Sealed NilFL)
    some = do
      Sealed p <- single
      Sealed ps <- go
      return $ Sealed (p :>: ps)
