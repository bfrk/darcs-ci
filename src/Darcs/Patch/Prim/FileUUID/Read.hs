{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Darcs.Patch.Prim.FileUUID.Read () where

import Darcs.Prelude hiding ( take )

import Control.Monad ( liftM, liftM2 )

import Darcs.Patch.Read ( ReadPatch(..), ReadPatches(..) )
import Darcs.Patch.Prim.FileUUID.Core( Prim(..), Hunk(..) )
import Darcs.Patch.Prim.FileUUID.ObjectMap
import Darcs.Patch.Witnesses.Sealed( seal )

import Darcs.Util.Path ( decodeWhiteName )
import Darcs.Util.Parser

instance ReadPatch Prim where
  readPatch' = do
    skipSpace
    choice $ map (liftM seal)
      [ identity
      , hunk "hunk" Hunk
      , manifest "manifest" Manifest
      , manifest "demanifest" Demanifest
      ]
    where
      manifest kind ctor = liftM2 ctor (patch kind) location
      identity = lexString "identity" >> return Identity
      patch x = string x >> uuid
      uuid =
        const Root <$> lexString "root"
        <|>
        Recorded <$> (lexString "r" >> lexWord)
        <|>
        Unrecorded <$> (lexString "u" >> unsigned)
      filename = do
        word <- lexWord
        either fail return $ decodeWhiteName word
      content = do
        lexString "content"
        len <- int
        _ <- char '\n'
        take len
      location = liftM2 L uuid filename
      hunk kind ctor = do
        uid <- patch kind
        offset <- int
        old <- content
        new <- content
        return $ ctor uid (H offset old new)

instance ReadPatches Prim
