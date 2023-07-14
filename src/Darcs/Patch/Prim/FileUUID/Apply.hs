{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Darcs.Patch.Prim.FileUUID.Apply ( hunkEdit, ObjectMap(..) ) where

import Darcs.Prelude

import Control.Monad.Catch ( MonadThrow(throwM) )
import Control.Monad.State( StateT, runStateT, gets, lift, put )
import qualified Data.ByteString as B
import qualified Data.Map as M

import Darcs.Patch.Apply ( Apply(..) )
import Darcs.Patch.ApplyMonad
    ( ApplyMonad(..), ApplyMonadTrans(..)
    , ApplyMonadOperations
    )
import Darcs.Patch.Prim.Class ( PrimApply(..) )
import Darcs.Patch.Prim.FileUUID.Core ( Prim(..), Hunk(..), HunkMove(..) )
import Darcs.Patch.Prim.FileUUID.Show
import Darcs.Patch.Prim.FileUUID.ObjectMap
import Darcs.Patch.Repair ( RepairToFL(..) )
import Darcs.Patch.Witnesses.Ordered ( FL(..) )

import Darcs.Util.Printer( text, packedString, ($$), renderString )


instance Apply Prim where
  type ApplyState Prim = ObjectMap
  apply (Manifest i (L dirid name)) = editDirectory dirid (addObject name i dirid)
  apply (Demanifest i (L dirid name)) = editDirectory dirid (delObject name i dirid)
  apply (Hunk i hunk) = editFile i (hunkEdit hunk)
  apply (HunkMove (HM fs ls ft lt c)) =
    editFile fs (hunkEdit (H ls c B.empty)) >> editFile ft (hunkEdit (H lt B.empty c))
  apply Identity = return ()

instance RepairToFL Prim where
  applyAndTryToFixFL p = apply p >> return Nothing

instance PrimApply Prim where
  applyPrimFL NilFL = return ()
  applyPrimFL (p :>: ps) = apply p >> applyPrimFL ps

addObject :: Name -> UUID -> UUID -> DirContent -> Either String DirContent
addObject name obj dirid dir
  | Just obj' <- M.lookup name dir =
    Left $ unwords
      [ "##error applying patch: cannot insert"
      , show (name, obj)
      , "into directory"
      , show dirid
      , "because another object"
      , show obj'
      , "with that name already exists"
      ]
  | otherwise = Right $ M.insert name obj dir

delObject :: Name -> UUID -> UUID -> DirContent -> Either String DirContent
delObject name obj dirid dir =
  case M.lookup name dir of
    Just obj'
      | obj == obj' -> Right $ M.delete name dir
      | otherwise ->
        Left $ unwords
          [ "##error applying patch: cannot remove"
          , show (name, obj)
          , "from directory"
          , show dirid
          , "because it contains a different object"
          , show obj'
          ]
    Nothing ->
        Left $ unwords
          [ "##error applying patch: cannot remove"
          , show (name, obj)
          , "from directory"
          , show dirid
          , "because it does not contain any object of that name"
          ]

hunkEdit :: Hunk wX wY -> FileContent -> Either String FileContent
hunkEdit h@(H off old new) c
  | old `B.isPrefixOf` (B.drop off c) =
      Right $ B.concat [B.take off c, new, B.drop (off + B.length old) c]
  | otherwise =
      Left $ renderString $
      text "##error applying hunk:" $$ displayHunk Nothing h $$ "##to" $$
      packedString c
--       $$ text "##old=" <> text (ppShow old) $$
--       text "##new=" <> text (ppShow new) $$
--       text "##c=" <> text (ppShow c)

editObject :: MonadThrow m
           => UUID
           -> (Maybe (Object m) -> Either String (Object m))
           -> (StateT (ObjectMap m) m) ()
editObject i edit = do
  load <- gets getObject
  store <- gets putObject
  obj <- lift $ load i
  obj' <- liftEither $ edit obj
  new <- lift $ store i $ obj'
  put new

-- a semantic, ObjectMap-based interface for patch application
class ApplyMonadObjectMap m where
  editFile :: UUID -> (FileContent -> Either String FileContent) -> m ()
  editDirectory :: UUID -> (DirContent -> Either String DirContent) -> m ()

type instance ApplyMonadOperations ObjectMap = ApplyMonadObjectMap

instance MonadThrow m => ApplyMonad ObjectMap (StateT (ObjectMap m) m) where
  readFilePS i = do
    load <- gets getObject
    mobj <- lift $ load i
    case mobj of
      Just (Blob readBlob _) -> lift readBlob
      Just _ -> throwM $ userError $ "readFilePS " ++ show i ++ ": object is not a file"
      Nothing -> throwM $ userError $ "readFilePS " ++ show i ++ ": no such file"

liftEither :: MonadThrow m => Either String a -> m a
liftEither (Left e) = throwM $ userError e
liftEither (Right v) = return v

instance MonadThrow m => ApplyMonadObjectMap (StateT (ObjectMap m) m) where
  editFile i edit = editObject i edit'
    where
      edit' (Just (Blob x _)) = Right $ Blob (liftEither . edit =<< x) Nothing
      edit' Nothing = Right $ Blob (liftEither $ edit "") Nothing
      edit' (Just (Directory _)) =
        Left $ "wrong kind of object: " ++ show i ++ " is a directory, not a file"
  editDirectory i edit = editObject i edit'
    where
      edit' (Just (Directory x)) = Directory <$> edit x
      edit' Nothing = Directory <$> edit M.empty
      edit' (Just (Blob _ _)) =
        Left $ "wrong kind of object: " ++ show i ++ " is a file, not a directory"

instance MonadThrow m => ApplyMonadTrans ObjectMap m where
  type ApplyMonadOver ObjectMap m = StateT (ObjectMap m) m
  runApplyMonad = runStateT
