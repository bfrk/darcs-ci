module Darcs.Repository.Unrevert
    ( finalizeTentativeUnrevert
    , revertTentativeUnrevert
    , writeUnrevert
    , readUnrevert
    , removeFromUnrevertContext
    ) where

import Darcs.Prelude

import Darcs.Patch ( PrimOf, RepoPatch, commuteRL )
import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch.Bundle ( interpretBundle, makeBundle, parseBundle )
import Darcs.Patch.Depends ( patchSetMerge, removeFromPatchSet )
import Darcs.Patch.Info ( patchinfo )
import Darcs.Patch.Named ( infopatch )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, hopefully )
import Darcs.Patch.Set ( Origin, PatchSet, SealedPatchSet )
import Darcs.Patch.Witnesses.Ordered
    ( (:/\:)(..)
    , (:>)(..)
    , FL(..)
    , lengthFL
    , reverseFL
    )
import Darcs.Patch.Witnesses.Sealed ( Sealed(Sealed) )
import Darcs.Repository.Paths ( tentativeUnrevertPath, unrevertPath )
import Darcs.Util.Exception ( catchDoesNotExistError, ifDoesNotExistError )
import Darcs.Util.Global ( debugMessage )
import Darcs.Util.IsoDate ( getIsoDateTime )
import Darcs.Util.Lock ( readBinFile, removeFileMayNotExist, writeDocBinFile )
import Darcs.Util.Prompt ( promptYorn )
import Darcs.Util.Tree ( Tree )

import System.Directory ( copyFile, renameFile )

finalizeTentativeUnrevert :: IO ()
finalizeTentativeUnrevert =
  renameFile tentativeUnrevertPath unrevertPath `catchDoesNotExistError`
    removeFileMayNotExist unrevertPath

revertTentativeUnrevert :: IO ()
revertTentativeUnrevert =
  copyFile unrevertPath tentativeUnrevertPath `catchDoesNotExistError`
    removeFileMayNotExist tentativeUnrevertPath

writeUnrevert :: (RepoPatch p, ApplyState p ~ Tree)
              => PatchSet p Origin wR
              -> FL (PrimOf p) wR wX
              -> IO ()
writeUnrevert _ NilFL = removeFileMayNotExist tentativeUnrevertPath
writeUnrevert recorded ps = do
  date <- getIsoDateTime
  info <- patchinfo date "unrevert" "anon" []
  let np = infopatch info ps
  bundle <- makeBundle Nothing recorded (np :>: NilFL)
  writeDocBinFile tentativeUnrevertPath bundle

readUnrevert :: RepoPatch p
             => PatchSet p Origin wR
             -> IO (SealedPatchSet p Origin)
readUnrevert us = do
  pf <- readBinFile tentativeUnrevertPath
        `catchDoesNotExistError` fail "There's nothing to unrevert!"
  case parseBundle pf of
      Right (Sealed bundle) -> do
        case interpretBundle us bundle of
          Left msg -> fail msg
          Right ps -> return (Sealed ps)
      Left err -> fail $ "Couldn't parse unrevert patch:\n" ++ err

removeFromUnrevertContext :: forall p wR wX. (RepoPatch p, ApplyState p ~ Tree)
                          => PatchSet p Origin wR
                          -> FL (PatchInfoAnd p) wX wR
                          -> IO ()
removeFromUnrevertContext _ NilFL = return () -- nothing to do
removeFromUnrevertContext ref ps =
  ifDoesNotExistError () $ do
    debugMessage "Reading the unrevert bundle..."
    Sealed bundle <- unrevert_patch_bundle
    debugMessage "Adjusting the context of the unrevert changes..."
    debugMessage $
      "Removing " ++ show (lengthFL ps) ++ " patches in removeFromUnrevertContext"
    Sealed bundle_ps <- bundle_to_patchset bundle
    case patchSetMerge ref bundle_ps of
      (unrevert :>: NilFL) :/\: _ -> do
        case commuteRL (reverseFL ps :> unrevert) of
          Nothing -> unrevert_impossible
          Just (unrevert' :> _) ->
            case removeFromPatchSet ps ref of
              Nothing -> unrevert_impossible
              Just common -> do
                debugMessage "Have now found the new context..."
                bundle' <- makeBundle Nothing common (hopefully unrevert' :>: NilFL)
                writeDocBinFile tentativeUnrevertPath bundle'
      _ -> return () -- TODO I guess this should be an error call
    debugMessage "Done adjusting the context of the unrevert changes"
  where
    unrevert_impossible = do
      confirmed <-
        promptYorn "This operation will make unrevert impossible!\nProceed?"
      if confirmed
        then removeFileMayNotExist tentativeUnrevertPath
        else fail "Cancelled."
    unrevert_patch_bundle = do
      pf <- readBinFile tentativeUnrevertPath
      case parseBundle pf of
        Right foo -> return foo
        Left err -> fail $ "Couldn't parse unrevert patch:\n" ++ err
    bundle_to_patchset bundle =
      either fail (return . Sealed) $ interpretBundle ref bundle
