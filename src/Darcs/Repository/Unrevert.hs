module Darcs.Repository.Unrevert
    ( finalizeTentativeUnrevert
    , revertTentativeUnrevert
    , writeUnrevert
    , unrevertPatchBundle
    , removeFromUnrevertContext
    ) where

import Darcs.Prelude
import Darcs.Patch ( PrimOf, RepoPatch, commuteRL )
import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch.Bundle ( interpretBundle, makeBundle, parseBundle )
import Darcs.Patch.Depends ( mergeThem, removeFromPatchSet )
import Darcs.Patch.Info ( patchinfo )
import Darcs.Patch.Named ( infopatch )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, hopefully )
import Darcs.Patch.Set ( Origin, PatchSet, SealedPatchSet )
import Darcs.Patch.Witnesses.Ordered ( (:>)(..), FL(..), lengthFL, reverseFL )
import Darcs.Patch.Witnesses.Sealed ( Sealed(Sealed) )
import Darcs.Repository.Paths ( tentativeUnrevertPath, unrevertPath )
import Darcs.Util.Exception ( catchDoesNotExistError )
import Darcs.Util.Global ( debugMessage )
import Darcs.Util.IsoDate ( getIsoDateTime )
import Darcs.Util.Lock ( readBinFile, removeFileMayNotExist, writeDocBinFile )
import Darcs.Util.Prompt ( promptYorn )
import Darcs.Util.Tree ( Tree )

import System.Directory ( copyFile, renameFile )
import System.Exit ( exitSuccess )

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
              -> Tree IO
              -> FL (PrimOf p) wR wX
              -> IO ()
writeUnrevert _ _ NilFL = removeFileMayNotExist tentativeUnrevertPath
writeUnrevert recorded pristine ps = do
  date <- getIsoDateTime
  info <- patchinfo date "unrevert" "anon" []
  let np = infopatch info ps
  bundle <- makeBundle (Just pristine) recorded (np :>: NilFL)
  writeDocBinFile tentativeUnrevertPath bundle

unrevertPatchBundle :: RepoPatch p
                    => PatchSet p Origin wR
                    -> IO (SealedPatchSet p Origin)
unrevertPatchBundle us = do
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
removeFromUnrevertContext ref ps = (do
  debugMessage "Reading the unrevert bundle..."
  Sealed bundle <- unrevert_patch_bundle
  debugMessage "Adjusting the context of the unrevert changes..."
  debugMessage $
    "Removing " ++ show (lengthFL ps) ++ " patches in removeFromUnrevertContext"
  Sealed bundle_ps <- bundle_to_patchset bundle
  case mergeThem ref bundle_ps of
    Sealed (h_us :>: NilFL) -> do
      case commuteRL (reverseFL ps :> h_us) of
        Nothing -> unrevert_impossible
        Just (us' :> _) ->
          case removeFromPatchSet ps ref of
            Nothing -> unrevert_impossible
            Just common -> do
              debugMessage "Have now found the new context..."
              bundle' <- makeBundle Nothing common (hopefully us' :>: NilFL)
              writeDocBinFile tentativeUnrevertPath bundle'
    Sealed _ -> return () -- TODO I guess this should be an error call
  debugMessage "Done adjusting the context of the unrevert changes")
  `catchDoesNotExistError` return ()
  where
    unrevert_impossible = do
      confirmed <-
        promptYorn "This operation will make unrevert impossible!\nProceed?"
      if confirmed
        then removeFileMayNotExist tentativeUnrevertPath
        else putStrLn "Cancelled." >> exitSuccess
    unrevert_patch_bundle = do
      pf <- readBinFile tentativeUnrevertPath
      case parseBundle pf of
        Right foo -> return foo
        Left err -> fail $ "Couldn't parse unrevert patch:\n" ++ err
    bundle_to_patchset bundle =
      either fail (return . Sealed) $ interpretBundle ref bundle
