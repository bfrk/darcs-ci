module Darcs.UI.ApplyPatches
    ( PatchApplier(..)
    , PatchProxy(..)
    , StandardPatchApplier(..)
    , applyPatchesStart
    , applyPatchesFinish
    ) where

import Darcs.Prelude

import Control.Monad ( when, void )

import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd )
import Darcs.Util.SignalHandler ( withSignalsBlocked )
import Darcs.UI.Commands
    ( putVerbose
    , putFinished
    , setEnvDarcsPatches
    )
import Darcs.UI.Commands.Util ( printDryRunMessageAndExit )
import Darcs.UI.Flags
    ( DarcsFlag, verbosity, reorder, allowConflicts
    , wantGuiPause, diffingOpts, setScriptsExecutable, isInteractive
    , xmlOutput, dryRun
    )
import qualified Darcs.UI.Options.All as O
import Darcs.UI.Options ( (?) )
import Darcs.UI.Commands.Util ( testTentativeAndMaybeExit )
import Darcs.Repository
    ( Repository
    , AccessType(..)
    , tentativelyMergePatches
    , finalizeRepositoryChanges
    , applyToWorking
    , setScriptsExecutablePatches
    )
import Darcs.Repository.Pristine ( readPristine )
import Darcs.Repository.Job ( RepoJob(RepoJob) )
import Darcs.Patch ( RepoPatch, description )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Patch.FromPrim ( PrimOf )
import Darcs.Patch.Set ( PatchSet, Origin )
import Darcs.Patch.Witnesses.Ordered
    ( FL, Fork(..), mapFL, nullFL )
import Darcs.Patch.Witnesses.Sealed ( Sealed(Sealed) )

import Darcs.Util.English ( presentParticiple )
import Darcs.Util.Printer ( vcat, text )
import Darcs.Util.Tree( Tree )

data PatchProxy (p :: * -> * -> *) = PatchProxy

-- |This class is a hack to abstract over pull/apply and rebase pull/apply.
class PatchApplier pa where

    repoJob
        :: pa
        -> (forall p wR wU
               . (RepoPatch p, ApplyState p ~ Tree)
              => (PatchProxy p -> Repository 'RW p wU wR -> IO ()))
        -> RepoJob 'RW ()

    applyPatches
        :: forall p wR wU wZ
         . (RepoPatch p, ApplyState p ~ Tree)
        => pa
        -> PatchProxy p
        -> String
        -> [DarcsFlag]
        -> Repository 'RW p wU wR
        -> Fork (PatchSet p)
                (FL (PatchInfoAnd p))
                (FL (PatchInfoAnd p)) Origin wR wZ
        -> IO ()

data StandardPatchApplier = StandardPatchApplier

instance PatchApplier StandardPatchApplier where
    repoJob StandardPatchApplier f = RepoJob (f PatchProxy)
    applyPatches StandardPatchApplier PatchProxy = standardApplyPatches

standardApplyPatches :: (RepoPatch p, ApplyState p ~ Tree)
                     => String
                     -> [DarcsFlag]
                     -> Repository 'RW p wU wR
                     -> Fork (PatchSet p)
                             (FL (PatchInfoAnd p))
                             (FL (PatchInfoAnd p)) Origin wR wZ
                     -> IO ()
standardApplyPatches cmdName opts repository patches@(Fork _ _ to_be_applied) = do
    !no_patches <- return (nullFL to_be_applied)
    applyPatchesStart cmdName opts to_be_applied
    Sealed pw <- mergeAndTest cmdName opts repository patches
    applyPatchesFinish cmdName opts repository pw (not no_patches)

mergeAndTest :: (RepoPatch p, ApplyState p ~ Tree)
             => String
             -> [DarcsFlag]
             -> Repository 'RW p wU wR
             -> Fork (PatchSet p)
                     (FL (PatchInfoAnd p))
                     (FL (PatchInfoAnd p)) Origin wR wZ
             -> IO (Sealed (FL (PrimOf p) wU))
mergeAndTest cmdName opts repository patches = do
    pw <- tentativelyMergePatches repository cmdName
                         (allowConflicts opts)
                         (wantGuiPause opts)
                         (reorder ? opts) (diffingOpts opts)
                         patches
    tree <- readPristine repository
    testTentativeAndMaybeExit tree opts
        "those patches do not pass the tests." (cmdName ++ " them") Nothing
    return pw

applyPatchesStart :: (RepoPatch p, ApplyState p ~ Tree)
                  => String -> [DarcsFlag] -> FL (PatchInfoAnd p) wX wY -> IO ()
applyPatchesStart cmdName opts to_be_applied = do
    printDryRunMessageAndExit cmdName
        (verbosity ? opts)
        (O.withSummary ? opts)
        (dryRun ? opts)
        (xmlOutput ? opts)
        (isInteractive True opts)
        to_be_applied
    if nullFL to_be_applied then
        putStrLn $ "You don't want to " ++ cmdName ++ " any patches, and that's fine with me!"
    else do
        putVerbose opts $ text $ "Will " ++ cmdName ++ " the following patches:"
        putVerbose opts . vcat $ mapFL description to_be_applied
        setEnvDarcsPatches to_be_applied

applyPatchesFinish :: (RepoPatch p, ApplyState p ~ Tree)
                   => String
                   -> [DarcsFlag]
                   -> Repository 'RW p wU wR
                   -> FL (PrimOf p) wU wY
                   -> Bool
                   -> IO ()
applyPatchesFinish cmdName opts _repository pw any_applied = do
    withSignalsBlocked $ do
        _repository <- finalizeRepositoryChanges _repository (O.dryRun ? opts)
        void $ applyToWorking _repository (verbosity ? opts) pw
        when (setScriptsExecutable ? opts == O.YesSetScriptsExecutable) $
            setScriptsExecutablePatches pw
    case (any_applied, reorder ? opts == O.Reorder) of
        (True,True)  -> putFinished opts $ "reordering"
        (False,True) -> putFinished opts $ presentParticiple cmdName ++ " and reordering"
        _            -> putFinished opts $ presentParticiple cmdName

