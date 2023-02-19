--  Copyright (C) 2009 Ganesh Sittampalam
--
--  BSD3

module Darcs.UI.Commands.Rebase ( rebase ) where

import Darcs.Prelude

import Darcs.UI.Commands
    ( DarcsCommand(..), withStdOpts
    , normalCommand, hiddenCommand
    , commandAlias
    , defaultRepo, nodefaults
    , putInfo, putVerbose
    , amInHashedRepository
    )
import Darcs.UI.Commands.Apply ( applyCmd )
import Darcs.UI.Commands.Log ( changelog, logInfoFL )
import Darcs.UI.Commands.Pull ( pullCmd )
import Darcs.UI.Commands.Util ( historyEditHelp, preselectPatches )
import Darcs.UI.Completion ( fileArgs, prefArgs, noArgs )
import Darcs.UI.Flags
    ( DarcsFlag
    , externalMerge, allowConflicts
    , compress, diffingOpts
    , reorder, verbosity, verbose
    , useCache, wantGuiPause
    , umask, changesReverse
    , diffAlgorithm, isInteractive
    , selectDeps, hasXmlOutput
    )
import qualified Darcs.UI.Flags as Flags ( getAuthor )
import Darcs.UI.Options ( oid, (?), (^) )
import qualified Darcs.UI.Options.All as O
import Darcs.UI.PatchHeader
    ( AskAboutDeps(..)
    , HijackOptions(..)
    , HijackT
    , getAuthor
    , patchHeaderConfig
    , runHijackT
    , updatePatchHeader
    )
import Darcs.Repository
    ( Repository, RepoJob(..), AccessType(..), withRepoLock, withRepository
    , tentativelyAddPatches, finalizeRepositoryChanges
    , tentativelyRemovePatches, readPatches
    , setTentativePending, unrecordedChanges, applyToWorking
    )
import Darcs.Repository.Flags ( UpdatePending(..), ExternalMerge(..) )
import Darcs.Repository.Hashed ( upgradeOldStyleRebase )
import Darcs.Repository.Merge ( tentativelyMergePatches )
import Darcs.Repository.Rebase
    ( readRebase
    , readTentativeRebase
    , writeTentativeRebase
    )
import Darcs.Repository.Resolution
    ( StandardResolution(..)
    , rebaseResolution
    , announceConflicts
    )

import Darcs.Patch ( invert, effect, commute, RepoPatch, displayPatch )
import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch.CommuteFn ( commuterIdFL )
import Darcs.Patch.Info ( displayPatchInfo )
import Darcs.Patch.Match ( secondMatch, splitSecondFL )
import Darcs.Patch.Merge ( cleanMerge )
import Darcs.Patch.Named ( Named, fmapFL_Named, patchcontents, patch2patchinfo )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, hopefully, info, n2pia )
import Darcs.Patch.Prim ( canonizeFL, PrimPatch )
import Darcs.Patch.Rebase.Change
    ( RebaseChange(RC), rcToPia
    , extractRebaseChange, reifyRebaseChange
    , partitionUnconflicted
    , WithDroppedDeps(..), WDDNamed, commuterIdWDD
    , simplifyPush, simplifyPushes
    )
import Darcs.Patch.Rebase.Fixup ( RebaseFixup(..), flToNamesPrims )
import Darcs.Patch.Rebase.Name ( RebaseName(..), commuteNameNamed )
import Darcs.Patch.Rebase.Suspended ( Suspended(..), addToEditsToSuspended )
import qualified Darcs.Patch.Rebase.Suspended as S ( simplifyPush )
import Darcs.Patch.Permutations ( partitionConflictingFL )
import Darcs.Patch.Progress ( progressRL )
import Darcs.Patch.Set ( PatchSet, Origin, patchSet2RL )
import Darcs.Patch.Split ( primSplitter )
import Darcs.UI.ApplyPatches
    ( PatchApplier(..)
    , PatchProxy(..)
    , applyPatchesStart
    , applyPatchesFinish
    )
import Darcs.UI.External ( viewDocWith )
import Darcs.UI.SelectChanges
    ( runSelection, runInvertibleSelection
    , selectionConfig, selectionConfigGeneric, selectionConfigPrim
    , WhichChanges(First, Last, LastReversed)
    , viewChanges
    )
import qualified Darcs.UI.SelectChanges as S ( PatchSelectionOptions (..) )
import Darcs.Patch.Witnesses.Ordered
    ( FL(..), (+>+), mapFL_FL
    , concatFL, mapFL, nullFL, lengthFL, reverseFL
    , (:>)(..)
    , (:\/:)(..)
    , (:/\:)(..)
    , RL(..), reverseRL, mapRL_RL
    , Fork(..)
    )
import Darcs.Patch.Witnesses.Sealed
    ( Sealed(..), seal, unseal
    , FlippedSeal(..)
    , Sealed2(..)
    )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP )
import Darcs.Util.English ( englishNum, Noun(Noun) )
import Darcs.Util.Printer
    ( text, ($$), redText
    , simplePrinters
    , renderString
    , formatWords
    , formatText
    , ($+$)
    )
import Darcs.Util.Printer.Color ( fancyPrinters )
import Darcs.Util.Progress ( debugMessage )
import Darcs.Util.Path ( AbsolutePath )

import Darcs.Util.SignalHandler ( withSignalsBlocked )
import Darcs.Util.Tree ( Tree )

import Control.Monad ( unless, when, void )
import Control.Monad.Trans ( liftIO )
import System.Exit ( exitSuccess )

rebase :: DarcsCommand
rebase = SuperCommand
    { commandProgramName = "darcs"
    , commandName = "rebase"
    , commandHelp = rebaseHelp
    , commandDescription = rebaseDescription
    , commandPrereq = amInHashedRepository
    , commandSubCommands =
        [ normalCommand pull
        , normalCommand apply
        , normalCommand suspend
        , normalCommand unsuspend
        , hiddenCommand reify
        , hiddenCommand inject
        , normalCommand obliterate
        , normalCommand log
        , hiddenCommand changes
        , normalCommand upgrade
        ]
    }
  where
    rebaseDescription = "Edit several patches at once."
    rebaseHelp = formatText 80
      [ "The `darcs rebase' command is used to edit a collection of darcs patches."
      , "The basic idea is that you can suspend patches from the end of\
        \ a repository. These patches are no longer part of the history and\
        \ have no effect on the working tree. Suspended patches are invisible\
        \ to commands that access the repository from the outside, such as\
        \ push, pull, clone, send, etc."
      , "The sequence of suspended patches can be manipulated in ways that are\
        \ not allowed for normal patches. For instance, `darcs rebase obliterate`\
        \ allows you to remove a patch in this sequence, even if other suspended\
        \ patches depend on it. These other patches will as a result become\
        \ conflicted."
      , "You can also operate on the normal patches in the usual way. If you add\
        \ or remove normal patches, the suspended patches will be automatically\
        \ adapted to still apply to the pristine state, possibly becoming\
        \ conflicted in the course."
      , "Note that as soon as a patch gets suspended, it will irrevocably loose\
        \ its identity. This means that suspending a patch is subject to the\
        \ usual warnings about editing the history of your project."
      , "The opposite of suspending a patch is to unsuspend it.\
        \ This turns it back into a normal patch.\
        \ If the patch is conflicted as a result of previous operations on\
        \ either the normal patches or the suspended patches, unsuspending\
        \ will create appropriate conflict markup. Note, however, that the\
        \ unsuspended patch itself WILL NOT BE CONFLICTED itself. This means\
        \ that there is no way to re-generate the conflict markup. Once you\
        \ removed it, by editing files or using `darcs revert`, any information\
        \ about the conflict is lost."
      , "As long as you have suspended patches, darcs will display a short\
        \ message after each command to remind you that your patch editing\
        \ operation is still in progress."
      ]

suspend :: DarcsCommand
suspend = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "suspend"
    , commandHelp = text suspendDescription $+$ historyEditHelp
    , commandDescription = suspendDescription
    , commandPrereq = amInHashedRepository
    , commandExtraArgs = 0
    , commandExtraArgHelp = []
    , commandCommand = suspendCmd
    , commandCompleteArgs = noArgs
    , commandArgdefaults = nodefaults
    , commandOptions = suspendOpts
    }
  where
    suspendBasicOpts
      = O.notInRemote
      ^ O.matchSeveralOrLast
      ^ O.selectDeps
      ^ O.interactive
      ^ O.withSummary
      ^ O.diffAlgorithm
    suspendAdvancedOpts
      = O.changesReverse
      ^ O.umask
    suspendOpts = suspendBasicOpts `withStdOpts` suspendAdvancedOpts
    suspendDescription =
      "Select patches to move into a suspended state at the end of the repo."

suspendCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
suspendCmd _ opts _args =
    withRepoLock (useCache ? opts) (umask ? opts) $
    StartRebaseJob $
    \_repository -> do
    suspended <- readTentativeRebase _repository
    (_ :> candidates) <- preselectPatches opts _repository
    let direction = if changesReverse ? opts then Last else LastReversed
        selection_config = selectionConfig
                              direction "suspend" (patchSelOpts True opts) Nothing Nothing
    (_ :> psToSuspend) <-
        runSelection
            candidates
            selection_config
    when (nullFL psToSuspend) $ do
        putStrLn "No patches selected!"
        exitSuccess
    -- test all patches for hijacking and abort if rejected
    runHijackT RequestHijackPermission
        $ mapM_ (getAuthor "suspend" False Nothing)
        $ mapFL info psToSuspend
    _repository <- doSuspend opts _repository suspended psToSuspend
    _repository <-
      finalizeRepositoryChanges _repository YesUpdatePending
        (compress ? opts) (O.dryRun ? opts)
    return ()

doSuspend
    :: (RepoPatch p, ApplyState p ~ Tree)
    => [DarcsFlag]
    -> Repository 'RW p wU wR
    -> Suspended p wR
    -> FL (PatchInfoAnd p) wX wR
    -> IO (Repository 'RW p wU wX)
doSuspend opts _repository suspended psToSuspend = do
    pend <- unrecordedChanges (diffingOpts opts) _repository Nothing
    FlippedSeal psAfterPending <-
        let effectPsToSuspend = effect psToSuspend in
        case commute (effectPsToSuspend :> pend) of
            Just (_ :> res) -> return (FlippedSeal res)
            Nothing -> do
                putVerbose opts $
                    let invPsEffect = invert effectPsToSuspend
                    in
                    case (partitionConflictingFL invPsEffect pend, partitionConflictingFL pend invPsEffect) of
                        (_ :> invSuspendedConflicts, _ :> pendConflicts) ->
                            let suspendedConflicts = invert invSuspendedConflicts in
                            redText "These changes in the suspended patches:" $$
                            displayPatch suspendedConflicts $$
                            redText "...conflict with these local changes:" $$
                            displayPatch pendConflicts
                fail $ "Can't suspend selected patches without reverting some unrecorded change."
                    ++ if (verbose opts) then "" else " Use --verbose to see the details."

    _repository <-
      tentativelyRemovePatches _repository (compress ? opts) NoUpdatePending psToSuspend
    new_suspended <-
      addToEditsToSuspended
        (O.diffAlgorithm ? opts)
        (mapFL_FL hopefully psToSuspend)
        suspended
    writeTentativeRebase _repository new_suspended
    withSignalsBlocked $
      unless (O.yes (O.dryRun ? opts)) $
        void $ applyToWorking _repository (verbosity ? opts) (invert psAfterPending)
    return _repository

unsuspend :: DarcsCommand
unsuspend = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "unsuspend"
    , commandHelp = text unsuspendDescription
    , commandDescription = unsuspendDescription
    , commandPrereq = amInHashedRepository
    , commandExtraArgs = 0
    , commandExtraArgHelp = []
    , commandCommand = unsuspendCmd "unsuspend" False
    , commandCompleteArgs = noArgs
    , commandArgdefaults = nodefaults
    , commandOptions = unsuspendOpts
    }
  where
    unsuspendBasicOpts
      = O.conflictsYes
      ^ O.matchSeveralOrFirst
      ^ O.interactive
      ^ O.withSummary
      ^ O.externalMerge
      ^ O.author
      ^ O.selectAuthor
      ^ O.patchname
      ^ O.askDeps
      ^ O.askLongComment
      ^ O.keepDate
      ^ O.diffAlgorithm
    unsuspendOpts = unsuspendBasicOpts `withStdOpts` oid
    unsuspendDescription =
      "Select suspended patches to restore to the end of the repo."

reify :: DarcsCommand
reify = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "reify"
    , commandHelp = text reifyDescription
    , commandDescription = reifyDescription
    , commandPrereq = amInHashedRepository
    , commandExtraArgs = 0
    , commandExtraArgHelp = []
    , commandCommand = unsuspendCmd "reify" True
    , commandCompleteArgs = noArgs
    , commandArgdefaults = nodefaults
    , commandOptions = reifyOpts
    }
  where
    reifyBasicOpts
      = O.matchSeveralOrFirst
      ^ O.interactive
      ^ O.withSummary
      ^ O.keepDate
      ^ O.author
      ^ O.diffAlgorithm
    reifyOpts = reifyBasicOpts `withStdOpts` O.umask
    reifyDescription =
      "Select suspended patches to restore to the end of the repo,\
      \ reifying any fixup patches."

unsuspendCmd :: String -> Bool -> (AbsolutePath, AbsolutePath)
             -> [DarcsFlag] -> [String] -> IO ()
unsuspendCmd cmd reifyFixups _ opts _args =
  withRepoLock (useCache ? opts) (umask ? opts) $ RebaseJob $ \_repository -> do
    Items suspended <- readTentativeRebase _repository

    let matchFlags = O.matchSeveralOrFirst ? opts
    inRange :> outOfRange <-
        return $
            if secondMatch matchFlags then
            splitSecondFL rcToPia matchFlags suspended
            else suspended :> NilFL

    offer :> dontoffer <-
        return $
            case O.conflictsYes ? opts of
              Nothing -> partitionUnconflicted inRange -- skip conflicts
              Just _ -> inRange :> NilRL

    let warnSkip NilRL = return ()
        warnSkip _ = putStrLn "Skipping some patches which would cause conflicts."

    warnSkip dontoffer

    let selection_config =
          selectionConfigGeneric rcToPia First cmd
            (patchSelOpts True opts) Nothing
    (chosen :> keep) <- runSelection offer selection_config
    when (nullFL chosen) $ do putStrLn "No patches selected!"
                              exitSuccess

    ps_to_unsuspend :> chosen_fixups <-
      if reifyFixups
        then do
          author <- Flags.getAuthor (O.author ? opts) False
          reifyRebaseChange author chosen
        else return $ extractRebaseChange (diffAlgorithm ? opts) chosen

    let ps_to_keep = simplifyPushes da chosen_fixups $
                     keep +>+ reverseRL dontoffer +>+ outOfRange

    context <- readPatches _repository

    let conflicts =
          rebaseResolution (patchSet2RL context) $
          progressRL "Examining patches for conflicts" $
          mapRL_RL wddPatch $
          reverseFL ps_to_unsuspend

    have_conflicts <- announceConflicts cmd (allowConflicts opts) conflicts
    Sealed resolution <-
        case (externalMerge ? opts, have_conflicts) of
            (NoExternalMerge, _) ->
                case O.conflictsYes ? opts of
                    Just O.YesAllowConflicts ->
                      return $ seal NilFL -- i.e. don't mark them
                    _ -> return $ mangled conflicts
            (_, False) -> return $ mangled conflicts
            (YesExternalMerge _, True) ->
                error $ "external resolution for "++cmd++" not implemented yet"

    unrec <- unrecordedChanges (diffingOpts opts) _repository Nothing

    -- TODO should catch logfiles (fst value from updatePatchHeader) and
    -- clean them up as in AmendRecord
    -- Note: we can hijack because we already asked about that on suspend time
    (unsuspended_ps, ps_to_keep') <-
      runHijackT IgnoreHijack $ hijack ps_to_unsuspend (unseal Items ps_to_keep)
    _repository <-
      tentativelyAddPatches _repository (compress ? opts) (verbosity ? opts)
        NoUpdatePending unsuspended_ps
    let effect_unsuspended = concatFL (mapFL_FL effect unsuspended_ps)
    case cleanMerge (effect_unsuspended :\/: unrec) of
      Nothing ->
        fail $ "Can't "++cmd++" because there are conflicting unrecorded changes."
      Just (unrec' :/\: effect_unsuspended') ->
        case cleanMerge (resolution :\/: unrec') of
          Nothing ->
            fail $ "Can't "++cmd++" because there are conflicting unrecorded changes."
          Just (unrec'' :/\: resolution') -> do
            let effect_to_apply = effect_unsuspended' +>+ resolution'
            setTentativePending _repository (resolution +>+ unrec'')
            writeTentativeRebase _repository ps_to_keep'
            withSignalsBlocked $ do
              _repository <-
                finalizeRepositoryChanges _repository YesUpdatePending
                  (compress ? opts) (O.dryRun ? opts)
              unless (O.yes (O.dryRun ? opts)) $
                void $ applyToWorking _repository (verbosity ? opts) effect_to_apply

    where da = diffAlgorithm ? opts

          hijack :: forall p wR wT. (RepoPatch p, ApplyState p ~ Tree)
                 => FL (WDDNamed p) wR wT
                 -> Suspended p wT
                 -> HijackT IO (FL (PatchInfoAnd p) wR wT, Suspended p wT)
          hijack NilFL to_keep = return (NilFL, to_keep)
          hijack (p :>: ps) to_keep = do
              case wddDependedOn p of
                  [] -> return ()
                  deps -> liftIO $ do
                      -- It might make sense to only print out this message
                      -- once, but we might find that the dropped dependencies
                      -- are interspersed with other output, e.g. if running
                      -- with --ask-deps
                      putStr $ "Warning: dropping the following explicit "
                            ++ englishNum (length deps) (Noun "dependency") ":\n\n"
                      let printIndented n =
                              mapM_ (putStrLn . (replicate n ' '++)) . lines .
                              renderString . displayPatchInfo
                      putStrLn . renderString . displayPatchInfo .
                              patch2patchinfo $ wddPatch p
                      putStr " depended on:\n"
                      mapM_ (printIndented 2) deps
                      putStr "\n"

              -- TODO should catch logfiles (fst value from updatePatchHeader)
              -- and clean them up as in AmendRecord
              p' <- snd <$> updatePatchHeader cmd
                      NoAskAboutDeps
                      (patchSelOpts True opts)
                      (patchHeaderConfig opts)
                      (fmapFL_Named effect (wddPatch p)) NilFL
              -- create a rename that undoes the change we just made
              let rename = Rename (info p') (patch2patchinfo (wddPatch p))
              -- push it through the remaining patches to fix them up
              Just (ps2 :> rename2) <-
                return $ commuterIdFL (commuterIdWDD commuteNameNamed) (rename :> ps)
              to_keep' <- return $ S.simplifyPush da (NameFixup rename2) to_keep
              (converted, to_keep'') <- hijack ps2 to_keep'
              -- the rename still has a null effect on the context after
              -- commuting, but it is not easy to convince the type-checker,
              -- so we just coerce here
              return $ unsafeCoerceP (p' :>: converted, to_keep'')

inject :: DarcsCommand
inject = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "inject"
    , commandHelp = text injectDescription
    , commandDescription = injectDescription
    , commandPrereq = amInHashedRepository
    , commandExtraArgs = 0
    , commandExtraArgHelp = []
    , commandCommand = injectCmd
    , commandCompleteArgs = noArgs
    , commandArgdefaults = nodefaults
    , commandOptions = injectOpts
    }
  where
    injectBasicOpts = O.keepDate ^ O.author ^ O.diffAlgorithm
    injectOpts = injectBasicOpts `withStdOpts` O.umask
    injectDescription =
      "Merge a change from the fixups of a patch into the patch itself."

injectCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
injectCmd _ opts _args =
    withRepoLock (useCache ? opts) (umask ? opts) $
    RebaseJob $
    \(_repository :: Repository 'RW p wU wR) -> do
    Items selects <- readTentativeRebase _repository

    -- TODO this selection doesn't need to respect dependencies
    -- TODO we only want to select one patch: generalise withSelectedPatchFromList
    let selection_config =
          selectionConfigGeneric rcToPia First "inject into" (patchSelOpts True opts) Nothing
    (chosens :> rest_selects) <- runSelection selects selection_config

    let extractSingle :: FL (RebaseChange prim) wX wY -> (FL (RebaseFixup prim) :> Named prim) wX wY
        extractSingle (RC fixups toedit :>: NilFL) = fixups :> toedit
        extractSingle _ = error "You must select precisely one patch!"

    fixups :> toedit <- return $ extractSingle chosens

    name_fixups :> prim_fixups <- return $ flToNamesPrims fixups

    let prim_selection_config =
          selectionConfigPrim
              Last "inject" (patchSelOpts True opts)
              (Just (primSplitter (diffAlgorithm ? opts))) Nothing Nothing
    (rest_fixups :> injects) <- runInvertibleSelection prim_fixups prim_selection_config

    when (nullFL injects) $ do
        putStrLn "No changes selected!"
        exitSuccess

    -- Don't bother to update patch header since unsuspend will do that later
    let da = diffAlgorithm ? opts
        toeditNew = fmapFL_Named (canonizeFL da . (injects +>+)) toedit
    writeTentativeRebase _repository $
      unseal Items $
      unseal (simplifyPushes da (mapFL_FL NameFixup name_fixups)) $
      simplifyPushes da (mapFL_FL PrimFixup rest_fixups) $
      RC NilFL toeditNew :>: rest_selects
    _repository <-
      finalizeRepositoryChanges _repository YesUpdatePending
        (compress ? opts) (O.dryRun ? opts)
    return ()

obliterate :: DarcsCommand
obliterate = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "obliterate"
    , commandHelp = text obliterateDescription
    , commandDescription = obliterateDescription
    , commandPrereq = amInHashedRepository
    , commandExtraArgs = 0
    , commandExtraArgHelp = []
    , commandCommand = obliterateCmd
    , commandCompleteArgs = noArgs
    , commandArgdefaults = nodefaults
    , commandOptions = obliterateOpts
    }
  where
    obliterateBasicOpts = O.diffAlgorithm
    obliterateOpts = obliterateBasicOpts `withStdOpts` O.umask
    obliterateDescription =
      "Obliterate a patch that is currently suspended."

obliterateCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
obliterateCmd _ opts _args =
    withRepoLock (useCache ? opts) (umask ? opts) $
    RebaseJob $
    \(_repository :: Repository 'RW p wU wR) -> (do
    Items selects <- readTentativeRebase _repository

    -- TODO this selection doesn't need to respect dependencies
    let selection_config = selectionConfigGeneric rcToPia First "obliterate" (obliteratePatchSelOpts opts) Nothing
    (chosen :> keep) <- runSelection selects selection_config
    when (nullFL chosen) $ do putStrLn "No patches selected!"
                              exitSuccess

    let da = diffAlgorithm ? opts
        do_obliterate
          :: PrimPatch prim
          => FL (RebaseChange prim) wX wY
          -> FL (RebaseChange prim) wY wZ
          -> Sealed (FL (RebaseChange prim) wX)
        do_obliterate NilFL = Sealed
        do_obliterate (RC fs e :>: qs) =
          unseal (simplifyPushes da fs) .
          -- since Named doesn't have any witness context for the
          -- patch names, the AddName here will be inferred to be wX wX
          unseal (simplifyPush da (NameFixup (AddName (patch2patchinfo e)))) .
          unseal (simplifyPushes da (mapFL_FL PrimFixup (patchcontents e))) .
          do_obliterate qs

    let ps_to_keep = do_obliterate chosen keep
    writeTentativeRebase _repository (unseal Items ps_to_keep)

    _repository <-
      finalizeRepositoryChanges _repository YesUpdatePending
        (compress ? opts) (O.dryRun ? opts)
    return ()
   ) :: IO ()


pull :: DarcsCommand
pull = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "pull"
    , commandHelp = text pullDescription
    , commandDescription = pullDescription
    , commandExtraArgs = -1
    , commandExtraArgHelp = ["[REPOSITORY]..."]
    , commandCommand = pullCmd RebasePatchApplier
    , commandPrereq = amInHashedRepository
    , commandCompleteArgs = prefArgs "repos"
    , commandArgdefaults = defaultRepo
    , commandOptions = pullOpts
    }
  where
    pullBasicOpts
      = O.matchSeveral
      ^ O.reorder
      ^ O.interactive
      ^ O.conflictsYes
      ^ O.externalMerge
      ^ O.testChanges
      ^ O.dryRunXml
      ^ O.withSummary
      ^ O.selectDeps
      ^ O.repoDir
      ^ O.allowUnrelatedRepos
      ^ O.diffAlgorithm
    pullAdvancedOpts
      = O.repoCombinator
      ^ O.compress
      ^ O.remoteRepos
      ^ O.setScriptsExecutable
      ^ O.umask
      ^ O.changesReverse
      ^ O.remoteDarcs
    pullOpts = pullBasicOpts `withStdOpts` pullAdvancedOpts
    pullDescription =
      "Copy and apply patches from another repository,\
      \ suspending any local patches that conflict."

stdindefault :: a -> [String] -> IO [String]
stdindefault _ [] = return ["-"]
stdindefault _ x = return x

apply :: DarcsCommand
apply = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "apply"
    , commandHelp = text applyDescription
    , commandDescription = applyDescription
    , commandExtraArgs = 1
    , commandExtraArgHelp = ["<PATCHFILE>"]
    , commandCommand = applyCmd RebasePatchApplier
    , commandPrereq = amInHashedRepository
    , commandCompleteArgs = fileArgs
    , commandArgdefaults = const stdindefault
    , commandOptions = applyOpts
    }
  where
    applyBasicOpts
      = O.verify
      ^ O.reorder
      ^ O.interactive
      ^ O.dryRunXml
      ^ O.matchSeveral
      ^ O.repoDir
      ^ O.diffAlgorithm
    applyAdvancedOpts
      = O.compress
      ^ O.setScriptsExecutable
      ^ O.umask
      ^ O.changesReverse
      ^ O.pauseForGui
    applyOpts = applyBasicOpts `withStdOpts` applyAdvancedOpts
    applyDescription =
      "Apply a patch bundle, suspending any local patches that conflict."

data RebasePatchApplier = RebasePatchApplier

instance PatchApplier RebasePatchApplier where
    repoJob RebasePatchApplier f = StartRebaseJob (f PatchProxy)
    applyPatches RebasePatchApplier PatchProxy = applyPatchesForRebaseCmd

applyPatchesForRebaseCmd
    :: forall p wR wU wZ
     . ( RepoPatch p, ApplyState p ~ Tree )
    => String
    -> [DarcsFlag]
    -> Repository 'RW p wU wR
    -> Fork (PatchSet p)
            (FL (PatchInfoAnd p))
            (FL (PatchInfoAnd p)) Origin wR wZ
    -> IO ()
applyPatchesForRebaseCmd cmdName opts _repository (Fork common us' to_be_applied) = do
    applyPatchesStart cmdName opts to_be_applied

    usOk :> usConflicted <- return $ partitionConflictingFL us' to_be_applied

    when (lengthFL usConflicted > 0) $
        putInfo opts $ text "The following local patches are in conflict:"

    -- TODO: we assume the options apply only to the main
    -- command, review if there are any we should keep
    let selection_config = selectionConfig LastReversed "suspend" applyPatchSelOpts Nothing Nothing

    (usKeep :> usToSuspend) <- runSelection usConflicted selection_config

    -- test all patches for hijacking and abort if rejected
    runHijackT RequestHijackPermission
        $ mapM_ (getAuthor "suspend" False Nothing)
        $ mapFL info usToSuspend

    suspended <- readTentativeRebase _repository

    _repository <- doSuspend opts _repository suspended usToSuspend
    -- the new rebase patch containing the suspended patches is now in the repo
    -- and the suspended patches have been removed

    Sealed pw <-
        tentativelyMergePatches
            _repository cmdName
            (allowConflicts opts)
            (externalMerge ? opts)
            (wantGuiPause opts) (compress ? opts) (verbosity ? opts)
            (reorder ? opts) (diffingOpts opts)
            (Fork common (usOk +>+ usKeep) to_be_applied)

    applyPatchesFinish cmdName opts _repository pw (nullFL to_be_applied)

-- TODO I doubt this is right, e.g. withContext should be inherited
applyPatchSelOpts :: S.PatchSelectionOptions
applyPatchSelOpts = S.PatchSelectionOptions
    { S.verbosity = O.NormalVerbosity
    , S.matchFlags = []
    , S.interactive = True
    , S.selectDeps = O.PromptDeps -- option not supported, use default
    , S.withSummary = O.NoSummary
    , S.withContext = O.NoContext
    }

obliteratePatchSelOpts :: [DarcsFlag] -> S.PatchSelectionOptions
obliteratePatchSelOpts opts = (patchSelOpts True opts)
    { S.selectDeps = O.NoDeps
    }

patchSelOpts :: Bool -> [DarcsFlag] -> S.PatchSelectionOptions
patchSelOpts defInteractive flags = S.PatchSelectionOptions
    { S.verbosity = verbosity ? flags
    , S.matchFlags = O.matchSeveralOrLast ? flags
    , S.interactive = isInteractive defInteractive flags
    , S.selectDeps = selectDeps ? flags
    , S.withSummary = O.withSummary ? flags
    , S.withContext = O.NoContext
    }

log :: DarcsCommand
log = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "log"
    , commandHelp = text logDescription
    , commandDescription = logDescription
    , commandPrereq = amInHashedRepository
    , commandExtraArgs = 0
    , commandExtraArgHelp = []
    , commandCommand = logCmd
    , commandCompleteArgs = noArgs
    , commandArgdefaults = nodefaults
    , commandOptions = logOpts
    }
  where
    logBasicOpts = O.withSummary ^ O.interactive -- False
    logAdvancedOpts = oid
    logOpts = logBasicOpts `withStdOpts` logAdvancedOpts
    logDescription = "List the currently suspended changes."

logCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
logCmd _ opts _files =
    withRepository (useCache ? opts) $
    RebaseJob $ \_repository -> do
        Items ps <- readRebase _repository
        let psToShow = mapFL_FL n2pia ps
        if isInteractive False opts
            then viewChanges (patchSelOpts False opts) (mapFL Sealed2 psToShow)
            else do
                debugMessage "About to print the changes..."
                let printers = if hasXmlOutput opts then simplePrinters else fancyPrinters
                let logDoc = changelog opts (reverseFL psToShow) (logInfoFL psToShow)
                viewDocWith printers logDoc

-- | changes is an alias for log
changes :: DarcsCommand
changes = commandAlias "changes" Nothing log

upgrade :: DarcsCommand
upgrade = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "upgrade"
    , commandHelp = help
    , commandDescription = desc
    , commandPrereq = amInHashedRepository
    , commandExtraArgs = 0
    , commandExtraArgHelp = []
    , commandCommand = upgradeCmd
    , commandCompleteArgs = noArgs
    , commandArgdefaults = nodefaults
    , commandOptions = opts
    }
  where
    basicOpts = oid
    opts = basicOpts `withStdOpts` O.umask
    desc = "Upgrade a repo with an old-style rebase in progress."
    help = text desc $+$ formatWords
      [ "Doing this means you won't be able to use darcs version < 2.15"
      , "with this repository until the rebase is finished."
      ]

upgradeCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
upgradeCmd _ opts _args =
  withRepoLock (useCache ? opts) (umask ? opts) $
  OldRebaseJob $ \(_repo :: Repository 'RW p wU wR) ->
    upgradeOldStyleRebase _repo (compress ? opts)

{-
TODO:

 - amend-record shows the diff between the conflicted state and the
   resolution, which is unhelpful
 - make aggregate commands
 - argument handling
 - what should happen to patch comment on unsuspend?
 - warn about suspending conflicts
 - indication of expected conflicts on unsuspend
    - why isn't ! when you do x accurate?
 - rebase pull/apply should suspend patches such that their order is not changed
 - amended patches will often be in both the target repo and in the rebase context, detect?
 - can we be more intelligent about conflict resolutions?
 - --all option to unsuspend
 - review other conflict options for unsuspend
 - darcs check should check integrity of rebase patch
 - review existence of reify and inject commands - bit of an internals hack
-}
