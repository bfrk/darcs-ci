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
    , putInfo
    , amInHashedRepository
    )
import Darcs.UI.Commands.Apply ( applyCmd )
import Darcs.UI.Commands.Log ( changelog, logInfoFL )
import Darcs.UI.Commands.Pull ( pullCmd )
import Darcs.UI.Commands.Util ( historyEditHelp, preselectPatches )
import Darcs.UI.Completion ( Pref(Repos), fileArgs, prefArgs, noArgs )
import Darcs.UI.Flags
    ( DarcsFlag
    , allowConflicts
    , diffingOpts
    , reorder, verbosity
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
    , editLog
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
import Darcs.Repository.Flags
    ( AllowConflicts(..)
    , ResolveConflicts(..)
    , UpdatePending(..)
    )
import Darcs.Repository.Merge ( tentativelyMergePatches )
import Darcs.Repository.Rebase
    ( checkHasRebase
    , readRebase
    , readTentativeRebase
    , writeTentativeRebase
    )
import Darcs.Repository.Resolution
    ( StandardResolution(..)
    , rebaseResolution
    , announceConflicts
    )
import Darcs.Repository.State ( updateIndex )
import Darcs.Repository.Transaction ( upgradeOldStyleRebase )

import Darcs.Patch ( PrimOf, invert, effect, commute, RepoPatch )
import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch.CommuteFn ( commuterFLId, commuterIdFL )
import Darcs.Patch.Info ( displayPatchInfo, piName )
import Darcs.Patch.Match ( secondMatch, splitSecondFL )
import Darcs.Patch.Merge ( cleanMerge )
import Darcs.Patch.Named ( fmapFL_Named, patchcontents, patch2patchinfo )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, hopefully, info, n2pia )
import Darcs.Patch.Prim ( canonizeFL, PrimPatch )
import Darcs.Patch.Rebase.Change
    ( RebaseChange(RC), rcToPia
    , extractRebaseChange, reifyRebaseChange
    , partitionUnconflicted
    , WithDroppedDeps(..), WDDNamed, commuterIdWDD
    , simplifyPush, simplifyPushes
    , forceCommuteRebaseChange
    )
import Darcs.Patch.Rebase.Fixup
    ( RebaseFixup(..)
    , commuteNamedFixup
    , flToNamesPrims
    , primNamedToFixups
    )
import Darcs.Patch.Rebase.Name ( RebaseName(..), commuteNameNamed )
import Darcs.Patch.Rebase.Suspended ( Suspended(..), addToEditsToSuspended )
import qualified Darcs.Patch.Rebase.Suspended as S ( simplifyPush )
import Darcs.Patch.Permutations ( genCommuteWhatWeCanFL, partitionConflictingFL )
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
import Darcs.UI.PrintPatch
    ( printContent
    , printContentWithPager
    , printFriendly
    , printSummary
    )
import Darcs.UI.Prompt ( PromptChoice(..), PromptConfig(..), runPrompt )
import Darcs.UI.SelectChanges
    ( runSelection, runInvertibleSelection
    , selectionConfig, selectionConfigGeneric, selectionConfigPrim
    , WhichChanges(First, Last, LastReversed)
    , viewChanges
    )
import qualified Darcs.UI.SelectChanges as S ( PatchSelectionOptions (..) )
import Darcs.Patch.Witnesses.Eq ( EqCheck(..) )
import Darcs.Patch.Witnesses.Ordered
    ( FL(..), (+>+), mapFL_FL
    , concatFL, mapFL, nullFL, lengthFL, reverseFL
    , (:>)(..)
    , (:\/:)(..)
    , (:/\:)(..)
    , RL(..), reverseRL, mapRL_RL
    , Fork(..)
    , (+>>+)
    )
import Darcs.Patch.Witnesses.Sealed
    ( Sealed(..), seal, unseal, mapSeal
    , Sealed2(..)
    )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP )
import Darcs.Util.English ( englishNum, Noun(Noun) )
import Darcs.Util.Printer
    ( text, redText
    , putDocLnWith, prefix
    , simplePrinters
    , formatWords
    , formatText
    , vcat
    , ($+$), ($$)
    )
import Darcs.Util.Printer.Color ( fancyPrinters )
import Darcs.Util.Progress ( debugMessage )
import Darcs.Util.Prompt ( promptYorn )
import Darcs.Util.Path ( AbsolutePath )

import Darcs.Util.SignalHandler ( withSignalsBlocked )
import Darcs.Util.Tree ( Tree )

import Control.Exception ( throwIO, try )
import Control.Monad ( mplus, unless, when, void )
import Control.Monad.Trans ( liftIO )
import System.Exit ( ExitCode(ExitSuccess), exitSuccess )

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
        , normalCommand edit
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
  withRepoLock (useCache ? opts) (umask ? opts) $ RepoJob $ \_repository -> do
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
    (_repository, Sealed toWorking) <-
      doSuspend "suspend" opts _repository suspended psToSuspend
    withSignalsBlocked $ do
      void $ finalizeRepositoryChanges _repository (O.dryRun ? opts)
      unless (O.yes (O.dryRun ? opts)) $
        void $ applyToWorking _repository (verbosity ? opts) toWorking

doSuspend
    :: (RepoPatch p, ApplyState p ~ Tree)
    => String
    -> [DarcsFlag]
    -> Repository 'RW p wU wR
    -> Suspended p wR
    -> FL (PatchInfoAnd p) wX wR
    -> IO (Repository 'RW p wU wX, Sealed (FL (PrimOf p) wU))
doSuspend cmdname opts _repository suspended to_suspend = do
  unrecorded <- unrecordedChanges (diffingOpts opts) _repository Nothing
  case genCommuteWhatWeCanFL (commuterFLId commute) (effect to_suspend :> unrecorded) of
    unrecorded' :> to_suspend_after_unrecorded :> to_revert -> do
      effect_to_suspend <-
        case to_revert of
          NilFL -> return to_suspend_after_unrecorded
          _ ->
            if isInteractive True opts then do
              putStrLn $
                "These unrecorded changes conflict with the " ++ cmdname ++ ":"
              printFriendly O.Verbose O.NoSummary to_revert
              yes <- promptYorn "Do you want to revert these changes?"
              if yes then
                return $ to_suspend_after_unrecorded +>+ to_revert
              else do
                putStrLn $ "Okay, " ++ cmdname ++ " cancelled."
                exitSuccess
            else
              fail $
                "Can't suspend these patches without reverting some unrecorded changes."
      _repository <-
        tentativelyRemovePatches _repository NoUpdatePending to_suspend
      -- rely on sifting to commute out prims not belonging in pending:
      setTentativePending _repository unrecorded'
      new_suspended <-
        addToEditsToSuspended (O.diffAlgorithm ? opts)
          (mapFL_FL hopefully to_suspend) suspended
      writeTentativeRebase _repository new_suspended
      return (_repository, Sealed (invert effect_to_suspend))

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
  withRepoLock (useCache ? opts) (umask ? opts) $ RepoJob $ \_repository -> do
    checkHasRebase _repository
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
    debugMessage "Working out conflict markup..."
    Sealed resolution <-
      if have_conflicts then
        case O.conflictsYes ? opts of
          Just (YesAllowConflicts (ExternalMerge _)) ->
            error $ "external resolution for "++cmd++" not implemented yet"
          Just (YesAllowConflicts NoResolveConflicts) -> return $ seal NilFL
          Just (YesAllowConflicts MarkConflicts) -> return $ mangled conflicts
          Just NoAllowConflicts -> error "impossible" -- was handled in announceConflicts
          Nothing -> error "impossible"
      else return $ seal NilFL

    unrec <- unrecordedChanges (diffingOpts opts) _repository Nothing

    -- TODO should catch logfiles (fst value from updatePatchHeader) and
    -- clean them up as in AmendRecord
    -- Note: we can allow hijack attempts here without warning the user
    -- because we already asked about that on suspend time
    (unsuspended_ps, ps_to_keep') <-
      runHijackT IgnoreHijack $ handleUnsuspend ps_to_unsuspend (unseal Items ps_to_keep)
    _repository <-
      tentativelyAddPatches _repository NoUpdatePending unsuspended_ps
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
              _repository <- finalizeRepositoryChanges _repository (O.dryRun ? opts)
              unless (O.yes (O.dryRun ? opts)) $
                void $ applyToWorking _repository (verbosity ? opts) effect_to_apply

    where da = diffAlgorithm ? opts

          handleUnsuspend
                 :: forall p wR wT. (RepoPatch p, ApplyState p ~ Tree)
                 => FL (WDDNamed p) wR wT
                 -> Suspended p wT
                 -> HijackT IO (FL (PatchInfoAnd p) wR wT, Suspended p wT)
          handleUnsuspend NilFL to_keep = return (NilFL, to_keep)
          handleUnsuspend (p :>: ps) to_keep = do
              case wddDependedOn p of
                  [] -> return ()
                  deps -> liftIO $ do
                      -- It might make sense to only print out this message
                      -- once, but we might find that the dropped dependencies
                      -- are interspersed with other output, e.g. if running
                      -- with --ask-deps
                      let indent n = prefix (replicate n ' ')
                      putDocLnWith fancyPrinters $
                        redText ("Dropping the following explicit " ++
                          englishNum (length deps) (Noun "dependency") ":") $$
                        displayPatchInfo (patch2patchinfo (wddPatch p)) $$
                        indent 1 (redText "depended on:") $$
                        indent 2 (vcat (map displayPatchInfo deps))

              -- TODO should catch logfiles (fst value from updatePatchHeader)
              -- and clean them up as in AmendRecord
              -- TODO should also ask user to supply explicit dependencies as
              -- replacements for those that have been lost (if any, see above)
              p' <- snd <$> updatePatchHeader @p cmd
                      NoAskAboutDeps
                      (patchSelOpts True opts)
                      (patchHeaderConfig opts)
                      (fmapFL_Named effect (wddPatch p)) NilFL
              -- Create a rename that undoes the change we just made, so that the
              -- context of patch names match up in the following sequence. We don't
              -- track patch names properly in witnesses yet and so the rename appears
              -- to have a null effect on the context.
              --   p' :: WDDNamed p wR wR2
              --   rename :: RebaseName wR2 wR2
              --   ps :: FL (WDDNamed p) wR2 wT
              let rename :: RebaseName wR2 wR2
                  rename = Rename (info p') (patch2patchinfo (wddPatch p))
              -- push it through the remaining patches to fix them up, which should leave
              -- us with
              --   p' :: WDDNamed p wR wR2
              --   ps2 :: FL (WDDNamed p) wR2 wT2
              --   rename2 :: RebaseName wT2 wT2
              Just (ps2 :> (rename2 :: RebaseName wT2 wT2')) <-
                return $ commuterIdFL (commuterIdWDD commuteNameNamed) (rename :> ps)
              -- However the commute operation loses the information that the rename2 has
              -- a null effect on the context so we have to assert it manually.
              IsEq <- return (unsafeCoerceP IsEq :: EqCheck wT2 wT2')
              to_keep' <- return $ S.simplifyPush da (NameFixup rename2) to_keep
              (converted, to_keep'') <- handleUnsuspend ps2 to_keep'
              return (p' :>: converted, to_keep'')

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
  withRepoLock (useCache ? opts) (umask ? opts) $ RepoJob $
    \(_repository :: Repository 'RW p wU wR) -> do
    checkHasRebase _repository
    Items selects <- readTentativeRebase _repository

    -- TODO this selection doesn't need to respect dependencies
    -- TODO we only want to select one patch: generalise withSelectedPatchFromList
    let selection_config =
          selectionConfigGeneric rcToPia First "inject into" (patchSelOpts True opts) Nothing
    (to_inject :> keep) <- runSelection selects selection_config

    let extractSingle :: FL (RebaseChange prim) wX wY -> RebaseChange prim wX wY
        extractSingle (rc :>: NilFL) = rc
        extractSingle _ = error "You must select precisely one patch!"

    rc <- return $ extractSingle to_inject
    Sealed new <- injectOne opts rc keep

    writeTentativeRebase _repository $ Items new
    void $ finalizeRepositoryChanges _repository (O.dryRun ? opts)

-- | Inject fixups into a 'RebaseChange' and update the remainder of the rebase
-- state. This is in 'IO' because it involves interactive selection of the
-- fixups to inject.
-- TODO: We currently offer only prim fixups, not name fixups, for injection. I
-- think it would make sense to extend this to name fixups, so the user can
-- explicitly resolve a lost dependency in cases where is clear that it won't
-- re-appear.
injectOne
  :: (PrimPatch prim, ApplyState prim ~ Tree)
  => [DarcsFlag]
  -> RebaseChange prim wX wY
  -> FL (RebaseChange prim) wY wZ
  -> IO (Sealed (FL (RebaseChange prim) wX))
injectOne opts (RC fixups toedit) rest_suspended = do
  name_fixups :> prim_fixups <- return $ flToNamesPrims fixups
  let prim_selection_config =
        selectionConfigPrim
          Last
          "inject"
          (patchSelOpts True opts)
          (Just (primSplitter (diffAlgorithm ? opts)))
          Nothing
  (rest_fixups :> injects) <-
    runInvertibleSelection prim_fixups prim_selection_config
  let da = diffAlgorithm ? opts
      toeditNew = fmapFL_Named (canonizeFL da . (injects +>+)) toedit
  return $
    unseal (simplifyPushes da (mapFL_FL NameFixup name_fixups)) $
    simplifyPushes da (mapFL_FL PrimFixup rest_fixups) $
    RC NilFL toeditNew :>: rest_suspended


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
  withRepoLock (useCache ? opts) (umask ? opts) $ RepoJob $ \_repository -> do
    checkHasRebase _repository
    Items selects <- readTentativeRebase _repository

    -- TODO this selection doesn't need to respect dependencies
    let selection_config = selectionConfigGeneric rcToPia First "obliterate" (obliteratePatchSelOpts opts) Nothing
    (chosen :> keep) <- runSelection selects selection_config
    when (nullFL chosen) $ do putStrLn "No patches selected!"
                              exitSuccess

    let ps_to_keep =
          foldSealedFL (obliterateOne (diffAlgorithm ? opts)) chosen (Sealed keep)
    writeTentativeRebase _repository (unseal Items ps_to_keep)

    void $ finalizeRepositoryChanges _repository (O.dryRun ? opts)

-- TODO: move to Darcs.Patch.Witnesses.Ordered ?
-- | Map a cons-like operation that may change the end state over an 'FL'.
-- Unfortunately this can't be generalized to 'foldrwFL', even though it has
-- exactly the same definition, because 'Sealed' doesn't have the right kind.
-- We could play with a newtype wrapper to fix this but the ensuing wrapping
-- and unwrapping would hardly make it clearer what's going on.
foldSealedFL
  :: (forall wA wB . p wA wB -> Sealed (q wB) -> Sealed (q wA))
  -> FL p wX wY -> Sealed (q wY) -> Sealed (q wX)
-- kind error: foldSealedFL = foldrwFL
foldSealedFL _ NilFL acc = acc
foldSealedFL f (p :>: ps) acc = f p (foldSealedFL f ps acc)

obliterateOne
  :: PrimPatch prim
  => O.DiffAlgorithm
  -> RebaseChange prim wX wY
  -> Sealed (FL (RebaseChange prim) wY)
  -> Sealed (FL (RebaseChange prim) wX)
obliterateOne da rc = unseal (simplifyPushes da (rcToFixups rc))

rcToFixups :: RebaseChange prim wX wY -> FL (RebaseFixup prim) wX wY
rcToFixups (RC fs e) = fs +>+ primNamedToFixups e

forceCommute
  :: PrimPatch prim
  => O.DiffAlgorithm
  -> RebaseChange prim wX wY
  -> RebaseChange prim wY wZ
  -> Sealed (FL (RebaseChange prim) wZ)
  -> Maybe (Sealed (FL (RebaseChange prim) wX))
forceCommute da rc1 rc2 (Sealed rcs) =
  do
    rc2' :> rc1' <- commute (rc1 :> rc2)
    return $ Sealed (rc2' :>: rc1' :>: rcs)
  `mplus`
  do
    RC fs2' e2' :> RC fs1' e1' <- forceCommuteRebaseChange (rc1 :> rc2)
    return $
      unseal (simplifyPushes da fs2') $
      mapSeal (RC NilFL e2' :>:) $
      unseal (simplifyPushes da fs1') $
      mapSeal (RC NilFL e1' :>:) $
      Sealed rcs

edit :: DarcsCommand
edit = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "edit"
    , commandHelp = text description
    , commandDescription = description
    , commandPrereq = amInHashedRepository
    , commandExtraArgs = 0
    , commandExtraArgHelp = []
    , commandCommand = editCmd
    , commandCompleteArgs = noArgs
    , commandArgdefaults = nodefaults
    , commandOptions = opts
    }
  where
    basicOpts = O.diffAlgorithm ^ O.withSummary
    opts = basicOpts `withStdOpts` O.umask
    description = "Edit suspended patches."

data EditState prim wX = EditState
  { count :: Int
  , index :: Int
  , patches :: Sealed ((RL (RebaseChange prim) :> FL (RebaseChange prim)) wX)
  }

data Edit prim wX = Edit
  { eWhat :: String
  , eState :: EditState prim wX
  }

editCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
editCmd _ opts _args =
  withRepoLock (useCache ? opts) (umask ? opts) $ RepoJob $ \_repository -> do
    checkHasRebase _repository
    Items items <- readTentativeRebase _repository
    let initial_state =
          EditState
            { count = lengthFL items
            , index = 0
            , patches = Sealed (NilRL :> items)
            }
    Sealed items' <- interactiveEdit opts [] initial_state []
    writeTentativeRebase _repository (Items items')
    void $ finalizeRepositoryChanges _repository (O.dryRun ? opts)

interactiveEdit
  :: (PrimPatch prim, ApplyState prim ~ Tree)
  => [DarcsFlag]
  -> [Edit prim wR]     -- ^ stack of undone edits, for redo
  -> EditState prim wR  -- ^ current state
  -> [Edit prim wR]     -- ^ stack of past edits, for undo
  -> IO (Sealed (FL (RebaseChange prim) wR))
interactiveEdit opts redos s@EditState{..} undos =
  -- invariants:
  --  * the "todo" patches are empty only if the "done" patches are; formally:
  --      case patches of Sealed (done :> todo) -> nullFL todo ==> nullRL done
  case patches of
    Sealed (_ :> NilFL) -> prompt
    Sealed (_ :> p :>: _) -> defaultPrintFriendly p >> prompt
  where
    da = diffAlgorithm ? opts

    -- helper functions
    defaultPrintFriendly =
      liftIO . printFriendly (O.verbosity ? opts) (O.withSummary ? opts)

    -- common actions
    undo =
      case undos of
        [] -> error "impossible"
        e : undos' ->
          -- pop last state from undos, push current state onto redos
          interactiveEdit opts (Edit (eWhat e) s : redos) (eState e) undos'
    redo =
      case redos of
        [] -> error "impossible"
        e : redos' ->
          -- pop last state from redos, push current state onto undos
          interactiveEdit opts redos' (eState e) (Edit (eWhat e) s : undos)
    quit = do
      putInfo opts $ text "Okay, rebase edit cancelled."
      exitSuccess
    commit =
      case patches of
        Sealed (done :> todo) -> return $ Sealed (done +>>+ todo)
    list = mapM_ (putStrLn . eWhat) (reverse undos) >> prompt
    choicesCommon =
      [ PromptChoice 'q' True quit "quit, discard all edits"
      , PromptChoice 'd' True commit "done editing, commit"
      , PromptChoice 'l' True list "list edits made so far"
      , PromptChoice 'u' (not (null undos)) undo "undo previous edit"
      , PromptChoice 'r' (not (null redos)) redo "redo previously undone edit"
      ]

    prompt =
      case patches of
        Sealed (_ :> NilFL) -> -- empty rebase state
          runPrompt PromptConfig
            { pPrompt = "No more suspended patches. What shall I do?"
            , pVerb = "rebase edit"
            , pChoices = [choicesCommon]
            , pDefault = Nothing
            }
        Sealed (done :> todo@(p :>: todo')) -> -- non-empty rebase state
          runPrompt PromptConfig
            { pPrompt = "What shall I do with this patch? " ++
                  "(" ++ show (index + 1) ++ "/" ++ show count ++ ")"
            , pVerb = "rebase edit"
            , pChoices = [choicesEdit, choicesCommon, choicesView, choicesNav]
            , pDefault = Nothing
            }
          where

            choicesEdit =
              [ PromptChoice 'o' True dropit "drop (obliterate, dissolve into fixups)"
              , PromptChoice 'e' True reword "edit name and/or long comment (log)"
              , PromptChoice 's' (index > 0) squash "squash with previous patch"
              , PromptChoice 'i' can_inject inject' "inject fixups"
              , PromptChoice 'c' (index > 0) comm "(force-)commute with previous patch)"
              -- TODO
              -- , PromptChoice '???' True ??? "select individual changes for editing"
              ]
            choicesView =
              [ PromptChoice 'v' True view "view this patch in full"
              , PromptChoice 'p' True pager "view this patch in full with pager"
              , PromptChoice 'y' True display "view this patch"
              , PromptChoice 'x' can_summarize summary
                "view a summary of this patch"
              ]
            choicesNav =
              [ PromptChoice 'n' (index + 1 < count) next "skip to next patch"
              , PromptChoice 'k' (index > 0) prev "back up to previous patch"
              , PromptChoice 'g' (index > 0) first "start over from the first patch"
              ]

            -- helper functions
            edit' op s' = do
              let what =
                    case p of RC _ np -> op ++ " " ++ piName (patch2patchinfo np)
              -- set new state s' and push the current one onto the undo stack
              -- discarding the redo stack
              interactiveEdit opts [] s' (Edit what s : undos)
            navigate s' =
              -- set new state s' with no undo or redo stack modification
              interactiveEdit opts redos s' undos

            can_summarize = not (O.yes (O.withSummary ? opts))
            can_inject = case p of (RC NilFL _) -> False; _ -> True

            -- editing
            dropit = do
              Sealed todo'' <- return $ obliterateOne da p (Sealed todo')
              edit' "drop  " s { count = count - 1 , patches = Sealed (done :> todo'') }
            inject' = do
              result <- try $ injectOne opts p todo'
              case result of
                Left ExitSuccess -> prompt
                Left e -> throwIO e
                Right (Sealed todo'') ->
                  edit' "inject" s { patches = Sealed (done :> todo'') }
            reword = do
              Sealed todo'' <- rewordOne da p todo'
              edit' "reword" s { patches = Sealed (done :> todo'') }
            comm = do
              case done of
                NilRL -> error "impossible"
                done' :<: q ->
                  case forceCommute da q p (Sealed todo') of
                    Just (Sealed todo'') ->
                      edit' "commute" s
                        { patches = Sealed (done' :> todo'')
                        , index = index - 1
                        }
                    Nothing -> do
                      putStrLn "Failed to commute fixups backward, try inject first."
                      prompt
            squash =
              case done of
                NilRL -> error "impossible"
                done' :<: q ->
                  case squashOne da q p todo' of
                    Just (Sealed todo'') ->
                      -- this moves back by one so the new squashed patch is
                      -- selected; useful in case you now want to edit the
                      -- comment or look at the result
                      edit' "squash" s
                        { count = count - 1
                        , index = index - 1
                        , patches = Sealed (done' :> todo'')
                        }
                    Nothing -> do
                      putStrLn "Failed to commute fixups backward, try inject first."
                      prompt

            -- viewing
            view = printContent p >> prompt
            pager = printContentWithPager p >> prompt
            display = defaultPrintFriendly p >> prompt
            summary = printSummary p >> prompt

            -- navigation
            next =
              case todo' of
                NilFL -> error "impossible"
                _ ->
                  navigate
                    s { index = index + 1, patches = Sealed (done :<: p :> todo') }
            prev =
              case done of
                NilRL -> error "impossible"
                done' :<: p' ->
                  navigate
                    s { index = index - 1, patches = Sealed (done' :> p' :>: todo) }
            first =
              navigate s { index = 0, patches = Sealed (NilRL :> done +>>+ todo) }

-- | Squash second patch with first, updating the rest of the rebase state.
-- This can fail if the second patch has fixups that don't commute with the
-- contents of the first patch.
squashOne
  :: PrimPatch prim
  => O.DiffAlgorithm
  -> RebaseChange prim wX wY
  -> RebaseChange prim wY wZ
  -> FL (RebaseChange prim) wZ wW
  -> Maybe (Sealed (FL (RebaseChange prim) wX))
squashOne da (RC fs1 e1) (RC fs2 e2) rest = do
  fs2' :> e1' <- commuterIdFL commuteNamedFixup (e1 :> fs2)
  let e1'' = fmapFL_Named (canonizeFL da . (+>+ patchcontents e2)) e1'
      e2_name_fixup = NameFixup (AddName (patch2patchinfo e2))
  return $
    case simplifyPush da e2_name_fixup rest of
      Sealed rest' -> simplifyPushes da (fs1 +>+ fs2') (RC NilFL e1'' :>: rest')

rewordOne
  :: (PrimPatch prim, ApplyState prim ~ Tree)
  => O.DiffAlgorithm
  -> RebaseChange prim wX wY
  -> FL (RebaseChange prim) wY wZ
  -> IO (Sealed (FL (RebaseChange prim) wX))
rewordOne da (RC fs e) rest = do
  e' <- editLog e
  let rename = NameFixup $ Rename (patch2patchinfo e') (patch2patchinfo e)
  case simplifyPush da rename rest of
    Sealed rest' -> return $ Sealed $ RC fs e' :>: rest'

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
    , commandCompleteArgs = prefArgs Repos
    , commandArgdefaults = defaultRepo
    , commandOptions = pullOpts
    }
  where
    pullBasicOpts
      = O.matchSeveral
      ^ O.reorder
      ^ O.interactive
      ^ O.conflictsYes
      ^ O.testChanges
      ^ O.dryRunXml
      ^ O.withSummary
      ^ O.selectDeps
      ^ O.repoDir
      ^ O.allowUnrelatedRepos
      ^ O.diffAlgorithm
    pullAdvancedOpts
      = O.repoCombinator
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
      = O.setScriptsExecutable
      ^ O.umask
      ^ O.changesReverse
      ^ O.pauseForGui
    applyOpts = applyBasicOpts `withStdOpts` applyAdvancedOpts
    applyDescription =
      "Apply a patch bundle, suspending any local patches that conflict."

data RebasePatchApplier = RebasePatchApplier

instance PatchApplier RebasePatchApplier where
    repoJob RebasePatchApplier f = RepoJob (f PatchProxy)
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

    (_repository, Sealed toWorking) <-
      doSuspend cmdName opts _repository suspended usToSuspend
    -- the new rebase patch containing the suspended patches is now in the repo
    -- and the suspended patches have been removed

    -- We must apply the suspend to working because tentativelyMergePatches
    -- calls unrecordedChanges. We also have to update the index, since that is
    -- used to filter the working tree (unless --ignore-times is in effect).
    updateIndex _repository
    _repository <- withSignalsBlocked $ do
        applyToWorking _repository (verbosity ? opts) toWorking

    Sealed pw <-
        tentativelyMergePatches
            _repository cmdName
            (allowConflicts opts)
            (wantGuiPause opts)
            (reorder ? opts) (diffingOpts opts)
            (Fork common (usOk +>+ usKeep) to_be_applied)

    applyPatchesFinish cmdName opts _repository pw (nullFL to_be_applied)

applyPatchSelOpts :: S.PatchSelectionOptions
applyPatchSelOpts = S.PatchSelectionOptions
    { S.verbosity = O.NormalVerbosity
    , S.matchFlags = []
    , S.interactive = True
    , S.selectDeps = O.PromptDeps -- option not supported, use default
    , S.withSummary = O.NoSummary
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
    withRepository (useCache ? opts) $ RepoJob $ \_repository -> do
        checkHasRebase _repository
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
  withRepoLock (useCache ? opts) (umask ? opts) $ OldRebaseJob $ \repo ->
    upgradeOldStyleRebase repo

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
