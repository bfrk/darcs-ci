module Darcs.Repository.Clone
    ( cloneRepository
    ) where

import Darcs.Prelude

import Control.Exception ( catch, SomeException )
import Control.Monad ( forM, unless, void, when )
import qualified Data.ByteString.Char8 as BC
import Data.List( intercalate )
import Data.Maybe( catMaybes )
import Safe ( tailErr )
import System.FilePath.Posix ( (</>) )
import System.Directory
    ( removeFile
    , listDirectory
    )

import Darcs.Repository.Create
    ( EmptyRepository(..)
    , createRepository
    )
import Darcs.Repository.Identify ( identifyRepositoryFor, ReadingOrWriting(..) )
import Darcs.Repository.Pristine
    ( applyToTentativePristine
    , createPristineDirectoryTree
    )
import Darcs.Repository.Hashed
    ( copyHashedInventory
    , readPatches
    , tentativelyRemovePatches
    , writeTentativeInventory
    )
import Darcs.Repository.Transaction
    ( finalizeRepositoryChanges
    , revertRepositoryChanges
    )
import Darcs.Repository.Working
    ( setAllScriptsExecutable
    , setScriptsExecutablePatches )
import Darcs.Repository.InternalTypes
    ( Repository
    , AccessType(..)
    , repoLocation
    , repoFormat
    , repoCache
    , modifyCache
    )
import Darcs.Repository.Job ( withUMaskFlag )
import Darcs.Util.Cache
    ( filterRemoteCaches
    , fetchFileUsingCache
    , speculateFileUsingCache
    , dropNonRepos
    )

import Darcs.Repository.ApplyPatches ( runDefault )
import Darcs.Repository.Inventory
    ( PatchHash
    , encodeValidHash
    , peekPristineHash
    )
import Darcs.Repository.Format
    ( RepoProperty ( HashedInventory, Darcs2, Darcs3 )
    , RepoFormat
    , formatHas
    )
import Darcs.Repository.Prefs ( addRepoSource, deleteSources )
import Darcs.Util.File
    ( copyFileOrUrl
    , Cachable(..)
    , gzFetchFilePS
    )
import Darcs.Repository.PatchIndex
    ( doesPatchIndexExist
    , createPIWithInterrupt
    )
import Darcs.Repository.Packs
    ( fetchAndUnpackBasic
    , fetchAndUnpackPatches
    , packsDir
    )
import Darcs.Repository.Paths ( hashedInventoryPath, pristineDirPath )
import Darcs.Repository.Resolution
    ( StandardResolution(..)
    , patchsetConflictResolutions
    , announceConflicts
    )
import Darcs.Repository.Working ( applyToWorking )
import Darcs.Util.Lock ( writeTextFile, withNewDirectory )
import Darcs.Repository.Flags
    ( UpdatePending(..)
    , UseCache(..)
    , RemoteDarcs (..)
    , remoteDarcs
    , CloneKind (..)
    , Verbosity (..)
    , DryRun (..)
    , UMask (..)
    , SetScriptsExecutable (..)
    , SetDefault (..)
    , InheritDefault (..)
    , WithWorkingDir (..)
    , ForgetParent (..)
    , WithPatchIndex (..)
    , PatchFormat (..)
    , AllowConflicts(..)
    , ResolveConflicts(..)
    , WithPrefsTemplates(..)
    )

import Darcs.Patch ( RepoPatch, description )
import Darcs.Patch.Depends ( findUncommon )
import Darcs.Patch.Invertible ( mkInvertible )
import Darcs.Patch.Set
    ( Origin
    , patchSet2FL
    , patchSet2RL
    , patchSetInventoryHashes
    , progressPatchSet
    )
import Darcs.Patch.Match ( MatchFlag(..), patchSetMatch, matchOnePatchset )
import Darcs.Patch.Progress ( progressRLShowTags, progressFL )
import Darcs.Patch.Apply ( Apply(..) )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..) )
import Darcs.Patch.Witnesses.Ordered
    ( (:\/:)(..)
    , FL(..)
    , RL(..)
    , lengthFL
    , mapRL
    , lengthRL
    , nullFL
    )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, extractHash )

import Darcs.Util.Tree( Tree )

import Darcs.Util.Exception ( catchall )
import Darcs.Util.English ( englishNum, Noun(..) )
import Darcs.Util.Global ( darcsdir )
import Darcs.Util.URL ( isValidLocalPath )
import Darcs.Util.SignalHandler ( catchInterrupt, withSignalsBlocked )
import Darcs.Util.Ssh ( resetSshConnections )
import Darcs.Util.Printer ( Doc, ($$), hsep, putDocLn, text )
import Darcs.Util.Printer.Color ( unsafeRenderStringColored )
import Darcs.Util.Progress
    ( debugMessage
    , tediousSize
    , beginTedious
    , endTedious
    )

joinUrl :: [String] -> String
joinUrl = intercalate "/"

cloneRepository ::
    String    -- origin repository path
    -> String -- new repository name (for relative path)
    -> Verbosity -> UseCache
    -> CloneKind
    -> UMask -> RemoteDarcs
    -> SetScriptsExecutable
    -> SetDefault
    -> InheritDefault
    -> [MatchFlag]
    -> RepoFormat
    -> WithWorkingDir
    -> WithPatchIndex   -- use patch index
    -> Bool   -- use packs
    -> ForgetParent
    -> WithPrefsTemplates
    -> IO ()
cloneRepository repourl mysimplename v useCache cloneKind um rdarcs sse
                setDefault inheritDefault matchFlags rfsource withWorkingDir
                usePatchIndex usePacks forget withPrefsTemplates =
  withUMaskFlag um $ withNewDirectory mysimplename $ do
      let patchfmt
            | formatHas Darcs3 rfsource = PatchFormat3
            | formatHas Darcs2 rfsource = PatchFormat2
            | otherwise                 = PatchFormat1
      EmptyRepository _toRepo <-
        createRepository patchfmt withWorkingDir
          (if cloneKind == LazyClone then NoPatchIndex else usePatchIndex)
          useCache withPrefsTemplates
      debugMessage "Finished initializing new repository."
      addRepoSource repourl NoDryRun setDefault inheritDefault False

      debugMessage "Identifying remote repository..."
      fromRepo <- identifyRepositoryFor Reading _toRepo useCache repourl
      let fromLoc = repoLocation fromRepo

      debugMessage "Copying prefs..."
      copyFileOrUrl (remoteDarcs rdarcs)
        (joinUrl [fromLoc, darcsdir, "prefs", "prefs"])
        (darcsdir </> "prefs/prefs") (MaxAge 600) `catchall` return ()

      debugMessage "Filtering remote sources..."
      cache <- filterRemoteCaches (repoCache fromRepo)
      _toRepo <- return $ modifyCache (const cache) _toRepo
      writeTextFile
        (darcsdir </> "prefs/sources")
        (unlines [show $ dropNonRepos cache])
      debugMessage $ "Considering sources:\n"++show (repoCache _toRepo)

      if formatHas HashedInventory (repoFormat fromRepo) then do
       debugMessage "Copying basic repository (hashed_inventory and pristine)"
       if usePacks && (not . isValidLocalPath) fromLoc
         then copyBasicRepoPacked    fromRepo _toRepo v rdarcs withWorkingDir
         else copyBasicRepoNotPacked fromRepo _toRepo v rdarcs withWorkingDir
       when (cloneKind /= LazyClone) $ do
         when (cloneKind /= CompleteClone) $
           putInfo v $ text "Copying patches, to get lazy repository hit ctrl-C..."
         debugMessage "Copying complete repository (inventories and patches)"
         if usePacks && (not . isValidLocalPath) fromLoc
           then copyCompleteRepoPacked    fromRepo _toRepo v cloneKind
           else copyCompleteRepoNotPacked fromRepo _toRepo v cloneKind
      else
       -- old-fashioned repositories are cloned differently since
       -- we need to copy all patches first and then build pristine
       copyRepoOldFashioned fromRepo _toRepo v withWorkingDir
      when (sse == YesSetScriptsExecutable) setAllScriptsExecutable
      case patchSetMatch matchFlags of
       Nothing -> return ()
       Just psm -> do
        putInfo v $ text "Going to specified version..."
        -- the following is necessary to be able to read _toRepo's patches
        _toRepo <- revertRepositoryChanges _toRepo
        patches <- readPatches _toRepo
        Sealed context <- matchOnePatchset patches psm
        to_remove :\/: only_in_context <- return $ findUncommon patches context
        case only_in_context of
          NilFL -> do
            let num_to_remove = lengthFL to_remove
            putInfo v $ hsep $ map text
              [ "Unapplying"
              , show num_to_remove
              , englishNum num_to_remove (Noun "patch") ""
              ]
            _toRepo <-
              tentativelyRemovePatches _toRepo NoUpdatePending to_remove
            _toRepo <- finalizeRepositoryChanges _toRepo NoDryRun
            runDefault (unapply to_remove) `catch` \(e :: SomeException) ->
                fail ("Couldn't undo patch in working tree.\n" ++ show e)
            when (sse == YesSetScriptsExecutable) $ setScriptsExecutablePatches to_remove
          _ ->
            -- This can only happen if the user supplied a context file that
            -- doesn't specify a subset of the remote repo.
            fail $ unsafeRenderStringColored
              $ text "Missing patches from context:"
              $$ description only_in_context
      when (forget == YesForgetParent) deleteSources
      -- TODO Checking for unresolved conflicts means we have to download
      -- at least all the patches referenced by hashed_inventory, even if
      -- --lazy is in effect. This can take a long time, and in extreme
      -- cases can even result in --lazy being slower than --complete.
      putVerbose v $ text "Checking for unresolved conflicts..."
      patches <- readPatches _toRepo
      let conflicts = patchsetConflictResolutions patches
      _ <- announceConflicts "clone" (YesAllowConflicts MarkConflicts) conflicts
      Sealed mangled_res <- return $ mangled conflicts
      unless (nullFL mangled_res) $
        withSignalsBlocked $ void $ applyToWorking _toRepo v mangled_res

putInfo :: Verbosity -> Doc -> IO ()
putInfo Quiet _ = return ()
putInfo _ d = putDocLn d

putVerbose :: Verbosity -> Doc -> IO ()
putVerbose Verbose d = putDocLn d
putVerbose _ _ = return ()

copyBasicRepoNotPacked  :: forall p wU wR.
                           Repository 'RO p wU wR -- remote
                        -> Repository 'RO p wU wR -- existing empty local
                        -> Verbosity
                        -> RemoteDarcs
                        -> WithWorkingDir
                        -> IO ()
copyBasicRepoNotPacked fromRepo toRepo verb rdarcs withWorkingDir = do
  putVerbose verb $ text "Copying hashed inventory from remote repo..."
  copyHashedInventory toRepo rdarcs (repoLocation fromRepo)
  putVerbose verb $ text "Writing pristine and working tree contents..."
  createPristineDirectoryTree toRepo "." withWorkingDir

copyCompleteRepoNotPacked :: forall rt p wU wR. (RepoPatch p, ApplyState p ~ Tree)
                        => Repository 'RO p wU wR -- remote
                        -> Repository rt p wU wR -- existing basic local
                        -> Verbosity
                        -> CloneKind
                        -> IO ()
copyCompleteRepoNotPacked _ toRepo verb cloneKind = do
       let cleanup = putInfo verb $ text "Using lazy repository."
       allowCtrlC cloneKind cleanup $ do
         fetchPatchesIfNecessary toRepo
         pi <- doesPatchIndexExist (repoLocation toRepo)
         ps <- readPatches toRepo
         when pi $ createPIWithInterrupt toRepo ps

copyBasicRepoPacked ::
  forall p wU wR.
     Repository 'RO p wU wR -- remote
  -> Repository 'RO p wU wR -- existing empty local repository
  -> Verbosity
  -> RemoteDarcs
  -> WithWorkingDir
  -> IO ()
copyBasicRepoPacked fromRepo toRepo verb rdarcs withWorkingDir =
  do let fromLoc = repoLocation fromRepo
     let hashURL = joinUrl [fromLoc, darcsdir, packsDir, "pristine"]
     mPackHash <- (Just <$> gzFetchFilePS hashURL Uncachable) `catchall` (return Nothing)
     let hiURL = fromLoc </> hashedInventoryPath
     i <- gzFetchFilePS hiURL Uncachable
     let currentHash = BC.pack $ encodeValidHash $ peekPristineHash i
     let copyNormally = copyBasicRepoNotPacked fromRepo toRepo verb rdarcs withWorkingDir
     case mPackHash of
      Just packHash | packHash == currentHash
              -> ( do copyBasicRepoPacked2 fromRepo toRepo verb withWorkingDir
                      -- need to obtain a fresh copy of hashed_inventory as reference
                      putVerbose verb $ text "Copying hashed inventory from remote repo..."
                      copyHashedInventory toRepo rdarcs (repoLocation fromRepo)
                   `catch` \(e :: SomeException) ->
                               do putStrLn ("Exception while getting basic pack:\n" ++ show e)
                                  copyNormally)
      _       -> do putVerbose verb $
                      text "Remote repo has no basic pack or outdated basic pack, copying normally."
                    copyNormally

copyBasicRepoPacked2 ::
  forall rt p wU wR.
     Repository 'RO p wU wR -- remote
  -> Repository rt p wU wR -- existing empty local repository
  -> Verbosity
  -> WithWorkingDir
  -> IO ()
copyBasicRepoPacked2 fromRepo toRepo verb withWorkingDir = do
  putVerbose verb $ text "Cloning packed basic repository."
  -- unpack inventory & pristine cache
  cleanDir pristineDirPath
  removeFile hashedInventoryPath
  fetchAndUnpackBasic (repoCache toRepo) (repoLocation fromRepo)
  putInfo verb $ text "Done fetching and unpacking basic pack."
  createPristineDirectoryTree toRepo "." withWorkingDir

copyCompleteRepoPacked ::
  forall rt p wU wR. (RepoPatch p, ApplyState p ~ Tree)
  => Repository 'RO p wU wR -- remote
  -> Repository rt p wU wR -- existing basic local repository
  -> Verbosity
  -> CloneKind
  -> IO ()
copyCompleteRepoPacked from to verb cloneKind =
    copyCompleteRepoPacked2 from to verb cloneKind
  `catch`
    \(e :: SomeException) -> do
      putStrLn ("Exception while getting patches pack:\n" ++ show e)
      putVerbose verb $ text "Problem while copying patches pack, copying normally."
      copyCompleteRepoNotPacked from to verb cloneKind

copyCompleteRepoPacked2 ::
  forall rt p wU wR. (RepoPatch p, ApplyState p ~ Tree)
  => Repository 'RO p wU wR
  -> Repository rt p wU wR
  -> Verbosity
  -> CloneKind
  -> IO ()
copyCompleteRepoPacked2 fromRepo toRepo verb cloneKind = do
  us <- readPatches toRepo
  -- get old patches
  let cleanup = putInfo verb $ text "Using lazy repository."
  allowCtrlC cloneKind cleanup $ do
    putVerbose verb $ text "Using patches pack."
    is <-
      forM (patchSetInventoryHashes us) $
        maybe (fail "unexpected unhashed inventory") return
    hs <-
      forM (mapRL hashedPatchHash $ patchSet2RL us) $
        maybe (fail "unexpected unhashed patch") return
    fetchAndUnpackPatches is hs (repoCache toRepo) (repoLocation fromRepo)
    pi <- doesPatchIndexExist (repoLocation toRepo)
    when pi $ createPIWithInterrupt toRepo us -- TODO or do another readPatches?

cleanDir :: FilePath -> IO ()
cleanDir d = mapM_ (\x -> removeFile $ d </> x) =<< listDirectory d

copyRepoOldFashioned :: forall p wU wR. (RepoPatch p, ApplyState p ~ Tree)
                        => Repository 'RO p wU wR  -- remote repo
                        -> Repository 'RO p Origin Origin -- local empty repo
                        -> Verbosity
                        -> WithWorkingDir
                        -> IO ()
copyRepoOldFashioned fromRepo _toRepo verb withWorkingDir = do
  _toRepo <- revertRepositoryChanges _toRepo
  patches <- readPatches fromRepo
  let k = "Copying patch"
  beginTedious k
  tediousSize k (lengthRL $ patchSet2RL patches)
  let patches' = progressPatchSet k patches
  writeTentativeInventory _toRepo patches'
  endTedious k
  local_patches <- readPatches _toRepo
  let patchesToApply = progressFL "Applying patch" $ patchSet2FL local_patches
  applyToTentativePristine _toRepo (mkInvertible patchesToApply)
  _toRepo <- finalizeRepositoryChanges _toRepo NoDryRun
  putVerbose verb $ text "Writing the working tree..."
  createPristineDirectoryTree _toRepo "." withWorkingDir

-- | This function fetches all patches that the given repository has
--   with fetchFileUsingCache.
fetchPatchesIfNecessary :: forall rt p wU wR. RepoPatch p
                        => Repository rt p wU wR
                        -> IO ()
fetchPatchesIfNecessary toRepo =
  do  ps <- readPatches toRepo
      let patches = patchSet2RL ps
          ppatches = progressRLShowTags "Copying patches" patches
          (first, other) = splitAt (100 - 1) $ tailErr $ hashes patches
          speculate = [] : first : map (:[]) other
      mapM_ fetchAndSpeculate $ zip (hashes ppatches) (speculate ++ repeat [])
  where hashes :: forall wX wY . RL (PatchInfoAnd p) wX wY -> [PatchHash]
        hashes = catMaybes . mapRL hashedPatchHash
        fetchAndSpeculate :: (PatchHash, [PatchHash]) -> IO ()
        fetchAndSpeculate (f, ss) = do
          _ <- fetchFileUsingCache c f
          mapM_ (speculateFileUsingCache c) ss
        c = repoCache toRepo

allowCtrlC :: CloneKind -> IO () -> IO () -> IO ()
allowCtrlC CompleteClone _ action = action
allowCtrlC _ cleanup action =
  action `catchInterrupt` do
    debugMessage "Cleanup after SIGINT in allowCtrlC"
    -- the SIGINT has also killed our running ssh connections,
    -- this will cause them to be restarted
    resetSshConnections
    cleanup

hashedPatchHash :: PatchInfoAnd p wA wB -> Maybe PatchHash
hashedPatchHash = either (const Nothing) Just . extractHash
