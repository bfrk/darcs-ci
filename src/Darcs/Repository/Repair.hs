module Darcs.Repository.Repair ( replayRepository, checkIndex,
                                 replayRepositoryInTemp,
                                 RepositoryConsistency(..) )
       where

import Darcs.Prelude

import Control.Monad ( when, unless )
import Control.Monad.Trans ( liftIO )
import Control.Exception ( catch, IOException )
import Data.List ( sort, (\\) )
import System.Directory
    ( createDirectoryIfMissing
    , getCurrentDirectory
    , setCurrentDirectory
    , withCurrentDirectory
    )

import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, hopefully, info )
import Darcs.Patch.Witnesses.Ordered
    ( FL(..)
    , lengthFL
    , mapFL
    , nullFL
    , reverseFL
    , reverseRL
    )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..), unFreeLeft, unseal )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Patch.Repair ( Repair(applyAndTryToFix) )
import Darcs.Patch.Info ( displayPatchInfo, makePatchname )
import Darcs.Patch.Set ( Origin, PatchSet(..), Tagged(..), patchSet2FL )
import Darcs.Patch ( RepoPatch, PrimOf, isInconsistent )

import Darcs.Repository.Diff( treeDiff )
import Darcs.Repository.Flags ( Verbosity(..), DiffAlgorithm )
import Darcs.Repository.Hashed ( readPatches, writeAndReadPatch )
import Darcs.Repository.InternalTypes ( Repository, repoCache, repoLocation )
import Darcs.Repository.Paths ( pristineDirPath )
import Darcs.Repository.Pending ( readPending )
import Darcs.Repository.Prefs ( filetypeFunction )
import Darcs.Repository.State
    ( readPristine
    , readIndex
    , readPristineAndPending
    )

import Darcs.Util.Cache ( Cache, mkDirCache )
import Darcs.Util.Progress
    ( beginTedious
    , endTedious
    , finishedOneIO
    , tediousSize
    )
import Darcs.Util.Lock( withDelayedDir )
import Darcs.Util.Path( anchorPath, toFilePath )
import Darcs.Util.Printer ( putDocLn, text, renderString, ($$) )
import Darcs.Util.Hash( showHash )
import Darcs.Util.Tree( Tree, emptyTree, list, restrict, expand, itemHash, zipTrees )
import Darcs.Util.Tree.Monad( TreeIO )
import Darcs.Util.Tree.Hashed( darcsUpdateHashes, hashedTreeIO )
import Darcs.Util.Tree.Plain( readPlainTree )
import Darcs.Util.Index( treeFromIndex )

applyAndFixPatchSet
  :: forall rt p wU wR. (RepoPatch p, ApplyState p ~ Tree)
  => Repository rt p wU wR
  -> PatchSet p Origin wR
  -> TreeIO (PatchSet p Origin wR, Bool)
applyAndFixPatchSet r s = do
    liftIO $ beginTedious k
    liftIO $ tediousSize k $ lengthFL $ patchSet2FL s
    result <- case s of
      PatchSet ts ps -> do
        (ts', ts_ok) <- applyAndFixTagged (reverseRL ts)
        (ps', ps_ok) <- applyAndFixPatches (reverseRL ps)
        return (PatchSet (reverseFL ts') (reverseFL ps'), ts_ok && ps_ok)
    liftIO $ endTedious k
    return result
  where
    k = "Replaying patch"
    applyAndFixTagged :: FL (Tagged p) wX wY -> TreeIO (FL (Tagged p) wX wY, Bool)
    applyAndFixTagged NilFL = return (NilFL, True)
    applyAndFixTagged (Tagged ps t _ :>: ts) = do
      (ps', ps_ok) <- applyAndFixPatches (reverseRL ps)
      (ts', ts_ok) <- applyAndFixTagged ts
      return (Tagged (reverseFL ps') t Nothing :>: ts', ps_ok && ts_ok)
    applyAndFixPatches
      :: FL (PatchInfoAnd p) wX wY -> TreeIO (FL (PatchInfoAnd p) wX wY, Bool)
    applyAndFixPatches NilFL = return (NilFL, True)
    applyAndFixPatches (p :>: ps) = do
      mp' <- applyAndTryToFix p
      case isInconsistent . hopefully $ p of
        Just err -> liftIO $ putDocLn err
        Nothing -> return ()
      liftIO $ finishedOneIO k $ show $ makePatchname $ info p
      (ps', ps_ok) <- applyAndFixPatches ps
      case mp' of
        Nothing -> return (p :>: ps', ps_ok)
        Just (e, p') ->
          liftIO $ do
            putStrLn $ renderString $ (displayPatchInfo $ info p) $$ text e
            -- FIXME While this is okay semantically, it means we can't
            -- run darcs check in a read-only repo
            p'' <-
              withCurrentDirectory (repoLocation r) $
              writeAndReadPatch (repoCache r) p'
            return (p'' :>: ps', False)

data RepositoryConsistency p wR = RepositoryConsistency
  { fixedPristine :: Maybe (Tree IO, Sealed (FL (PrimOf p) wR))
  , fixedPatches :: Maybe (PatchSet p Origin wR)
  , fixedPending :: Maybe (Sealed (FL (PrimOf p) wR))
  }

hasDuplicate :: Ord a => [a] -> Maybe a
hasDuplicate li = hd $ sort li
    where hd [_] = Nothing
          hd [] = Nothing
          hd (x1:x2:xs) | x1 == x2 = Just x1
                        | otherwise = hd (x2:xs)

replayRepository'
  :: forall rt p wR wU. (RepoPatch p, ApplyState p ~ Tree)
  => DiffAlgorithm
  -> Cache
  -> Repository rt p wU wR
  -> Verbosity
  -> IO (RepositoryConsistency p wR)
replayRepository' dflag cache repo verbosity = do
  let putVerbose s = when (verbosity == Verbose) $ putDocLn s
      putInfo s = unless (verbosity == Quiet) $ putDocLn s

  putVerbose $ text "Checking that patch names are unique..."
  patches <- readPatches repo
  case hasDuplicate $ mapFL info $ patchSet2FL patches of
    Nothing -> return ()
    Just pinf -> do
      putInfo $ text "Error! Duplicate patch name:"
      putInfo $ displayPatchInfo pinf
      -- FIXME repair duplicates by re-generating their salt
      fail "Duplicate patches found."

  -- we have to read pristine before fixing patches as that updates pristine
  pris <-
    (readPristine repo >>= expand >>= darcsUpdateHashes)
    `catch`
    \(_ :: IOException) -> return emptyTree

  putVerbose $ text "Checking content of recorded patches..."
  ((newpatches, patches_ok), newpris) <-
    hashedTreeIO (applyAndFixPatchSet repo patches) emptyTree cache

  putVerbose $ text "Checking pristine..."
  ftf <- filetypeFunction
  pristine_diff <- unFreeLeft `fmap` treeDiff dflag ftf pris newpris
  let pristine_ok = unseal nullFL pristine_diff

  putVerbose $ text "Checking pending patch..."
  Sealed pend <- readPending repo
  maybe_newpend <- fst <$> hashedTreeIO (applyAndTryToFix pend) newpris cache
  (newpend, pending_ok) <- convertFixed pend maybe_newpend

  return $ RepositoryConsistency
    { fixedPristine = if pristine_ok then Nothing else Just (newpris, pristine_diff)
    , fixedPatches = if patches_ok then Nothing else Just newpatches
    , fixedPending = if pending_ok then Nothing else Just (Sealed newpend)
    }

  where
    convertFixed :: a -> Maybe (String, a) -> IO (a, Bool)
    convertFixed x Nothing = return (x, True)
    convertFixed _ (Just (e, x)) = do
      unless (verbosity == Quiet) $ putStrLn e
      return (x, False)

replayRepositoryInTemp
  :: (RepoPatch p, ApplyState p ~ Tree)
  => DiffAlgorithm
  -> Repository rt p wU wR
  -> Verbosity
  -> IO (RepositoryConsistency p wR)
replayRepositoryInTemp dflag r verb = do
  repodir <- getCurrentDirectory
  {- The reason we use withDelayedDir here, instead of withTempDir, is that
  replayRepository' may return a new pristine that is read from the 
  temporary location and reading a Tree is done using lazy ByteStrings (for
  file contents). Then we check if there is a difference to our stored
  pristine, but when there are differences the check may terminate early
  and not all of the new pristine was read/evaluated. This may then cause
  does-not-exist-failures later on when the tree is evaluated further.
  -}
  withDelayedDir "darcs-check" $ \tmpDir -> do
    setCurrentDirectory repodir
    replayRepository' dflag (mkDirCache (toFilePath tmpDir)) r verb

replayRepository
  :: (RepoPatch p, ApplyState p ~ Tree)
  => DiffAlgorithm
  -> Repository rt p wU wR
  -> Verbosity
  -> (RepositoryConsistency p wR -> IO a)
  -> IO a
replayRepository dflag r verb job = do
  createDirectoryIfMissing False pristineDirPath
  st <- replayRepository' dflag (repoCache r) r verb
  job st

checkIndex
  :: (RepoPatch p, ApplyState p ~ Tree)
  => Repository rt p wU wR
  -> Bool
  -> IO Bool
checkIndex repo quiet = do
  index <- treeFromIndex =<< readIndex repo
  pristine <- expand =<< readPristineAndPending repo
  working <- expand =<< restrict pristine <$> readPlainTree "."
  working_hashed <- darcsUpdateHashes working
  let index_paths = [ p | (p, _) <- list index ]
      working_paths = [ p | (p, _) <- list working ]
      index_extra = index_paths \\ working_paths
      working_extra = working_paths \\ index_paths
      gethashes p (Just i1) (Just i2) = (p, itemHash i1, itemHash i2)
      gethashes p (Just i1) Nothing   = (p, itemHash i1, Nothing)
      gethashes p   Nothing (Just i2) = (p,     Nothing, itemHash i2)
      gethashes p   Nothing Nothing   = error $ "Bad case at " ++ show p
      mismatches =
        [miss | miss@(_, h1, h2) <- zipTrees gethashes index working_hashed, h1 /= h2]

      format paths = unlines $ map (("  " ++) . anchorPath "") paths
      mismatches_disp = unlines [ anchorPath "" p ++
                                    "\n    index: " ++ showHash h1 ++
                                    "\n  working: " ++ showHash h2
                                  | (p, h1, h2) <- mismatches ]
  unless (quiet || null index_extra) $
         putStrLn $ "Extra items in index!\n" ++ format index_extra
  unless (quiet || null working_extra) $
         putStrLn $ "Missing items in index!\n" ++ format working_extra
  unless (quiet || null mismatches) $
         putStrLn $ "Hash mismatch(es)!\n" ++ mismatches_disp
  return $ null index_extra && null working_extra && null mismatches

