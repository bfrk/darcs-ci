{-# LANGUAGE CPP #-}
-- Copyright (C) 2009 Petr Rockai
--           (C) 2012 José Neder
--
-- Permission is hereby granted, free of charge, to any person
-- obtaining a copy of this software and associated documentation
-- files (the "Software"), to deal in the Software without
-- restriction, including without limitation the rights to use, copy,
-- modify, merge, publish, distribute, sublicense, and/or sell copies
-- of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
-- NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
-- BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
-- ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
-- CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

module Darcs.Repository.State
    ( restrictSubpaths, restrictBoring, TreeFilter(..), restrictDarcsdir
    -- * Diffs
    , unrecordedChanges
    -- * Trees
    , readPristine, readUnrecorded, readPristineAndPending, readWorking
    , readPendingAndWorking, readUnrecordedFiltered
    -- * Index
    , readIndex, updateIndex
    -- * Utilities
    , filterOutConflicts
    -- * Pending-related functions that depend on repo state
    , unsafeAddToPending, addToPending
    ) where

import Darcs.Prelude

import Control.Monad ( when, foldM, forM, void )
import Control.Monad.State ( StateT, runStateT, get, put, liftIO )
import Control.Exception ( catch, IOException )
import Data.Ord ( comparing )
import Data.List ( sortBy, union, delete )

import System.Directory( doesFileExist, renameFile )
import System.FilePath ( (<.>) )

import qualified Data.ByteString as B ( ByteString, concat )
import qualified Data.ByteString.Char8 as BC ( pack, unpack )
import qualified Data.ByteString.Lazy as BL ( toChunks )

import Darcs.Patch ( RepoPatch, PrimOf, canonizeFL
                   , PrimPatch, maybeApplyToTree
                   , tokreplace, forceTokReplace, move )
import Darcs.Patch.Named ( anonymous )
import Darcs.Patch.Apply ( ApplyState, applyToTree, effectOnPaths )
import Darcs.Patch.Witnesses.Ordered ( FL(..), (+>+)
                                     , (:>)(..), reverseRL, reverseFL
                                     , mapFL, concatFL, toFL, nullFL )
import Darcs.Patch.Witnesses.Eq ( EqCheck(IsEq, NotEq) )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP, unsafeCoercePEnd )
import Darcs.Patch.Witnesses.Sealed ( Sealed(Sealed), seal, unFreeLeft, mapSeal
                                    , freeGap, emptyGap, joinGap, FreeLeft, Gap(..) )
import Darcs.Patch.Commute ( commuteFL )
import Darcs.Patch.Permutations ( partitionConflictingFL, genCommuteWhatWeCanRL )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, n2pia )
import Darcs.Patch.FileHunk ( FileHunk(..), IsHunk(..) )
import Darcs.Patch.TokenReplace ( breakToTokens, defaultToks )

import Darcs.Repository.Flags
    ( DiffAlgorithm(..)
    , LookForMoves(..)
    , LookForReplaces(..)
    , LookForAdds(..)
    , UseIndex(..)
    , DiffOpts(..)
    )
import Darcs.Repository.InternalTypes
    ( AccessType(..)
    , Repository
    , repoFormat
    , repoLocation
    )
import Darcs.Repository.Format(formatHas, RepoProperty(NoWorkingDir))
import qualified Darcs.Repository.Pending as Pending
import Darcs.Repository.Prefs ( filetypeFunction, isBoring )
import Darcs.Repository.Pristine ( readPristine )
import Darcs.Repository.Diff ( treeDiff )
import Darcs.Repository.Paths
    ( indexPath
    , indexInvalidPath
    )

import Darcs.Util.File ( removeFileMayNotExist )
import Darcs.Util.Global ( debugMessage )
import Darcs.Util.Path
    ( AnchoredPath
    , realPath
    , filterPaths
    , inDarcsdir
    , parents
    , movedirfilename
    )
import Darcs.Util.Tree( Tree, restrict, FilterTree, expand, emptyTree, overlay, find
                      , ItemType(..), itemType, readBlob, modifyTree, findFile, TreeItem(..)
                      , makeBlobBS, expandPath )
import qualified Darcs.Util.Tree.Plain as PlainTree ( readPlainTree )
import Darcs.Util.Tree.Hashed ( darcsTreeHash )
import Darcs.Util.Index
    ( Index
    , indexFormatValid
    , openIndex
    , treeFromIndex
    , updateIndexFrom
    )
import qualified Darcs.Util.Tree as Tree
import Darcs.Util.Index ( listFileIDs, getFileID )

#define TEST_INDEX 0

#if TEST_INDEX
import Control.Monad ( unless )
import Darcs.Util.Path ( displayPath )
import Darcs.Util.Tree ( list )
#else
import System.IO ( hPutStrLn, stderr )
import System.IO.Error ( catchIOError )
#endif

newtype TreeFilter m = TreeFilter { applyTreeFilter :: forall tr . FilterTree tr m => tr m -> tr m }

-- | From a repository and a list of AnchoredPath's, construct a filter that can be
-- used on a Tree (recorded or unrecorded state) of this repository. This
-- constructed filter will take pending into account, so the subpaths will be
-- translated correctly relative to pending move patches.
restrictSubpaths :: (RepoPatch p, ApplyState p ~ Tree)
                 => Repository rt p wU wR -> [AnchoredPath]
                 -> IO (TreeFilter m)
restrictSubpaths repo paths = do
  Sealed pending <- Pending.readPending repo
  restrictSubpathsAfter pending repo paths

-- | Like 'restrictSubpaths' but with the pending patch passed as a parameter.
-- The 'Repository' parameter is not used, we need it only to avoid
-- abiguous typing of @p@.
restrictSubpathsAfter :: (RepoPatch p, ApplyState p ~ Tree)
                      => FL (PrimOf p) wR wP
                      -> Repository rt p wU wR
                      -> [AnchoredPath]
                      -> IO (TreeFilter m)
restrictSubpathsAfter pending _repo paths = do
  let paths' = paths `union` effectOnPaths pending paths
      restrictPaths :: FilterTree tree m => tree m -> tree m
      restrictPaths = Tree.filter (filterPaths paths')
  return (TreeFilter restrictPaths)

-- note we assume pending starts at the recorded state
maybeRestrictSubpaths :: (RepoPatch p, ApplyState p ~ Tree)
                      => FL (PrimOf p) wR wP
                      -> Repository rt p wU wR
                      -> Maybe [AnchoredPath]
                      -> IO (TreeFilter m)
maybeRestrictSubpaths pending repo =
  maybe (return $ TreeFilter id) (restrictSubpathsAfter pending repo)

-- | Construct a 'TreeFilter' that removes any boring files that are not also
-- contained in the argument 'Tree'.
--
-- The standard use case is for the argument to be the recorded state, possibly
-- with further patches applied, so as not to discard any files already known
-- to darcs. The result is usually applied to the full working state.
restrictBoring :: Tree m -> IO (TreeFilter m)
restrictBoring guide = do
  boring <- isBoring
  let exclude p t = inDarcsdir p || boring (appendSlash t (realPath p))
      appendSlash TreeType fp = fp ++ "/"
      appendSlash BlobType fp = fp
      restrictTree :: FilterTree t m => t m -> t m
      restrictTree =
        Tree.filter $ \p i ->
          case find guide p of
            Nothing -> not (exclude p (itemType i))
            _ -> True
  return (TreeFilter restrictTree)

-- | Construct a Tree filter that removes any darcs metadata files the
-- Tree might have contained.
restrictDarcsdir :: TreeFilter m
restrictDarcsdir = TreeFilter $ Tree.filter $ \p _ -> not (inDarcsdir p)

{- |
For a repository and an optional list of paths (when 'Nothing', take
everything) compute a (forward) list of prims (i.e. a patch) going from the
recorded state of the repository (pristine) to the unrecorded state of the
repository (the working tree + pending). When a list of paths is given, at
least the files that live under any of these paths in either recorded or
unrecorded will be included in the resulting patch. NB. More patches may be
included in this list, eg. the full contents of the pending patch. This is
usually not a problem, since selectChanges will properly filter the results
anyway.

This also depends on the options given:

--look-for-moves: Detect pending file moves using the index. The resulting
  patches are added to pending and taken into consideration, when filtering
  the tree according to the given path list.

--look-for-adds: Include files in the working state that do not exist in the
  recorded + pending state.

--include-boring: Include even boring files.

--look-for-replaces: Detect pending replace patches. Like detected moves,
  these are added to the pending patch. Note that, like detected moves,
  these are mere proposals for the user to consider or reject.

--ignore-times: Disables index usage completely -- for each file, we read
  both the unrecorded and the recorded copy and run a diff on them. This is
  very inefficient, although in extremely rare cases, the index could go out
  of sync (file is modified, index is updated and file is modified again
  within a single second).

  Note that use of the index is also disabled when we detect moves or
  replaces, since this implies that the index is out of date.
-}
unrecordedChanges :: (RepoPatch p, ApplyState p ~ Tree)
                  => DiffOpts
                  -> Repository rt p wU wR
                  -> Maybe [AnchoredPath] -> IO (FL (PrimOf p) wR wU)
unrecordedChanges dopts@DiffOpts{..} r paths = do
  (pending :> working) <- readPendingAndWorking dopts r paths
  return $ canonizeFL diffAlg (pending +>+ working)

-- Implementation note: it is important to do things in the right order: we
-- first have to read the pending patch, then detect moves, then detect adds,
-- then detect replaces.
readPendingAndWorking :: (RepoPatch p, ApplyState p ~ Tree)
                      => DiffOpts
                      -> Repository rt p wU wR
                      -> Maybe [AnchoredPath]
                      -> IO ((FL (PrimOf p) :> FL (PrimOf p)) wR wU)
readPendingAndWorking _ r _ | formatHas NoWorkingDir (repoFormat r) = do
  IsEq <- return $ workDirLessRepoWitness r
  return (NilFL :> NilFL)
readPendingAndWorking DiffOpts{..} repo mbpaths = do
  debugMessage "readPendingAndWorking: start"
  (pending_tree, working_tree, (pending :> moves)) <-
    readPendingAndMovesAndUnrecorded repo withIndex lookForAdds lookForMoves mbpaths
  debugMessage "readPendingAndWorking: after readPendingAndMovesAndUnrecorded"
  (pending_tree_with_replaces, Sealed replaces) <-
    getReplaces lookForReplaces diffAlg repo pending_tree working_tree
  debugMessage "readPendingAndWorking: after getReplaces"
  ft <- filetypeFunction
  wrapped_diff <- treeDiff diffAlg ft pending_tree_with_replaces working_tree
  case unFreeLeft wrapped_diff of
    Sealed diff -> do
      debugMessage "readPendingAndWorking: done"
      return $ unsafeCoercePEnd $ pending :> (moves +>+ replaces +>+ diff)

readPendingAndMovesAndUnrecorded
  :: (RepoPatch p, ApplyState p ~ Tree)
  => Repository rt p wU wR
  -> UseIndex
  -> LookForAdds
  -> LookForMoves
  -> Maybe [AnchoredPath]
  -> IO ( Tree IO             -- pristine with (pending + moves)
        , Tree IO             -- working
        , (FL (PrimOf p) :> FL (PrimOf p)) wR wM -- pending :> moves
        )
readPendingAndMovesAndUnrecorded repo useidx scan lfm mbpaths = do
  debugMessage "readPendingAndMovesAndUnrecorded: start"
  (pending_tree, Sealed pending) <- readPending repo
  moves <- getMoves lfm repo mbpaths
  -- we want to include any user specified paths before and after pending
  -- and detected moves
  relevant <- maybeRestrictSubpaths (pending +>+ moves) repo mbpaths
  pending_tree_with_moves <-
    applyTreeFilter relevant <$> applyToTree moves pending_tree
  debugMessage "readPendingAndMovesAndUnrecorded: before readIndexOrPlainTree"
  -- the moves are detected i.e. they are already applied in the working tree;
  -- also note that we have to use the amended pending tree to restrict the
  -- working tree in case we don't use the index (here and below)
  index <- readIndexOrPlainTree repo useidx relevant pending_tree_with_moves
  debugMessage "readPendingAndMovesAndUnrecorded: before filteredWorking"
  -- TODO this conditional looks wrong; so if we do have detected moves,
  -- then we cannot use the index to read the working state? Why not?
  let useidx' = if nullFL moves then useidx else IgnoreIndex
  working_tree <-
    filteredWorking repo useidx' scan relevant index pending_tree_with_moves
  debugMessage "readPendingAndMovesAndUnrecorded: done"
  return
    (pending_tree_with_moves, working_tree, unsafeCoercePEnd (pending :> moves))

-- | @filteredWorking useidx scan relevant from_index pending_tree@ reads the
-- working tree and filters it according to options and @relevant@ file paths.
-- The @pending_tree@ is understood to have @relevant@ already applied and is
-- used (only) if @useidx == 'IgnoreIndex'@ and @scan /= 'EvenLookForBoring'@ to act as
-- a guide for filtering the working tree.
filteredWorking :: Repository rt p wU wR
                -> UseIndex
                -> LookForAdds
                -> TreeFilter IO
                -> Tree IO
                -> Tree IO
                -> IO (Tree IO)
filteredWorking repo useidx scan relevant from_index pending_tree =
  applyTreeFilter restrictDarcsdir <$> applyTreeFilter relevant <$>
    case useidx of
      UseIndex ->
        case scan of
          NoLookForAdds -> return from_index
          YesLookForAdds -> do
            nonboring <- restrictBoring from_index
            plain <- applyTreeFilter nonboring <$> readPlainTree repo
            return $ plain `overlay` from_index
          EvenLookForBoring -> do
            plain <- readPlainTree repo
            return $ plain `overlay` from_index
      IgnoreIndex -> do
        working <- readPlainTree repo
        case scan of
          NoLookForAdds -> do
            guide <- expand pending_tree
            return $ restrict guide working
          YesLookForAdds -> do
            guide <- expand pending_tree
            nonboring <- restrictBoring guide
            return $ applyTreeFilter nonboring working
          EvenLookForBoring -> return working

-- | Witnesses the fact that in the absence of a working tree, the
-- unrecorded state cannot differ from the record state.
workDirLessRepoWitness :: Repository rt p wU wR -> EqCheck wU wR
workDirLessRepoWitness r
 | formatHas NoWorkingDir (repoFormat r) = unsafeCoerceP IsEq
 | otherwise                             = NotEq

-- | Obtains a Tree corresponding to the "unrecorded" state of the repository:
-- the modified files of the working tree plus the "pending" patch.
-- The optional list of paths allows to restrict the query to a subtree.
--
-- Limiting the query may be more efficient, since hashes on the uninteresting
-- parts of the index do not need to go through an up-to-date check (which
-- involves a relatively expensive lstat(2) per file.
readUnrecorded :: (RepoPatch p, ApplyState p ~ Tree)
               => Repository rt p wU wR
               -> UseIndex
               -> Maybe [AnchoredPath]
               -> IO (Tree IO)
readUnrecorded repo useidx mbpaths = do
#if TEST_INDEX
  t1 <- expand =<< readUnrecordedFiltered repo useidx NoLookForAdds NoLookForMoves mbpaths
  (pending_tree, Sealed pending) <- readPending repo
  relevant <- maybeRestrictSubpaths pending repo mbpaths
  t2 <- readIndexOrPlainTree repo useidx relevant pending_tree
  assertEqualTrees "indirect" t1 "direct" t2
  return t1
#else
  expand =<< readUnrecordedFiltered repo useidx NoLookForAdds NoLookForMoves mbpaths
#endif

#if TEST_INDEX
assertEqualTrees :: String -> Tree m -> String -> Tree m -> IO ()
assertEqualTrees n1 t1 n2 t2 =
  unless (t1 `eqTree` t2) $
    fail $ "Trees are not equal!\n" ++ showTree n1 t1 ++ showTree n2 t2

eqTree :: Tree m -> Tree m -> Bool
eqTree t1 t2 = map fst (list t1) == map fst (list t2)

showTree :: String -> Tree m -> String
showTree name tree = unlines (name : map (("  "++) . displayPath . fst) (list tree))
#endif

readIndexOrPlainTree :: (ApplyState p ~ Tree, RepoPatch p)
                     => Repository rt p wU wR
                     -> UseIndex
                     -> TreeFilter IO
                     -> Tree IO
                     -> IO (Tree IO)
#if TEST_INDEX
readIndexOrPlainTree repo useidx treeFilter pending_tree = do
  indexTree <-
    treeFromIndex =<< applyTreeFilter treeFilter <$> readIndex repo
  plainTree <- do
    guide <- expand pending_tree
    expand =<< applyTreeFilter treeFilter . restrict guide <$> readPlainTree repo
  assertEqualTrees "index tree" indexTree "plain tree" plainTree
  return $
    case useidx of
      UseIndex -> indexTree
      IgnoreIndex -> plainTree
#else
readIndexOrPlainTree repo UseIndex treeFilter pending_tree =
  (treeFromIndex =<< applyTreeFilter treeFilter <$> readIndex repo)
    `catchIOError` \e -> do
      hPutStrLn stderr ("Warning, cannot access the index:\n" ++ show e)
      readIndexOrPlainTree repo IgnoreIndex treeFilter pending_tree
readIndexOrPlainTree repo IgnoreIndex treeFilter pending_tree = do
  guide <- expand pending_tree
  expand =<< applyTreeFilter treeFilter . restrict guide <$> readPlainTree repo
#endif

-- | A variant of 'readUnrecorded' that takes the UseIndex and LookForAdds
-- options into account, similar to 'readPendingAndWorking'. We are only
-- interested in the resulting tree, not the patch, so the 'DiffAlgorithm' option
-- is irrelevant.
readUnrecordedFiltered :: (RepoPatch p, ApplyState p ~ Tree)
                       => Repository rt p wU wR
                       -> UseIndex
                       -> LookForAdds
                       -> LookForMoves
                       -> Maybe [AnchoredPath] -> IO (Tree IO)
readUnrecordedFiltered repo useidx scan lfm mbpaths = do
  (_, working_tree, _) <-
    readPendingAndMovesAndUnrecorded repo useidx scan lfm mbpaths
  return working_tree

-- | Obtains the relevant (according to the given filter) part of the working tree.
readWorking :: TreeFilter IO -> IO (Tree IO)
readWorking relevant =
  expand =<<
  (applyTreeFilter relevant . applyTreeFilter restrictDarcsdir <$>
   PlainTree.readPlainTree ".")

-- | Obtains the recorded 'Tree' with the pending patch applied.
readPristineAndPending :: (RepoPatch p, ApplyState p ~ Tree)
                       => Repository rt p wU wR -> IO (Tree IO)
readPristineAndPending repo = fst `fmap` readPending repo

-- | Obtains the recorded 'Tree' with the pending patch applied, plus
--   the pending patch itself. The pending patch should start at the
--   recorded state (we even verify that it applies, and degrade to
--   renaming pending and starting afresh if it doesn't).
readPending :: (RepoPatch p, ApplyState p ~ Tree)
            => Repository rt p wU wR
            -> IO (Tree IO, Sealed (FL (PrimOf p) wR))
readPending repo = do
  pristine <- readPristine repo
  Sealed pending <- Pending.readPending repo
  catch ((\t -> (t, seal pending)) <$> applyToTree pending pristine) $ \(e::IOException) -> do
    fail $
      "Cannot apply pending patch, please run `darcs repair`\n"
      ++ show e

-- | Open the index or re-create it in case it is invalid or non-existing.
readIndex :: (RepoPatch p, ApplyState p ~ Tree)
          => Repository rt p wU wR -> IO Index
readIndex repo = do
  okay <- checkIndex
  if not okay
    then internalUpdateIndex repo
    else openIndex indexPath (Just . darcsTreeHash)

-- | Update the index so that it matches pristine+pending. If the index does
-- not exist or is invalid, create a new one. Returns the updated index.
internalUpdateIndex :: (RepoPatch p, ApplyState p ~ Tree)
            => Repository rt p wU wR -> IO Index
internalUpdateIndex repo = do
  pris <-
    readPristineAndPending repo
    `catch` \(_::IOException) -> readPristine repo
  idx <- updateIndexFrom indexPath (Just . darcsTreeHash) pris
  removeFileMayNotExist indexInvalidPath
  return idx

-- | Update the index so that it matches pristine+pending. If the index does
-- not exist or is invalid, create a new one.
--
-- This has to be called whenever the listing of pristine+pending changes. Note
-- that this only concerns files added and removed or renamed: changes to file
-- content in either pristine or working are handled transparently by the index
-- reading code.
updateIndex :: (RepoPatch p, ApplyState p ~ Tree)
            => Repository rt p wU wR -> IO ()
updateIndex repo = do
  -- call checkIndex to throw away the index if it is invalid;
  -- this can happen if we are called with --ignore-times
  -- TODO make this impossible i.e. honor UseIndex here
  void checkIndex
  void $ internalUpdateIndex repo

-- | Check if we have a valid index. This means that the index file exists, is
-- readable, and can be mmapped. For compatibility with older darcs versions we
-- also check that indexInvalidPath does not exist. We do not yet remove
-- indexInvalidPath in case updating the index fails.
checkIndex :: IO Bool
checkIndex = do
  invalid <- doesFileExist $ indexInvalidPath
  formatValid <- indexFormatValid indexPath
  exist <- doesFileExist indexPath
  -- this fails with a permission (access denied) error on windows
  -- if we use removeFileMayNotExist instead of renameFile
  when (exist && not formatValid) $ renameFile indexPath (indexPath <.> "old")
  return (not invalid && formatValid)

-- |Remove any patches (+dependencies) from a sequence that
-- conflict with the recorded or unrecorded changes in a repo
filterOutConflicts
  :: (RepoPatch p, ApplyState p ~ Tree)
  => Repository rt p wU wR     -- ^Repository itself, used for grabbing
                                  --  unrecorded changes
  -> UseIndex                     -- ^Whether to use the index when reading
                                  --  the working state
  -> FL (PatchInfoAnd p) wX wR -- ^Recorded patches from repository, starting from
                                  --  same context as the patches to filter
  -> FL (PatchInfoAnd p) wX wZ -- ^Patches to filter
  -> IO (Bool, Sealed (FL (PatchInfoAnd p) wX))
                                  -- ^True iff any patches were removed,
                                  --  possibly filtered patches
filterOutConflicts repository useidx us them
     = do -- Note: use of anonymous is benign here since we only try to merge cleanly
          unrec <- fmap n2pia . anonymous
                     =<< unrecordedChanges
                          (DiffOpts useidx NoLookForAdds NoLookForReplaces
                          NoLookForMoves MyersDiff) repository Nothing
          them' :> rest <-
            return $ partitionConflictingFL them (us +>+ unrec :>: NilFL)
          return (check rest, Sealed them')
  where check :: FL p wA wB -> Bool
        check NilFL = False
        check _ = True

-- | Automatically detect file moves using the index.
-- TODO: This function lies about the witnesses.
getMoves :: forall rt p wU wR wB prim.
            (RepoPatch p, ApplyState p ~ Tree, prim ~ PrimOf p)
         => LookForMoves
         -> Repository rt p wU wR
         -> Maybe [AnchoredPath]
         -> IO (FL prim wB wB)
getMoves NoLookForMoves _ _ = return NilFL
getMoves YesLookForMoves repository files =
    mkMovesFL <$> getMovedFiles repository files
  where
    mkMovesFL [] = NilFL
    mkMovesFL ((a,b,_):xs) = move a b :>: mkMovesFL xs

    getMovedFiles :: Repository rt p wU wR
                  -> Maybe [AnchoredPath]
                  -> IO [(AnchoredPath, AnchoredPath, ItemType)]
    getMovedFiles repo fs = do
        old <- sortBy (comparing snd) <$> (listFileIDs =<< readIndex repo)
        nonboring <- restrictBoring emptyTree
        let addIDs = foldM (\xs (p, it)-> do mfid <- getFileID p
                                             return $ case mfid of
                                               Nothing -> xs
                                               Just fid -> ((p, it), fid):xs) []
        new <- sortBy (comparing snd) <$>
                 (addIDs . map (\(a,b) -> (a, itemType b)) . Tree.list  =<<
                   expand =<< applyTreeFilter nonboring <$> readPlainTree repository)
        let match (x:xs) (y:ys)
              | snd x > snd y = match (x:xs) ys
              | snd x < snd y = match xs (y:ys)
              | snd (fst x) /= snd (fst y) = match xs ys
              | otherwise = (fst (fst x), fst (fst y), snd (fst x)):match xs ys
            match _ _ = []
            movedfiles = match old new
            fmovedfiles =
              case fs of
                Nothing -> movedfiles
                Just paths ->
                  filter (\(f1, f2, _) -> any (`elem` selfiles) [f1, f2]) movedfiles
                  where selfiles = paths
        return (resolve fmovedfiles)

    resolve :: [(AnchoredPath, AnchoredPath, ItemType)]
            -> [(AnchoredPath, AnchoredPath, ItemType)]
    resolve xs = fixPaths $ sortMoves $ deleteCycles xs
      where
        -- Input relation is left-and-right-unique. Makes cycle detection easier.
        deleteCycles [] = []
        deleteCycles whole@( x@(start,_,_):rest)
            = if hasCycle start whole start
                  then deleteCycles (deleteFrom start whole [])
                  else x:deleteCycles rest
           where hasCycle current ((a',b',_):rest') first
                     | a' == current = b' == first || hasCycle b' whole first
                     | otherwise     = hasCycle current rest' first 
                 hasCycle _ [] _     = False
                 deleteFrom a (y@(a',b',_):ys) seen
                   | a == a'   = deleteFrom b' (seen++ys) []
                   | otherwise = deleteFrom a ys (y:seen)
                 deleteFrom _ [] seen = seen

        sortMoves []                           = []
        sortMoves whole@(current@(_,dest,_):_) =
              smallest:sortMoves (delete smallest whole)
              where
               smallest = follow dest whole current
               follow prevDest (y@(s,d,_):ys) currentSmallest
                 -- destination is source of another move
                 | prevDest == s             = follow d whole y
                 -- parent of destination is also destination of a move
                 | d `elem` parents prevDest = follow d whole y
                 | otherwise     = follow prevDest ys currentSmallest
               follow _ [] currentSmallest = currentSmallest

        -- rewrite [d/ -> e/, .., d/f -> e/h] to [d/ -> e/, .., e/f -> e/h]
        -- and throw out moves that don't move anything (can they be in there?)
        fixPaths [] = []
        fixPaths (y@(f1,f2,t):ys)
                        | f1 == f2         = fixPaths ys -- no effect, throw out
                        | TreeType <- t    = y:fixPaths (map replacepp ys)
                        | otherwise        = y:fixPaths ys
         -- TODO why adapt only if1 here and not if2?
         --      is this a bug?
         where replacepp (if1,if2,it) = (movedirfilename f1 f2 if1, if2, it)

-- | Search for possible replaces between the recordedAndPending state
-- and the unrecorded (or working) state. Return a Sealed FL list of
-- replace patches to be applied to the recordedAndPending state.
getReplaces :: forall rt p wU wR
             . (RepoPatch p, ApplyState p ~ Tree)
            => LookForReplaces
            -> DiffAlgorithm
            -> Repository rt p wU wR
            -> Tree IO -- ^ pending tree (including possibly detected moves)
            -> Tree IO -- ^ working tree
            -> IO (Tree IO, -- new pending tree
                   Sealed (FL (PrimOf p) wU))
getReplaces NoLookForReplaces _ _ pending _ = return (pending, Sealed NilFL)
getReplaces YesLookForReplaces diffalg _repo pending working = do
    ftf <- filetypeFunction
    Sealed changes <- unFreeLeft <$> treeDiff diffalg ftf pending working
    let allModifiedTokens = concat $ mapFL modifiedTokens changes
        replaces = rmInvalidReplaces allModifiedTokens
    (patches, new_pending) <-
      flip runStateT pending $
        forM replaces $ \(path, a, b) ->
          doReplace defaultToks path (BC.unpack a) (BC.unpack b)
    return (new_pending, mapSeal concatFL $ toFL patches)
  where
    modifiedTokens :: PrimOf p wX wY -> [(AnchoredPath, B.ByteString, B.ByteString)]
    modifiedTokens p = case isHunk p of
      Just (FileHunk f _ old new) ->
        map (\(a,b) -> (f, a, b)) (concatMap checkModified $
          filter (\(a,b) -> length a == length b) -- only keep lines with same number of tokens
            $ zip (map breakToTokens old) (map breakToTokens new))
      Nothing -> []

    -- from a pair of token lists, create a pair of modified token lists
    checkModified = filter (\(a,b) -> a/=b) . uncurry zip

    rmInvalidReplaces [] = []
    rmInvalidReplaces ((f,old,new):rs)
      | any (\(f',a,b) -> f' == f && old == a && b /= new) rs =
          -- inconsistency detected
          rmInvalidReplaces $ filter (\(f'',a',_) -> f'' /= f || a' /= old) rs
    rmInvalidReplaces (r:rs) = r:rmInvalidReplaces (filter (/=r) rs)

    doReplace toks path old new = do
        pend <- get
        mpend' <- liftIO $ maybeApplyToTree replacePatch pend
        case mpend' of
          Nothing -> getForceReplace path toks old new
          Just pend' -> do
            put pend'
            return $ joinGap (:>:) (freeGap replacePatch) (emptyGap NilFL)
      where
        replacePatch = tokreplace path toks old new

    getForceReplace :: (PrimPatch prim, ApplyState prim ~ Tree)
                    => AnchoredPath -> String -> String -> String
                    -> StateT (Tree IO) IO (FreeLeft (FL prim))
    getForceReplace path toks old new = do
        -- the tree here is the "current" pending state
        tree <- get
        -- It would be nice if we could fuse the two traversals here, that is,
        -- expandPath and findFile. OTOH it is debatable whether adding a new
        -- effectful version of findFile to Darcs.Util.Tree is justified.
        expandedTree <- liftIO $ expandPath tree path
        content <- case findFile expandedTree path of
          Just blob -> liftIO $ readBlob blob
          Nothing -> error $ "getForceReplace: not in tree: " ++ show path
        let newcontent = forceTokReplace toks (BC.pack new) (BC.pack old)
                            (B.concat $ BL.toChunks content)
            tree' = modifyTree expandedTree path . Just . File $ makeBlobBS newcontent
        ftf <- liftIO $ filetypeFunction
        normaliseNewTokPatch <- liftIO $ treeDiff diffalg ftf expandedTree tree'
        -- make sure we can apply them to the pending state
        patches <- return $ joinGap (+>+) normaliseNewTokPatch $ freeGap $
            tokreplace path toks old new :>: NilFL
        mtree'' <- case unFreeLeft patches of
            Sealed ps -> liftIO $ maybeApplyToTree ps tree
        case mtree'' of
            Nothing -> error "getForceReplace: unable to apply detected force replaces"
            Just tree'' -> do
                put tree''
                return patches

-- | Add an 'FL' of patches started from the pending state to the pending patch.
unsafeAddToPending :: (RepoPatch p, ApplyState p ~ Tree)
                   => Repository 'RW p wU wR
                   -> FreeLeft (FL (PrimOf p)) -> IO ()
unsafeAddToPending repo newP = do
    (_, Sealed toPend) <- readPending repo
    case unFreeLeft newP of
        (Sealed p) -> do
            Pending.writeTentativePending repo (toPend +>+ p)

-- | Add an 'FL' of patches starting from the working state to the pending patch,
-- including as much extra context as is necessary (context meaning
-- dependencies), by commuting the patches to be added past as much of the
-- changes between pending and working as is possible, and including anything
-- that doesn't commute, and the patch itself in the new pending patch.
addToPending :: (RepoPatch p, ApplyState p ~ Tree)
             => Repository 'RW p wU wR
             -> DiffOpts
             -> FL (PrimOf p) wU wY -> IO ()
addToPending repo dopts p = do
   (toPend :> toUnrec) <-
      readPendingAndWorking dopts repo Nothing
   case genCommuteWhatWeCanRL commuteFL (reverseFL toUnrec :> p) of
       (toP' :> p'  :> _excessUnrec) -> do
           Pending.writeTentativePending repo (toPend +>+ reverseRL toP' +>+ p')

readPlainTree :: Repository rt p wU wR -> IO (Tree IO)
readPlainTree repo  = PlainTree.readPlainTree (repoLocation repo)
