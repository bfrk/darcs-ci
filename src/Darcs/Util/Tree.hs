--  Copyright (C) 2009-2011 Petr Rockai
--
--  BSD3
{-# LANGUAGE MultiParamTypeClasses #-}

-- | The abstract representation of a Tree and useful abstract utilities to
-- handle those.
module Darcs.Util.Tree
    ( Tree, Blob(..), TreeItem(..), ItemType(..), Hash
    , makeTree, makeTreeWithHash, emptyTree, emptyBlob, makeBlob, makeBlobBS

    -- * Unfolding stubbed (lazy) Trees.
    --
    -- | By default, Tree obtained by a read function is stubbed: it will
    -- contain Stub items that need to be executed in order to access the
    -- respective subtrees. 'expand' will produce an unstubbed Tree.
    , expandUpdate, expand, expandPath, checkExpand

    -- * Tree access and lookup.
    , items, list, listImmediate, treeHash
    , lookup, find, findFile, findTree, itemHash, itemType
    , zipCommonFiles, zipFiles, zipTrees, diffTrees
    , explodePath, explodePaths, locate, isDir
    , treeHas, treeHasDir, treeHasFile, treeHasAnycase

    -- * Files (Blobs).
    , readBlob

    -- * Filtering trees.
    , FilterTree(..), restrict

    -- * Manipulating trees.
    , modifyTree, updateTree, partiallyUpdateTree, updateSubtrees, overlay
    , addMissingHashes

    -- * Properties
    , prop_explodePath
    ) where

import Darcs.Prelude hiding ( filter )
import qualified Prelude ( filter )

import Control.Exception( catch, IOException )
import Darcs.Util.Path
import Darcs.Util.Hash

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.Map as M

import Data.Maybe( catMaybes, isNothing )
import Data.Either( lefts, rights )
import Data.List( union, sort )
import qualified Data.List
import Control.Monad( filterM, join )

--------------------------------
-- Tree, Blob and friends
--

data Blob m = Blob !(m BL.ByteString) !(Maybe Hash)
data TreeItem m = File !(Blob m)
                | SubTree !(Tree m)
                | Stub !(m (Tree m)) !(Maybe Hash)

data ItemType = TreeType | BlobType deriving (Show, Eq, Ord)

-- | Abstraction of a filesystem tree.
-- Please note that the Tree returned by the respective read operations will
-- have TreeStub items in it. To obtain a Tree without such stubs, call
-- expand on it, eg.:
--
-- > tree <- readDarcsPristine "." >>= expand
--
-- When a Tree is expanded, it becomes \"final\". All stubs are forced and the
-- Tree can be traversed purely. Access to actual file contents stays in IO
-- though.
--
-- A Tree may have a Hash associated with it. A pair of Tree's is identical
-- whenever their hashes are (the reverse need not hold, since not all Trees
-- come equipped with a hash).
data Tree m = Tree { items :: M.Map Name (TreeItem m)
                   -- | Get hash of a Tree. This is guaranteed to uniquely
                   -- identify the Tree (including any blob content), as far as
                   -- cryptographic hashes are concerned. Sha256 is recommended.
                   , treeHash :: !(Maybe Hash) }

listImmediate :: Tree m -> [(Name, TreeItem m)]
listImmediate = M.toList . items

-- | Get a hash of a TreeItem. May be Nothing.
itemHash :: TreeItem m -> Maybe Hash
itemHash (File (Blob _ h)) = h
itemHash (SubTree t) = treeHash t
itemHash (Stub _ h) = h

itemType :: TreeItem m -> ItemType
itemType (File _) = BlobType
itemType (SubTree _) = TreeType
itemType (Stub _ _) = TreeType

emptyTree :: Tree m
emptyTree = Tree { items = M.empty
                 , treeHash = Nothing }

emptyBlob :: (Monad m) => Blob m
emptyBlob = Blob (return BL.empty) Nothing

makeBlob :: (Monad m) => BL.ByteString -> Blob m
makeBlob str = Blob (return str) (Just $ sha256 str)

makeBlobBS :: (Monad m) => B.ByteString -> Blob m
makeBlobBS s' = let s = BL.fromChunks [s'] in Blob (return s) (Just $ sha256 s)

makeTree :: [(Name,TreeItem m)] -> Tree m
makeTree l = Tree { items = M.fromList l
                  , treeHash = Nothing }

makeTreeWithHash :: [(Name,TreeItem m)] -> Hash -> Tree m
makeTreeWithHash l h = Tree { items = M.fromList l
                            , treeHash = Just h }

-----------------------------------
-- Tree access and lookup
--

-- | Look up a 'Tree' item (an immediate subtree or blob).
lookup :: Tree m -> Name -> Maybe (TreeItem m)
lookup t n = M.lookup n (items t)

find' :: TreeItem m -> AnchoredPath -> Maybe (TreeItem m)
find' t (AnchoredPath []) = Just t
find' (SubTree t) (AnchoredPath (d : rest)) =
    case lookup t d of
      Just sub -> find' sub (AnchoredPath rest)
      Nothing -> Nothing
find' _ _ = Nothing

-- | Find a 'TreeItem' by its path. Gives 'Nothing' if the path is invalid.
find :: Tree m -> AnchoredPath -> Maybe (TreeItem m)
find = find' . SubTree

-- | Find a 'Blob' by its path. Gives 'Nothing' if the path is invalid, or does
-- not point to a Blob.
findFile :: Tree m -> AnchoredPath -> Maybe (Blob m)
findFile t p = case find t p of
                 Just (File x) -> Just x
                 _ -> Nothing

-- | Find a 'Tree' by its path. Gives 'Nothing' if the path is invalid, or does
-- not point to a Tree.
findTree :: Tree m -> AnchoredPath -> Maybe (Tree m)
findTree t p = case find t p of
                 Just (SubTree x) -> Just x
                 _ -> Nothing

-- | List all contents of a 'Tree'.
list :: Tree m -> [(AnchoredPath, TreeItem m)]
list t_ = paths t_ (AnchoredPath [])
    where paths t p = [ (appendPath p n, i)
                          | (n,i) <- listImmediate t ] ++
                    concat [ paths subt (appendPath p subn)
                             | (subn, SubTree subt) <- listImmediate t ]

-- | Like 'find' but monadic and thus able to expand 'Stub's on the way.
locate :: Monad m => Tree m -> AnchoredPath -> m (Maybe (TreeItem m))
locate tree (AnchoredPath names) = go names (SubTree tree)
  where
    go [] i = return (Just i)
    go _ (File _) = return Nothing
    go ns (Stub mkTree _) = mkTree >>= go ns . SubTree
    go (n:ns) (SubTree t) =
      case lookup t n of
        Nothing -> return Nothing
        Just i -> go ns i

isDir :: TreeItem m -> Bool
isDir (File _) = False
isDir _ = True

treeHasAnycase :: Monad m
               => Tree m
               -> AnchoredPath
               -> m Bool
treeHasAnycase tree (AnchoredPath names) = go names (SubTree tree)
  where
    go [] _ = return True
    go ns (Stub mkTree _) = mkTree >>= go ns . SubTree
    go _ (File _) = return False
    go (n:ns) (SubTree t) =
      case Data.List.find (eqAnycase n . fst) (listImmediate t) of
        Nothing -> return False
        Just (_,i) -> go ns i

treeHas :: Monad m => Tree m -> AnchoredPath -> m Bool
treeHas tree path = maybe False (const True) <$> locate tree path

treeHasDir :: Monad m => Tree m -> AnchoredPath -> m Bool
treeHasDir tree path = maybe False isDir <$> locate tree path

treeHasFile :: Monad m => Tree m -> AnchoredPath -> m Bool
treeHasFile tree path = maybe False (not . isDir) <$> locate tree path

-- | Like 'explodePath' but for multiple paths.
explodePaths :: Tree IO -> [AnchoredPath] -> [AnchoredPath]
explodePaths tree paths = concatMap (explodePath tree) paths

-- | All paths in the tree that that have the given path as prefix.
--
-- prop> explodePath t p == Prelude.filter (p `isPrefix`) (map fst (list t))
explodePath :: Tree m -> AnchoredPath -> [AnchoredPath]
explodePath tree path =
  path : maybe [] (map (catPaths path . fst) . list) (findTree tree path)

expandUpdate
  :: (Monad m) => (AnchoredPath -> Tree m -> m (Tree m)) -> Tree m -> m (Tree m)
expandUpdate update = go (AnchoredPath [])
  where
    go path t = do
      let subtree (name, sub) = do
            tree <- go (path `appendPath` name) =<< unstub sub
            return (name, SubTree tree)
      expanded <- mapM subtree [ x | x@(_, item) <- listImmediate t, isDir item ]
      let orig_map     = M.filter (not . isDir) (items t)
          expanded_map = M.fromList expanded
          tree         = t { items = M.union orig_map expanded_map }
      update path tree

-- | Expand a stubbed Tree into a one with no stubs in it. You might want to
-- filter the tree before expanding to save IO. This is the basic
-- implementation, which may be overriden by some Tree instances (this is
-- especially true of the Index case).
expand :: (Monad m) => Tree m -> m (Tree m)
expand = expandUpdate $ const return

-- | Unfold a path in a (stubbed) Tree, such that the leaf node of the path is
-- reachable without crossing any stubs. Moreover, the leaf ought not be a Stub
-- in the resulting Tree. A non-existent path is expanded as far as it can be.
expandPath :: (Monad m) => Tree m -> AnchoredPath -> m (Tree m)
expandPath t (AnchoredPath []) = return t
expandPath t (AnchoredPath (n:rest)) =
  case lookup t n of
    (Just item) | isDir item -> amend t n rest =<< unstub item
    _ -> return t -- fail $ "Descent error in expandPath: " ++ show path_
    where
          amend t' name rest' sub = do
            sub' <- expandPath sub (AnchoredPath rest')
            let tree = t' { items = M.insert name (SubTree sub') (items t') }
            return tree

-- | Check the disk version of a Tree: expands it, and checks each
-- hash. Returns either the expanded tree or a list of AnchoredPaths
-- where there are problems. The first argument is the hashing function
-- used to create the tree.
checkExpand :: (TreeItem IO -> IO Hash) -> Tree IO
            -> IO (Either [(AnchoredPath, Maybe Hash, Maybe Hash)] (Tree IO))
checkExpand hashFunc t = go (AnchoredPath []) t
    where
      go path t_ = do
        let
            subtree (name, sub) =
                do let here = path `appendPath` name
                   sub' <- (Just <$> unstub sub) `catch` \(_ :: IOException) -> return Nothing
                   case sub' of
                     Nothing -> return $ Left [(here, treeHash t_, Nothing)]
                     Just sub'' -> do
                       treeOrTrouble <- go (path `appendPath` name) sub''
                       return $ case treeOrTrouble of
                              Left problems -> Left problems
                              Right tree -> Right (name, SubTree tree)
            badBlob (_, f@(File (Blob _ h))) =
              fmap (/= h) (fmap Just (hashFunc f) `catch` (\(_ :: IOException) -> return Nothing))
            badBlob _ = return False
            render (name, f@(File (Blob _ h))) = do
              h' <- (Just <$> hashFunc f) `catch` \(_ :: IOException) -> return Nothing
              return (path `appendPath` name, h, h')
            render (name, _) = return (path `appendPath` name, Nothing, Nothing)
        subs <- mapM subtree [ x | x@(_, item) <- listImmediate t_, isDir item ]
        badBlobs <- filterM badBlob (listImmediate t) >>= mapM render
        let problems = badBlobs ++ concat (lefts subs)
        if null problems
         then do
           let orig_map = M.filter (not . isDir) (items t)
               expanded_map = M.fromList $ rights subs
               tree = t_ {items = orig_map `M.union` expanded_map}
           h' <- hashFunc (SubTree t_)
           if Just h' `match` treeHash t_
            then return $ Right tree
            else return $ Left [(path, treeHash t_, Just h')]
         else return $ Left problems

class (Monad m) => FilterTree a m where
    -- | Given @pred tree@, produce a 'Tree' that only has items for which
    -- @pred@ returns @True@.
    -- The tree might contain stubs. When expanded, these will be subject to
    -- filtering as well.
    filter :: (AnchoredPath -> TreeItem m -> Bool) -> a m -> a m

instance (Monad m) => FilterTree Tree m where
    filter predicate t_ = filter' t_ (AnchoredPath [])
        where filter' t path = t { items = M.mapMaybeWithKey (wibble path) $ items t }
              wibble path name item =
                  let npath = path `appendPath` name in
                      if predicate npath item
                         then Just $ filterSub npath item
                         else Nothing
              filterSub npath (SubTree t) = SubTree $ filter' t npath
              filterSub npath (Stub stub h) =
                  Stub (do x <- stub
                           return $ filter' x npath) h
              filterSub _ x = x

-- | Given two Trees, a @guide@ and a @tree@, produces a new Tree that is a
-- identical to @tree@, but only has those items that are present in both
-- @tree@ and @guide@. The @guide@ Tree may not contain any stubs.
restrict :: (FilterTree t m) => Tree n -> t m -> t m
restrict guide tree = filter accept tree
    where accept path item =
              case (find guide path, item) of
                (Just (SubTree _), SubTree _) -> True
                (Just (SubTree _), Stub _ _) -> True
                (Just (File _), File _) -> True
                (Just (Stub _ _), _) ->
                    error "*sulk* Go away, you, you precondition violator!"
                (_, _) -> False

-- | Read a Blob into a Lazy ByteString. Might be backed by an mmap, use with
-- care.
readBlob :: Blob m -> m BL.ByteString
readBlob (Blob r _) = r

-- | For every pair of corresponding blobs from the two supplied trees,
-- evaluate the supplied function and accumulate the results in a list. Hint:
-- to get IO actions through, just use sequence on the resulting list.
-- NB. This won't expand any stubs.
zipCommonFiles :: (AnchoredPath -> Blob m -> Blob m -> a) -> Tree m -> Tree m -> [a]
zipCommonFiles f a b = catMaybes [ flip (f p) x `fmap` findFile a p
                                   | (p, File x) <- list b ]

-- | For each file in each of the two supplied trees, evaluate the supplied
-- function (supplying the corresponding file from the other tree, or Nothing)
-- and accumulate the results in a list. Hint: to get IO actions through, just
-- use sequence on the resulting list.  NB. This won't expand any stubs.
zipFiles :: (AnchoredPath -> Maybe (Blob m) -> Maybe (Blob m) -> a)
         -> Tree m -> Tree m -> [a]
zipFiles f a b = [ f p (findFile a p) (findFile b p)
                   | p <- paths a `sortedUnion` paths b ]
    where paths t = sort [ p | (p, File _) <- list t ]

zipTrees :: (AnchoredPath -> Maybe (TreeItem m) -> Maybe (TreeItem m) -> a)
         -> Tree m -> Tree m -> [a]
zipTrees f a b = [ f p (find a p) (find b p)
                   | p <- reverse (paths a `sortedUnion` paths b) ]
    where paths t = sort [ p | (p, _) <- list t ]

-- | Helper function for taking the union of AnchoredPath lists that
-- are already sorted.  This function does not check the precondition
-- so use it carefully.
sortedUnion :: [AnchoredPath] -> [AnchoredPath] -> [AnchoredPath]
sortedUnion [] ys = ys
sortedUnion xs [] = xs
sortedUnion a@(x:xs) b@(y:ys) = case compare x y of
                                LT -> x : sortedUnion xs b
                                EQ -> x : sortedUnion xs ys
                                GT -> y : sortedUnion a ys

-- | Cautiously extracts differing subtrees from a pair of Trees. It will never
-- do any unneccessary expanding. Tree hashes are used to cut the comparison as
-- high up the Tree branches as possible. The result is a pair of trees that do
-- not share any identical subtrees. They are derived from the first and second
-- parameters respectively and they are always fully expanded. It might be
-- advantageous to feed the result into 'zipFiles' or 'zipTrees'.
diffTrees :: forall m. (Monad m) => Tree m -> Tree m -> m (Tree m, Tree m)
diffTrees left right =
            if treeHash left `match` treeHash right
               then return (emptyTree, emptyTree)
               else diff left right
  where isFile (File _) = True
        isFile _ = False
        notFile = not . isFile
        isEmpty = null . listImmediate
        subtree :: TreeItem m -> m (Tree m)
        subtree (Stub x _) = x
        subtree (SubTree x) = return x
        subtree (File _) = error "diffTrees tried to descend a File as a subtree"
        maybeUnfold (Stub x _) = SubTree `fmap` (x >>= expand)
        maybeUnfold (SubTree x) = SubTree `fmap` expand x
        maybeUnfold i = return i
        immediateN t = [ n | (n, _) <- listImmediate t ]
        diff left' right' = do
          is <- sequence [
                   case (lookup left' n, lookup right' n) of
                     (Just l, Nothing) -> do
                       l' <- maybeUnfold l
                       return (n, Just l', Nothing)
                     (Nothing, Just r) -> do
                       r' <- maybeUnfold r
                       return (n, Nothing, Just r')
                     (Just l, Just r)
                         | itemHash l `match` itemHash r ->
                             return (n, Nothing, Nothing)
                         | notFile l && notFile r ->
                             do x <- subtree l
                                y <- subtree r
                                (x', y') <- diffTrees x y
                                if isEmpty x' && isEmpty y'
                                   then return (n, Nothing, Nothing)
                                   else return (n, Just $ SubTree x', Just $ SubTree y')
                         | isFile l && isFile r ->
                             return (n, Just l, Just r)
                         | otherwise ->
                             do l' <- maybeUnfold l
                                r' <- maybeUnfold r
                                return (n, Just l', Just r')
                     _ -> error "n lookups failed"
                   | n <- immediateN left' `union` immediateN right' ]
          let is_l = [ (n, l) | (n, Just l, _) <- is ]
              is_r = [ (n, r) | (n, _, Just r) <- is ]
          return (makeTree is_l, makeTree is_r)

-- | Modify a Tree (by replacing, or removing or adding items).
modifyTree :: (Monad m) => Tree m -> AnchoredPath -> Maybe (TreeItem m) -> Tree m
modifyTree t_ p_ i_ = snd $ go t_ p_ i_
  where fix t unmod items' = (unmod, t { items = (countmap items':: Int) `seq` items'
                                       , treeHash = if unmod then treeHash t else Nothing })

        go t (AnchoredPath []) (Just (SubTree sub)) = (treeHash t `match` treeHash sub, sub)

        go t (AnchoredPath [n]) (Just item) = fix t unmod items'
            where !items' = M.insert n item (items t)
                  !unmod = itemHash item `match` join (fmap itemHash (lookup t n))

        go t (AnchoredPath [n]) Nothing = fix t unmod items'
            where !items' = M.delete n (items t)
                  !unmod = isNothing $ lookup t n

        go t path@(AnchoredPath (n:r)) item = fix t unmod items'
            where subtree s = go s (AnchoredPath r) item
                  !items' = M.insert n sub (items t)
                  !sub = snd sub'
                  !unmod = fst sub'
                  !sub' = case lookup t n of
                    Just (SubTree s) -> let (mod', sub'') = subtree s in (mod', SubTree sub'')
                    Just (Stub s _) -> (False, Stub (do x <- s
                                                        return $! snd $! subtree x) Nothing)
                    Nothing -> (False, SubTree $! snd $! subtree emptyTree)
                    _ -> error $ "Modify tree at " ++ show path

        go _ (AnchoredPath []) (Just (Stub _ _)) =
            error $ "descending in modifyTree, case = (Just (Stub _ _)), path = " ++ show p_
        go _ (AnchoredPath []) (Just (File _)) =
            error $ "descending in modifyTree, case = (Just (File _)), path = " ++ show p_
        go _ (AnchoredPath []) Nothing =
            error $ "descending in modifyTree, case = Nothing, path = " ++ show p_

countmap :: forall a k. M.Map k a -> Int
countmap = M.foldr (\_ i -> i + 1) 0

updateSubtrees :: (Tree m -> Tree m) -> Tree m -> Tree m
updateSubtrees fun t =
    fun $ t { items = M.mapWithKey (curry $ snd . update) $ items t
            , treeHash = Nothing }
  where update (k, SubTree s) = (k, SubTree $ updateSubtrees fun s)
        update (k, File f) = (k, File f)
        update (_, Stub _ _) = error "Stubs not supported in updateTreePostorder"

-- | Does /not/ expand the tree.
updateTree :: (Monad m) => (TreeItem m -> m (TreeItem m)) -> Tree m -> m (Tree m)
updateTree fun t = partiallyUpdateTree fun (\_ _ -> True) t

-- | Does /not/ expand the tree.
partiallyUpdateTree :: (Monad m) => (TreeItem m -> m (TreeItem m))
                       -> (AnchoredPath -> TreeItem m -> Bool) -> Tree m -> m (Tree m)
partiallyUpdateTree fun predi t' = go (AnchoredPath []) t'
  where go path t = do
          items' <- M.fromList <$> mapM (maybeupdate path) (listImmediate t)
          subtree <- fun . SubTree $ t { items = items'
                                       , treeHash = Nothing }
          case subtree of
            SubTree t'' -> return t''
            _ -> error "function passed to partiallyUpdateTree changed SubTree to something else"
        maybeupdate path (k, item) = if predi (path `appendPath` k) item
          then update (path `appendPath` k) (k, item)
          else return (k, item)
        update path (k, SubTree tree) = (\new -> (k, SubTree new)) <$> go path tree
        update    _ (k, item) = (\new -> (k, new)) <$> fun item

-- | Lay one tree over another. The resulting Tree will look like the base (1st
-- parameter) Tree, although any items also present in the overlay Tree will be
-- taken from the overlay. It is not allowed to overlay a different kind of an
-- object, nor it is allowed for the overlay to add new objects to base.  This
-- means that the overlay Tree should be a subset of the base Tree (although
-- any extraneous items will be ignored by the implementation).
overlay :: Applicative m => Tree m -> Tree m -> Tree m
overlay base over = Tree {items = M.fromList immediate, treeHash = Nothing}
  where
    immediate = [(n, get n) | (n, _) <- listImmediate base]
    get n =
      case (M.lookup n $ items base, M.lookup n $ items over) of
        (Just (File _), Just f@(File _)) -> f
        (Just (SubTree b), Just (SubTree o)) -> SubTree $ overlay b o
        (Just (Stub b _), Just (SubTree o)) -> Stub (overlay <$> b <*> pure o) Nothing
        (Just (SubTree b), Just (Stub o _)) -> Stub (overlay <$> pure b <*> o) Nothing
        (Just (Stub b _), Just (Stub o _)) -> Stub (overlay <$> b <*> o) Nothing
        (Just x, _) -> x
        (_, _) -> error $ "Unexpected case in overlay at get " ++ show n ++ "."

-- | Calculate and insert hashes for all 'TreeItem's contained in a 'Tree',
-- including the argument 'Tree' itself. If necessary, this expands 'Stub's.
addMissingHashes :: (Monad m) => (TreeItem m -> m Hash) -> Tree m -> m (Tree m)
addMissingHashes make = updateTree update
    where update item@(SubTree t) = do
              hash <- make item
              return $ SubTree (t { treeHash = Just hash })
          update item@(File (Blob con Nothing)) = do
              hash <- make item
              return $ File (Blob con (Just hash))
          update (Stub s Nothing) = update . SubTree =<< s
          update x = return x

-- ---- Private utilities shared among multiple functions. --------

unstub :: (Monad m) => TreeItem m -> m (Tree m)
unstub (Stub s _) = s
unstub (SubTree s) = return s
unstub _ = return emptyTree

-- Properties

-- | Specification of 'explodePath'
prop_explodePath :: Tree m -> AnchoredPath -> Bool
prop_explodePath t p =
  explodePath t p == Prelude.filter (isPrefix p) (map fst (list t))
