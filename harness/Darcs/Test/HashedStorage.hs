{-# OPTIONS_GHC -Wno-orphans #-}
module Darcs.Test.HashedStorage ( tests, unsafeMakeName ) where

import Prelude hiding ( filter, readFile, writeFile, lookup, (<$>) )
import qualified Prelude
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString.Char8 as BC
import System.Directory( doesFileExist, removeFile )
import Control.Monad ( when, forM_ )
import Control.Monad.Identity ( Identity, runIdentity )
import Control.Monad.Trans( lift )
import Control.Applicative( (<$>) )
import Codec.Archive.Zip( extractFilesFromArchive, toArchive )

import Data.Maybe
import Data.List( sort, intercalate, intersperse )

import Darcs.Repository.Inventory.Format ( peekPristineHash )
import Darcs.Repository.Paths ( hashedInventoryPath )

import Darcs.Util.Cache ( mkRepoCache )
import Darcs.Util.Path hiding ( setCurrentDirectory )
import Darcs.Util.Lock ( withPermDir )
import Darcs.Util.Tree hiding ( lookup )
import Darcs.Util.Index
import Darcs.Util.Tree.Hashed
import Darcs.Util.Hash
import Darcs.Util.Tree.Monad hiding ( tree, createDirectory )
import Darcs.Util.Tree.Plain

import System.Mem( performGC )

import qualified Data.Set as S

import Test.HUnit hiding ( path )
import Test.Framework( testGroup )
import qualified Test.Framework as TF ( Test )
import Test.QuickCheck
import Test.QuickCheck.Instances.ByteString ()

import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

------------------------
-- Test Data
--

blobs :: [(AnchoredPath, BLC.ByteString)]
blobs = [ (unsafeFloatPath "foo_a", BLC.pack "a\n")
        , (unsafeFloatPath "foo_dir/foo_a", BLC.pack "a\n")
        , (unsafeFloatPath "foo_dir/foo_b", BLC.pack "b\n")
        , (unsafeFloatPath "foo_dir/foo_subdir/foo_a", BLC.pack "a\n")
        , (unsafeFloatPath "foo space/foo\nnewline", BLC.pack "newline\n")
        , (unsafeFloatPath "foo space/foo\\backslash", BLC.pack "backslash\n")
        , (unsafeFloatPath "foo space/foo_a", BLC.pack "a\n") ]

files :: [AnchoredPath]
files = map fst blobs

dirs :: [AnchoredPath]
dirs = [ unsafeFloatPath "foo_dir"
       , unsafeFloatPath "foo_dir/foo_subdir"
       , unsafeFloatPath "foo space" ]

emptyStub :: TreeItem IO
emptyStub = Stub (return emptyTree) Nothing

unsafeMakeName :: String -> Name
unsafeMakeName = either error id . makeName

testTree :: Tree IO
testTree =
    makeTree [ (unsafeMakeName "foo", emptyStub)
             , (unsafeMakeName "subtree", SubTree sub)
             , (unsafeMakeName "substub", Stub getsub Nothing) ]
    where sub = makeTree [ (unsafeMakeName "stub", emptyStub)
                         , (unsafeMakeName "substub", Stub getsub2 Nothing)
                         , (unsafeMakeName "x", SubTree emptyTree) ]
          getsub = return sub
          getsub2 = return $ makeTree [ (unsafeMakeName "file", File emptyBlob)
                                      , (unsafeMakeName "file2",
                                         File $ Blob (return $ BLC.pack "foo") Nothing) ]

equals_testdata :: Tree IO -> IO ()
equals_testdata t = sequence_ [
                     do isJust (findFile t p) @? show p ++ " in tree"
                        ours <- readBlob (fromJust $ findFile t p)
                        ours @?= stored
                     | (p, stored) <- blobs ] >>
                    sequence_ [ isJust (Prelude.lookup p blobs) @? show p ++ " extra in tree"
                                | (p, File _) <- list t ]

---------------------------
-- Test list
--

tests :: [TF.Test]
tests = [ testGroup "Darcs.Util.Hash" hash
        , testGroup "Darcs.Util.Tree" tree
        , testGroup "Darcs.Util.Index" index
        , testGroup "Darcs.Util.Tree.Monad" monad
        , testGroup "Hashed Storage" hashed ]

--------------------------
-- Tests
--

hashed :: [TF.Test]
hashed = [ testCase "plain has all files" have_files
         , testCase "pristine has all files" have_pristine_files
         , testCase "pristine has no extras" pristine_no_extra
         , testCase "pristine file contents match" pristine_contents
         , testCase "plain file contents match" plain_contents
         , testCase "writePlainTree works" write_plain ]
    where
      check_file t f = assertBool
                       ("path " ++ show f ++ " is missing in tree " ++ show t)
                       (isJust $ find t f)
      check_files = forM_ files . check_file

      pristine_no_extra = extractRepoAndRun $
       do
        t <- readDarcsPristine "." >>= expand
        forM_ (list t) $ \(path,_) -> assertBool (show path ++ " is extraneous in tree")
                                                 (path `elem` (dirs ++ files))
      have_files = extractRepoAndRun ( readPlainTree "." >>= expand >>= check_files )
      have_pristine_files = extractRepoAndRun ( readDarcsPristine "." >>= expand >>= check_files )

      pristine_contents = extractRepoAndRun $
       do
        t <- readDarcsPristine "." >>= expand
        equals_testdata t

      plain_contents = extractRepoAndRun $
       do
        t <- expand =<< filter nondarcs `fmap` readPlainTree "."
        equals_testdata t

      write_plain = extractRepoAndRun $
       do
        orig <- readDarcsPristine "." >>= expand
        writePlainTree orig "_darcs/plain"
        t <- expand =<< readPlainTree "_darcs/plain"
        equals_testdata t

index :: [TF.Test]
index = [ testCase "index versioning" check_index_versions
        , testCase "index listing" check_index
        , testCase "index content" check_index_content
        , testProperty "align bounded" prop_align_bounded
        , testProperty "align aligned" prop_align_aligned ]
    where pristine = readDarcsPristine "." >>= expand
          build_index =
            do x <- pristine
               exist <- doesFileExist "_darcs/index"
               performGC -- required in win32 to trigger file close
               when exist $ removeFile "_darcs/index"
               idx <- treeFromIndex =<< updateIndexFrom "_darcs/index" x
               return (x, idx)
          check_index = extractRepoAndRun $
            do (pris, idx) <- build_index
               (sort $ map fst $ list idx) @?= (sort $ map fst $ list pris)
          check_blob_pair p x y =
              do a <- readBlob x
                 b <- readBlob y
                 assertEqual ("content match on " ++ show p) a b
          check_index_content = extractRepoAndRun $
            do (_, idx) <- build_index
               plain <- readPlainTree "."
               x <- sequence $ zipCommonFiles check_blob_pair plain idx
               assertBool "files match" (length x > 0)
          check_index_versions = extractRepoAndRun $
            do performGC -- required in win32 to trigger file close
               Prelude.writeFile "_darcs/index" "nonsense index... do not crash!"
               valid <- indexFormatValid "_darcs/index"
               assertBool "index format invalid" $ not valid
          prop_align_bounded (bound, x) =
              bound > 0 && bound < 1024 && x >= 0 ==>
                    align bound x >= x && align bound x < x + bound
              where _types = (bound, x) :: (Int, Int)
          prop_align_aligned (bound, x) =
              bound > 0 && bound < 1024 && x >= 0 ==>
                    align bound x `rem` bound == 0
              where _types = (bound, x) :: (Int, Int)

tree :: [TF.Test]
tree = [ testCase "modifyTree" check_modify
       , testCase "complex modifyTree" check_modify_complex
       , testCase "modifyTree removal" check_modify_remove
       , testCase "expand" check_expand
       , testCase "expandPath" check_expand_path
       , testCase "expandPath of sub" check_expand_path_sub
       , testCase "diffTrees" check_diffTrees
       , testCase "diffTrees identical" check_diffTrees_ident
       , testProperty "expandPath" prop_expandPath
       , testProperty "shapeEq" prop_shape_eq
       , testProperty "expandedShapeEq" prop_expanded_shape_eq
       , testProperty "expand is identity" prop_expand_id
       , testProperty "filter True is identity" prop_filter_id
       , testProperty "filter False is empty" prop_filter_empty
       , testProperty "restrict both ways keeps shape" prop_restrict_shape_commutative
       , testProperty "restrict is a subtree of both" prop_restrict_subtree
       , testProperty "overlay keeps shape" prop_overlay_shape
       , testProperty "overlay is superset of over" prop_overlay_super ]
    where blob x = File $ Blob (return (BLC.pack x)) (Just $ sha256 $ BLC.pack x)
          name = unsafeMakeName
          check_modify =
              let t = makeTree [(name "foo", blob "bar")]
                  modify = modifyTree t (unsafeFloatPath "foo") (Just $ blob "bla")
               in do x <- readBlob $ fromJust $ findFile t (unsafeFloatPath "foo")
                     y <- readBlob $ fromJust $ findFile modify (unsafeFloatPath "foo")
                     assertEqual "old version" x (BLC.pack "bar")
                     assertEqual "new version" y (BLC.pack "bla")
                     assertBool "list has foo" $
                                isJust (Prelude.lookup (unsafeFloatPath "foo") $ list modify)
                     length (list modify) @?= 1
          check_modify_complex =
              let t = makeTree [ (name "foo", blob "bar")
                               , (name "bar", SubTree t1) ]
                  t1 = makeTree [ (name "foo", blob "bar") ]
                  modify = modifyTree t (unsafeFloatPath "bar/foo") (Just $ blob "bla")
               in do foo <- readBlob $ fromJust $ findFile t (unsafeFloatPath "foo")
                     foo' <- readBlob $ fromJust $ findFile modify (unsafeFloatPath "foo")
                     bar_foo <- readBlob $ fromJust $
                                findFile t (unsafeFloatPath "bar/foo")
                     bar_foo' <- readBlob $ fromJust $
                                 findFile modify (unsafeFloatPath "bar/foo")
                     assertEqual "old foo" foo (BLC.pack "bar")
                     assertEqual "old bar/foo" bar_foo (BLC.pack "bar")
                     assertEqual "new foo" foo' (BLC.pack "bar")
                     assertEqual "new bar/foo" bar_foo' (BLC.pack "bla")
                     assertBool "list has bar/foo" $
                                isJust (Prelude.lookup (unsafeFloatPath "bar/foo") $ list modify)
                     assertBool "list has foo" $
                                isJust (Prelude.lookup (unsafeFloatPath "foo") $ list modify)
                     length (list modify) @?= length (list t)
          check_modify_remove =
              let t1 = makeTree [(name "foo", blob "bar")]
                  t2 :: Tree Identity = makeTree [ (name "foo", blob "bar")
                                                 , (name "bar", SubTree t1) ]
                  modify1 = modifyTree t1 (unsafeFloatPath "foo") Nothing
                  modify2 = modifyTree t2 (unsafeFloatPath "bar") Nothing
                  file = findFile modify1 (unsafeFloatPath "foo")
                  subtree = findTree modify2 (unsafeFloatPath "bar")
               in do assertBool "file is gone" (isNothing file)
                     assertBool "subtree is gone" (isNothing subtree)

          no_stubs t = null [ () | (_, Stub _ _) <- list t ]
          path = unsafeFloatPath "substub/substub/file"
          badpath = unsafeFloatPath "substub/substub/foo"
          check_expand = do
            x <- expand testTree
            assertBool "no stubs in testTree" $ not (no_stubs testTree)
            assertBool "stubs in expanded tree" $ no_stubs x
            assertBool "path reachable" $ path `elem` (map fst $ list x)
            assertBool "badpath not reachable" $
                       badpath `notElem` (map fst $ list x)
          check_expand_path = do
            test_exp <- expand testTree
            t <- expandPath testTree path
            t' <- expandPath test_exp path
            t'' <- expandPath testTree $ unsafeFloatPath "substub/x"
            assertBool "path not reachable in testTree" $ path `notElem` (map fst $ list testTree)
            assertBool "path reachable in t" $ path `elem` (map fst $ list t)
            assertBool "path reachable in t'" $ path `elem` (map fst $ list t')
            assertBool "path reachable in t (with findFile)" $
                       isJust $ findFile t path
            assertBool "path reachable in t' (with findFile)" $
                       isJust $ findFile t' path
            assertBool "path not reachable in t''" $ path `notElem` (map fst $ list t'')
            assertBool "badpath not reachable in t" $
                       badpath `notElem` (map fst $ list t)
            assertBool "badpath not reachable in t'" $
                       badpath `notElem` (map fst $ list t')

          check_expand_path_sub = do
            t <- expandPath testTree $ unsafeFloatPath "substub"
            t' <- expandPath testTree $ unsafeFloatPath "substub/stub"
            t'' <- expandPath testTree $ unsafeFloatPath "subtree/stub"
            assertBool "leaf is not a Stub" $
                isNothing (findTree testTree $ unsafeFloatPath "substub")
            assertBool "leaf is not a Stub" $ isJust (findTree t $ unsafeFloatPath "substub")
            assertBool "leaf is not a Stub (2)" $ isJust (findTree t' $ unsafeFloatPath "substub/stub")
            assertBool "leaf is not a Stub (3)" $ isJust (findTree t'' $ unsafeFloatPath "subtree/stub")

          check_diffTrees = extractRepoAndRun $
                 do Prelude.writeFile "foo_dir/foo_a" "b\n"
                    working_plain <- filter nondarcs `fmap` readPlainTree "."
                    working <- treeFromIndex =<<
                                 updateIndexFrom "_darcs/index" working_plain
                    pristine <- readDarcsPristine "."
                    (working', pristine') <- diffTrees working pristine
                    let foo_work = findFile working' (unsafeFloatPath "foo_dir/foo_a")
                        foo_pris = findFile pristine' (unsafeFloatPath "foo_dir/foo_a")
                    working' `shapeEq` pristine'
                             @? show working' ++ " `shapeEq` " ++ show pristine'
                    assertBool "foo_dir/foo_a is in working'" $ isJust foo_work
                    assertBool "foo_dir/foo_a is in pristine'" $ isJust foo_pris
                    foo_work_c <- readBlob (fromJust foo_work)
                    foo_pris_c <- readBlob (fromJust foo_pris)
                    BLC.unpack foo_work_c @?= "b\n"
                    BLC.unpack foo_pris_c @?= "a\n"
                    assertEqual "working' tree is minimal" 2 (length $ list working')
                    assertEqual "pristine' tree is minimal" 2 (length $ list pristine')

          check_diffTrees_ident = do
            pristine <- readDarcsPristine "."
            (t1, t2) <- diffTrees pristine pristine
            assertBool "t1 is empty" $ null (list t1)
            assertBool "t2 is empty" $ null (list t2)

          prop_shape_eq x = no_stubs x ==> x `shapeEq` x
              where _types = x :: Tree Identity
          prop_expanded_shape_eq x = runIdentity $ expandedShapeEq x x
              where _types = x :: Tree Identity
          prop_expand_id x = no_stubs x ==> runIdentity (expand x) `shapeEq` x
              where _types = x :: Tree Identity
          prop_filter_id x = runIdentity $ expandedShapeEq x $ filter (\_ _ -> True) x
              where _types = x :: Tree Identity
          prop_filter_empty x = runIdentity $ expandedShapeEq emptyTree $ filter (\_ _ -> False) x
              where _types = x :: Tree Identity
          prop_restrict_shape_commutative (t1, t2) =
              no_stubs t1 && no_stubs t2 && not (restrict t1 t2 `shapeEq` emptyTree) ==>
                  restrict t1 t2 `shapeEq` restrict t2 t1
              where _types = (t1 :: Tree Identity, t2 :: Tree Identity)
          prop_restrict_subtree (t1, t2) =
              no_stubs t1 && not (restrict t1 t2 `shapeEq` emptyTree) ==>
                  let restricted = S.fromList (map fst $ list $ restrict t1 t2)
                      orig1 = S.fromList (map fst $ list t1)
                      orig2 = S.fromList (map fst $ list t2)
                   in and [restricted `S.isSubsetOf` orig1, restricted `S.isSubsetOf` orig2]
              where _types = (t1 :: Tree Identity, t2 :: Tree Identity)
          prop_overlay_shape (t1 :: Tree Identity, t2) =
              (Just LT == runIdentity (t2 `cmpExpandedShape` t1)) ==>
              runIdentity $ (t1 `overlay` t2) `expandedShapeEq` t1
          prop_overlay_super (t1 :: Tree Identity, t2) =
              (Just LT == runIdentity (t2 `cmpExpandedShape` t1)) && no_stubs t2 ==>
              Just EQ == (runIdentity $ restrict t2 (t1 `overlay` t2) `cmpTree` t2)
          prop_expandPath (TreeWithPath t p) =
              notStub $ find (runIdentity $ expandPath t p) p
            where notStub (Just (Stub _ _)) = False
                  notStub Nothing = error "Did not exist."
                  notStub _ = True

hash :: [TF.Test]
hash = [ testProperty "decodeBase16 . encodeBase16 == Just" prop_base16 ]
    where prop_base16 x = (decodeBase16 . encodeBase16) x == Just x

monad :: [TF.Test]
monad = [ testCase "path expansion" check_virtual
        , testCase "rename" check_rename ]
    where check_virtual = virtualTreeMonad run testTree >> return ()
              where run = do file <- readFile (unsafeFloatPath "substub/substub/file")
                             file2 <- readFile (unsafeFloatPath "substub/substub/file2")
                             lift $ BLC.unpack file @?= ""
                             lift $ BLC.unpack file2 @?= "foo"
          check_rename = do (_, t) <- virtualTreeMonad run testTree
                            t' <- darcsAddMissingHashes =<< expand t
                            forM_ [ (p, i) | (p, i) <- list t' ] $ \(p,i) ->
                               assertBool ("have hash: " ++ show p) $ itemHash i /= Nothing
              where run = do rename (unsafeFloatPath "substub/substub/file") (unsafeFloatPath "substub/file2")

----------------------------------
-- Arbitrary instances
--

instance Arbitrary Hash where
    arbitrary = mkHash . BC.pack <$> sequence (replicate 32 arbitrary)

instance (Monad m) => Arbitrary (TreeItem m) where
  arbitrary = sized tree'
    where tree' 0 = oneof [ return (File emptyBlob), return (SubTree emptyTree) ]
          tree' n = oneof [ file n, subtree n ]
          file 0 = return (File emptyBlob)
          file _ = do content <- arbitrary
                      return (File $ Blob (return content) Nothing)
          subtree n = do branches <- choose (1, n)
                         let sub name = do t <- tree' ((n - 1) `div` branches)
                                           return (unsafeMakeName $ show name, t)
                         sublist <- mapM sub [0..branches]
                         oneof [ tree' 0
                               , return (SubTree $ makeTree sublist)
                               , return $ (Stub $ return (makeTree sublist)) Nothing ]

instance (Monad m) => Arbitrary (Tree m) where
  arbitrary = do item <- arbitrary
                 case item of
                   File _ -> arbitrary
                   Stub _ _ -> arbitrary
                   SubTree t -> return t

data TreeWithPath = TreeWithPath (Tree Identity) AnchoredPath deriving (Show)

instance Arbitrary TreeWithPath where
  arbitrary = do t <- arbitrary
                 p <- oneof $ return (AnchoredPath []) :
                             (map (return . fst) $ list (runIdentity $ expand t))
                 return $ TreeWithPath t p

---------------------------
-- Other instances
--

instance Show (Blob m) where
    show (Blob _ h) = "Blob " ++ show h

instance Show (TreeItem m) where
    show (File f) = "File (" ++ show f ++ ")"
    show (Stub _ h) = "Stub _ " ++ show h
    show (SubTree s) = "SubTree (" ++ show s ++ ")"

instance Show (Tree m) where
    show t = "Tree " ++ show (treeHash t) ++ " { " ++
             (concat . intersperse ", " $ itemstrs) ++ " }"
        where itemstrs = map show $ listImmediate t

instance Show (Int -> Int) where
    show f = "[" ++ intercalate ", " (map val [1..20]) ++ " ...]"
        where val x = show x ++ " -> " ++ show (f x)

-----------------------
-- Test utilities
--

shapeEq :: Tree m -> Tree m -> Bool
shapeEq a b = Just EQ == cmpShape a b

expandedShapeEq :: Monad m => Tree m -> Tree m -> m Bool
expandedShapeEq a b = (Just EQ ==) <$> cmpExpandedShape a b

cmpcat :: [Maybe Ordering] -> Maybe Ordering
cmpcat (x:y:rest) | x == y = cmpcat (x:rest)
                  | x == Just EQ = cmpcat (y:rest)
                  | y == Just EQ = cmpcat (x:rest)
                  | otherwise = Nothing
cmpcat [x] = x
cmpcat [] = Just EQ -- empty things are equal

cmpTree :: Monad m => Tree m -> Tree m -> m (Maybe Ordering)
cmpTree x y = do x' <- expand x
                 y' <- expand y
                 con <- contentsEq x' y'
                 return $ cmpcat [cmpShape x' y', con]
    where contentsEq a b = cmpcat <$> sequence (zipTrees cmp a b)
          cmp _ (Just (File a)) (Just (File b)) = do a' <- readBlob a
                                                     b' <- readBlob b
                                                     return $ Just (compare a' b')
          cmp _ _ _ = return (Just EQ) -- neutral

cmpShape :: Tree m -> Tree m -> Maybe Ordering
cmpShape t r = cmpcat $ zipTrees cmp t r
    where cmp _ (Just a) (Just b) = a `item` b
          cmp _ Nothing (Just _) = Just LT
          cmp _ (Just _) Nothing = Just GT
          cmp _ Nothing Nothing = Just EQ
          item (File _) (File _) = Just EQ
          item (SubTree s) (SubTree p) = s `cmpShape` p
          item _ _ = Nothing

cmpExpandedShape :: (Monad m) => Tree m -> Tree m -> m (Maybe Ordering)
cmpExpandedShape a b = do x <- expand a
                          y <- expand b
                          return $ x `cmpShape` y

nondarcs :: AnchoredPath -> TreeItem m -> Bool
nondarcs (AnchoredPath (x:_)) _ | x == unsafeMakeName "_darcs" = False
                                     | otherwise = True
nondarcs (AnchoredPath []) _ = True

readDarcsPristine :: FilePath -> IO (Tree IO)
readDarcsPristine dir = do
  ph <- peekPristineHash <$> BC.readFile hashedInventoryPath
  readDarcsHashed (mkRepoCache dir) ph

extractRepoAndRun :: IO a -> IO a
extractRepoAndRun action = do
  zipFile <- toArchive . BLC.fromStrict <$> BC.readFile "harness/hstestdata.zip"
  withPermDir "_test_playground" $ \_ -> do
    extractFilesFromArchive [] zipFile
    action
