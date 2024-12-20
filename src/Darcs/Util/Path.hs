-- Copyright (C) 2007 Eric Kow
-- Copyright (C) 2010 Petr Rockai
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

{-# LANGUAGE CPP #-}
module Darcs.Util.Path
    ( encodeWhite
    , decodeWhite
    , encodeWhiteName
    , decodeWhiteName
    -- * AbsolutePath
    , AbsolutePath
    , makeAbsolute
    , ioAbsolute
    -- * AbsolutePathOrStd
    , AbsolutePathOrStd
    , makeAbsoluteOrStd
    , ioAbsoluteOrStd
    , useAbsoluteOrStd
    , stdOut
    -- * AbsoluteOrRemotePath
    , AbsoluteOrRemotePath
    , ioAbsoluteOrRemote
    , isRemote
    -- * SubPath
    , SubPath
    , makeSubPathOf
    , simpleSubPath
    , floatSubPath
    , makeRelativeTo
    -- * Miscellaneous
    , FilePathOrURL(..)
    , FilePathLike(toFilePath)
    , getCurrentDirectory
    , setCurrentDirectory
    , getUniquePathName
    -- * Tree filtering.
    , filterPaths
    -- * AnchoredPaths: relative paths within a Tree. All paths are
    -- anchored at a certain root (this is usually the Tree root). They are
    -- represented by a list of Names (these are just strict bytestrings).
    , Name
    , makeName
    , rawMakeName
    , eqAnycase
    , AnchoredPath(..)
    , anchoredRoot
    , appendPath
    , anchorPath
    , isPrefix
    , movedirfilename
    , parent
    , parents
    , replaceParent
    , catPaths
    , flatten
    , inDarcsdir
    , displayPath
    , realPath
    , isRoot
    , darcsdirName
    , floatPath
    -- * Unsafe AnchoredPath functions.
    , unsafeFloatPath
    ) where

import Darcs.Prelude

import Control.Exception ( bracket_ )
import Control.Monad ( when, (<=<) )
import Darcs.Util.ByteString ( decodeLocale, encodeLocale )
import Data.Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Short as BS
import Data.Char ( chr, isSpace, ord, toLower )
import Data.List ( inits, isPrefixOf, isSuffixOf, stripPrefix )
import GHC.Base ( unsafeChr )
import GHC.Stack ( HasCallStack )
import qualified System.Directory ( setCurrentDirectory )
import System.Directory ( doesDirectoryExist, doesPathExist )
import qualified System.FilePath as NativeFilePath
import qualified System.FilePath.Posix as FilePath
import System.Posix.Files ( fileID, getFileStatus, isDirectory )

import Darcs.Util.Exception ( ifDoesNotExistError )
import Darcs.Util.Global ( darcsdir )
import Darcs.Util.URL ( isAbsolute, isHttpUrl, isRelative, isSshNopath, isSshUrl )
import qualified Darcs.Util.Workaround as Workaround ( getCurrentDirectory )


-- Utilities for use by command implementations

-- | For displaying paths to the user. It should never be used
-- for on-disk patch storage. This adds the "./" for consistency
-- with how repo paths are displayed by 'showPatch' and friends,
-- except for the root path which is displayed as plain ".".
displayPath :: AnchoredPath -> FilePath
displayPath p
  | isRoot p = "."
  | otherwise = anchorPath "." p

-- | Interpret an 'AnchoredPath' as relative the current working
-- directory. Intended for IO operations in the file system.
-- This returns the empty string for the root, so use with care!
realPath :: AnchoredPath -> FilePath
realPath = anchorPath ""

-- | Encode whitespace and backslashes in filenames to a darcs-specific
-- format (numerical representation according to 'ord' surrounded by
-- backslashes).
--
--   > encodeWhite "hello there" == "hello\32\there"
--   > encodeWhite "hello\there" == "hello\92\there"
encodeWhite :: FilePath -> String
encodeWhite = foldr encodesWhiteChar [] where
  encodesWhiteChar c acc
    | isSpace c || c == '\\' = '\\' : show (ord c) ++ '\\' : acc
    | otherwise = c : acc

-- | Decode filenames from the darcs-specific encoding produced by
-- 'encodeWhite'.
--
--   > decodeWhite "hello\32\there"  == Right "hello there"
--   > decodeWhite "hello\92\there"  == Right "hello\there"
--   > decodeWhite "hello\there"   == Left "malformed filename"
decodeWhite :: String -> Either String FilePath
decodeWhite s = go s where
  go [] = return []
  go (c:cs)
    | c == '\\' =
      case break (== '\\') cs of
        (theord, '\\':rest) -> (chr (read theord) :) <$> go rest
        _ -> Left $ "malformed filename: " ++ s
    | otherwise = (c :) <$> go cs

class FilePathOrURL a where
  toPath :: a -> String

class FilePathOrURL a => FilePathLike a where
  toFilePath :: a -> FilePath

-- | Paths which are relative to the local darcs repository and normalized.
-- Note: These are understood not to have the dot in front.
newtype SubPath      = SubPath FilePath deriving (Eq, Ord)

newtype AbsolutePath = AbsolutePath FilePath deriving (Eq, Ord)

-- | This is for situations where a string (e.g. a command line argument)
-- may take the value \"-\" to mean stdin or stdout (which one depends on
-- context) instead of a normal file path.
data AbsolutePathOrStd = AP AbsolutePath | APStd deriving (Eq, Ord)
data AbsoluteOrRemotePath = AbsP AbsolutePath | RmtP String deriving (Eq, Ord)

instance FilePathOrURL AbsolutePath where
  toPath (AbsolutePath x) = x
instance FilePathOrURL SubPath where
  toPath (SubPath x) = x
instance FilePathOrURL AbsoluteOrRemotePath where
  toPath (AbsP a) = toPath a
  toPath (RmtP r) = r
instance FilePathOrURL FilePath where
  toPath = id

instance FilePathLike AbsolutePath where
  toFilePath (AbsolutePath x) = x
instance FilePathLike SubPath where
  toFilePath (SubPath x) = x
instance FilePathLike FilePath where
  toFilePath = id

-- | Make the second path relative to the first, if possible.
-- Note that this returns an empty 'SubPath' if the inputs are equal.
makeSubPathOf :: AbsolutePath -> AbsolutePath -> Maybe SubPath
makeSubPathOf (AbsolutePath p1) (AbsolutePath p2) =
 -- The slash prevents "foobar" from being treated as relative to "foo"
 if p1 == p2 || (p1 ++ "/") `isPrefixOf` p2
    then Just $ SubPath $ drop (length p1 + 1) p2
    else Nothing

simpleSubPath :: HasCallStack => FilePath -> Maybe SubPath
simpleSubPath x | null x = error "simpleSubPath called with empty path"
                | isRelative x = Just $ SubPath $ FilePath.normalise $ pathToPosix x
                | otherwise = Nothing

-- | Interpret a possibly relative path wrt the current working directory.
-- This also canonicalizes the path, resolving symbolic links etc.
ioAbsolute :: FilePath -> IO AbsolutePath
ioAbsolute dir =
    do isdir <- doesDirectoryExist dir
       here <- getCurrentDirectory
       if isdir
         then bracket_ (setCurrentDirectory dir)
                       (setCurrentDirectory $ toFilePath here)
                       getCurrentDirectory
         else let super_dir = case NativeFilePath.takeDirectory dir of
                                "" ->  "."
                                d  -> d
                  file = NativeFilePath.takeFileName dir
              in do abs_dir <- if dir == super_dir
                               then return $ AbsolutePath dir
                               else ioAbsolute super_dir
                    return $ makeAbsolute abs_dir file

-- | The first argument must be the absolute path of a @directory@, the second
-- is an arbitrary absolute @path@. Find the longest prefix of @path@ that
-- points to the same @directory@; if there is none, return 'Nothing', else
-- return 'Just' the remainder.
makeRelativeTo :: HasCallStack => AbsolutePath -> AbsolutePath -> IO (Maybe SubPath)
makeRelativeTo (AbsolutePath dir) (AbsolutePath path) = do
  dir_stat <- getFileStatus dir
  let dir_id = fileID dir_stat
  when (not (isDirectory dir_stat)) $
    error $ "makeRelativeTo called with non-dir " ++ dir
  findParent dir_id path []
  where
    findParent dir_id ap acc = do
      map_stat <- ifDoesNotExistError Nothing (Just <$> getFileStatus ap)
      case map_stat of
        Just ap_stat | fileID ap_stat == dir_id -> do
          -- found ancestor that matches dir
          return $ Just $ SubPath $ FilePath.joinPath acc
        _ -> do
          -- recurse
          let (parent_,child) =
                -- splitFileName only does what one expects if there is no
                -- trailing path separator
                NativeFilePath.splitFileName $
                  NativeFilePath.dropTrailingPathSeparator ap
          if null child then
            return Nothing
          else
            findParent dir_id parent_ (child:acc)

-- | Take an absolute path and a string representing a (possibly relative)
-- path and combine them into an absolute path. If the second argument is
-- already absolute, then the first argument gets ignored. This function also
-- takes care that the result is converted to Posix convention and
-- normalized. Also, parent directories (\"..\") at the front of the string
-- argument get canceled out against trailing directory parts of the
-- absolute path argument.
--
-- Regarding the last point, someone more familiar with how these functions
-- are used should verify that this is indeed necessary or at least useful.
makeAbsolute :: AbsolutePath -> FilePath -> AbsolutePath
makeAbsolute a dir = if not (null dir) && isAbsolute dir
                     then AbsolutePath (normSlashes dir')
                     else ma a dir'
  where
    dir' = FilePath.normalise $ pathToPosix dir
    -- Why do we care to reduce ".." here?
    -- Why not do this throughout the whole path, i.e. "x/y/../z" -> "x/z" ?
    ma here ('.':'.':'/':r) = ma (takeDirectory here) r
    ma here ".." = takeDirectory here
    ma here "." = here
    ma here "" = here
    ma here r = here /- ('/':r)

(/-) :: AbsolutePath -> String -> AbsolutePath
x /- ('/':r) = x /- r
(AbsolutePath "/") /- r = AbsolutePath ('/':simpleClean r)
(AbsolutePath x) /- r = AbsolutePath (x++'/':simpleClean r)

-- | Convert to posix, remove trailing slashes, and (under Posix)
-- reduce multiple leading slashes to one.
simpleClean :: String -> String
simpleClean = normSlashes . reverse . dropWhile (=='/') . reverse . pathToPosix

makeAbsoluteOrStd :: AbsolutePath -> String -> AbsolutePathOrStd
makeAbsoluteOrStd _ "-" = APStd
makeAbsoluteOrStd a p = AP $ makeAbsolute a p

stdOut :: AbsolutePathOrStd
stdOut = APStd

ioAbsoluteOrStd :: String -> IO AbsolutePathOrStd
ioAbsoluteOrStd "-" = return APStd
ioAbsoluteOrStd p = AP `fmap` ioAbsolute p

-- | Execute either the first or the second argument action, depending on
-- whether the given path is an 'AbsolutePath' or stdin/stdout.
useAbsoluteOrStd :: (AbsolutePath -> a) -> a -> AbsolutePathOrStd -> a
useAbsoluteOrStd _ f APStd = f
useAbsoluteOrStd f _ (AP x) = f x

ioAbsoluteOrRemote :: String -> IO AbsoluteOrRemotePath
ioAbsoluteOrRemote p = do
  isdir <- doesDirectoryExist p
  if not isdir
     then return $ RmtP $
          case () of _ | isSshNopath p    -> p++"."
                       | "/" `isSuffixOf` p -> init p
                       | otherwise          -> p
     else AbsP `fmap` ioAbsolute p

isRemote :: AbsoluteOrRemotePath -> Bool
isRemote (RmtP _) = True
isRemote _ = False

takeDirectory :: AbsolutePath -> AbsolutePath
takeDirectory (AbsolutePath x) =
    case reverse $ drop 1 $ dropWhile (/='/') $ reverse x of
    "" -> AbsolutePath "/"
    x' -> AbsolutePath x'

instance Show AbsolutePath where
 show = show . toFilePath
instance Show SubPath where
 show = show . toFilePath
instance Show AbsolutePathOrStd where
    show (AP a) = show a
    show APStd = "standard input/output"
instance Show AbsoluteOrRemotePath where
    show (AbsP a) = show a
    show (RmtP r) = show r

-- | Normalize the path separator to Posix style (slash, not backslash).
-- This only affects Windows systems.
pathToPosix :: FilePath -> FilePath
#ifdef WIN32
pathToPosix = map convert where
  convert '\\' = '/'
  convert c = c
#else
pathToPosix = id
#endif

-- | Reduce multiple leading slashes to one. This only affects Posix systems.
normSlashes :: FilePath -> FilePath
#ifndef WIN32
-- multiple slashes in front are ignored under Posix
normSlashes ('/':p) = '/' : dropWhile (== '/') p
#endif
normSlashes p = p

getCurrentDirectory :: IO AbsolutePath
getCurrentDirectory = AbsolutePath `fmap` Workaround.getCurrentDirectory

setCurrentDirectory :: HasCallStack => FilePathLike p => p -> IO ()
setCurrentDirectory path
  | isHttpUrl (toFilePath path) || isSshUrl (toFilePath path) =
    error $ "setCurrentDirectory " ++ toFilePath path
setCurrentDirectory path = System.Directory.setCurrentDirectory (toFilePath path)

-- | Iteratively tries find first non-existing path generated by
-- buildName, it feeds to buildName the number starting with -1.  When
-- it generates non-existing path and it isn't first, it displays the
-- message created with buildMsg. Usually used for generation of the
-- name like <path>_<number> when <path> already exist
-- (e.g. darcs.net_0).
getUniquePathName :: Bool -> (FilePath -> String) -> (Int -> FilePath) -> IO FilePath
getUniquePathName talkative buildMsg buildName = go (-1)
 where
  go :: Int -> IO FilePath
  go i = do
    exists <- doesPathExist thename
    if not exists
       then do when (i /= -1 && talkative) $ putStrLn $ buildMsg thename
               return thename
       else go $ i+1
    where thename = buildName i

-------------------------------
-- AnchoredPath utilities
--

-- The type of file and directory names that can appear as entries inside a
-- directory. Must not be empty, ".", or "..", and must not contain path
-- separators. Also must be a valid (relative) file path on the native platform
-- (TODO this is currently not checked).
newtype Name = Name BS.ShortByteString deriving (Binary, Eq, Ord, Show)

fromName :: Name -> B.ByteString
fromName (Name s) = BS.fromShort s

-- | This is a type of "sane" file paths. These are always canonic in the sense
-- that there are no stray slashes, no ".." components and similar. They are
-- usually used to refer to a location within a Tree, but a relative filesystem
-- path works just as well. These are either constructed from individual name
-- components (using "appendPath", "catPaths" and "makeName"), or converted
-- from a FilePath ("unsafeFloatPath" -- but take care when doing that).
newtype AnchoredPath = AnchoredPath [Name] deriving (Binary, Eq, Show, Ord)

-- | Check whether a path is a prefix of another path.
isPrefix :: AnchoredPath -> AnchoredPath -> Bool
(AnchoredPath a) `isPrefix` (AnchoredPath b) = a `isPrefixOf` b

-- | Append an element to the end of a path.
appendPath :: AnchoredPath -> Name -> AnchoredPath
appendPath (AnchoredPath p) n = AnchoredPath $ p ++ [n]

-- | Catenate two paths together. Not very safe, but sometimes useful
-- (e.g. when you are representing paths relative to a different point than a
-- Tree root).
catPaths :: AnchoredPath -> AnchoredPath -> AnchoredPath
catPaths (AnchoredPath p) (AnchoredPath n) = AnchoredPath (p ++ n)

-- | Get parent (path) of a given path. foo/bar/baz -> foo/bar
parent :: AnchoredPath -> Maybe AnchoredPath
parent (AnchoredPath []) = Nothing
parent (AnchoredPath x) = Just (AnchoredPath (init x))

-- | List all (proper) parents of a given path. foo/bar/baz -> [.,foo, foo/bar]
parents :: AnchoredPath -> [AnchoredPath]
parents (AnchoredPath []) = [] -- root has no parents
parents (AnchoredPath xs) = map AnchoredPath $ inits $ init xs

-- | Take a "root" directory and an anchored path and produce a full
-- 'FilePath'. Moreover, you can use @anchorPath \"\"@ to get a relative
-- 'FilePath'.
anchorPath :: FilePath -> AnchoredPath -> FilePath
anchorPath dir p = dir FilePath.</> decodeLocale (flatten p)
{-# INLINE anchorPath #-}

-- FIXME returning "." for the root is wrong
flatten :: AnchoredPath -> BC.ByteString
flatten (AnchoredPath []) = BC.singleton '.'
flatten (AnchoredPath p) = BC.intercalate (BC.singleton '/') (map fromName p)

-- | Make a 'Name' from a 'String'. May fail if the input 'String'
-- is invalid, that is, "", ".", "..", or contains a '/'.
makeName :: String -> Either String Name
makeName = rawMakeName . encodeLocale

-- | Take a relative FilePath and turn it into an AnchoredPath. This is a
-- partial function. Basically, by using unsafeFloatPath, you are testifying that the
-- argument is a path relative to some common root -- i.e. the root of the
-- associated "Tree" object. In particular, the input path may not contain any
-- ocurrences of "." or ".." after normalising. You should sanitize any
-- FilePaths before you declare them "good" by converting into AnchoredPath
-- (using this function), especially if the FilePath come from any external
-- source (command line, file, environment, network, etc)
unsafeFloatPath :: HasCallStack => FilePath -> AnchoredPath
unsafeFloatPath = either error id . floatPath

floatPath :: FilePath -> Either String AnchoredPath
floatPath path = do
    r <- mapM makeName (prepare path)
    return (AnchoredPath r)
  where
    sensible s = s `notElem` ["", "."]
    prepare = filter sensible .
      NativeFilePath.splitDirectories . NativeFilePath.normalise .
      NativeFilePath.dropTrailingPathSeparator

anchoredRoot :: AnchoredPath
anchoredRoot = AnchoredPath []

-- | A view on 'AnchoredPath's.
parentChild :: AnchoredPath -> Maybe (AnchoredPath, Name)
parentChild (AnchoredPath []) = Nothing
parentChild (AnchoredPath xs) = Just (AnchoredPath (init xs), last xs)

-- | Replace the second arg's parent with the first arg.
replaceParent :: AnchoredPath -> AnchoredPath -> Maybe AnchoredPath
replaceParent (AnchoredPath xs) p =
  case parentChild p of
    Nothing -> Nothing
    Just (_,x) -> Just (AnchoredPath (xs ++ [x]))

-- | Make a 'Name' from a 'B.ByteString'.
rawMakeName :: B.ByteString -> Either String Name
rawMakeName s
  | isBadName s =
      Left $ "'"++decodeLocale s++"' is not a valid AnchoredPath component name"
  | otherwise = Right (Name (BS.toShort s))

isBadName :: B.ByteString -> Bool
isBadName n = hasPathSeparator n || n `elem` forbiddenNames

-- It would be nice if we could add BC.pack "_darcs" to the list, however
-- "_darcs" could be a valid file or dir name if not inside the top level
-- directory.
forbiddenNames :: [B.ByteString]
forbiddenNames = [BC.empty, BC.pack ".", BC.pack ".."]

hasPathSeparator :: B.ByteString -> Bool
hasPathSeparator x = any (`BC.elem` x) NativeFilePath.pathSeparators

eqAnycase :: Name -> Name -> Bool
eqAnycase (Name a) (Name b) = BS.map to_lower a == BS.map to_lower b
  where
    to_lower :: Word8 -> Word8
    to_lower = fromIntegral . ord . toLower . unsafeChr . fromIntegral

encodeWhiteName :: Name -> B.ByteString
encodeWhiteName = encodeLocale . encodeWhite . decodeLocale . fromName

decodeWhiteName :: B.ByteString -> Either String Name
decodeWhiteName =
  rawMakeName . encodeLocale <=< decodeWhite . decodeLocale

-- | The effect of renaming on paths.
-- The first argument is the old path, the second is the new path,
-- and the third is the possibly affected path we are interested in.
movedirfilename :: AnchoredPath -> AnchoredPath -> AnchoredPath -> AnchoredPath
movedirfilename (AnchoredPath old) newp@(AnchoredPath new) orig@(AnchoredPath path) =
  case stripPrefix old path of
    Just [] -> newp -- optimization to avoid allocation in this case
    Just rest -> AnchoredPath (new ++ rest)
    Nothing -> orig -- old is not a prefix => no change

-- | Construct a filter from a list of AnchoredPaths, that will accept any path
-- that is either a parent or a child of any of the listed paths, and discard
-- everything else.
filterPaths :: [AnchoredPath] -> AnchoredPath -> t -> Bool
filterPaths files p _ = any (\x -> x `isPrefix` p || p `isPrefix` x) files

-- | Transform a SubPath into an AnchoredPath.
floatSubPath :: SubPath -> Either String AnchoredPath
floatSubPath = floatPath . toFilePath

-- | Is the given path in (or equal to) the _darcs metadata directory?
inDarcsdir :: AnchoredPath -> Bool
inDarcsdir (AnchoredPath (x:_)) | x == darcsdirName = True
inDarcsdir _ = False

darcsdirName :: Name
darcsdirName = either error id (makeName darcsdir)

isRoot :: AnchoredPath -> Bool
isRoot (AnchoredPath xs) = null xs
