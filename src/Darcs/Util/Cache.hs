module Darcs.Util.Cache
    ( Cache
    , mkCache
    , mkDirCache
    , mkRepoCache
    , cacheEntries
    , CacheType(..)
    , CacheLoc(..)
    , WritableOrNot(..)
    , HashedDir(..)
    , hashedDir
    , bucketFolder
    , filterRemoteCaches
    , cleanCaches
    , cleanCachesWithHint
    , fetchFileUsingCache
    , speculateFileUsingCache
    , speculateFilesUsingCache
    , writeFileUsingCache
    , peekInCache
    , parseCacheLoc
    , showCacheLoc
    , writable
    , isThisRepo
    , hashedFilePath
    , allHashedDirs
    , reportBadSources
    , closestWritableDirectory
    , dropNonRepos
    ) where

import Control.Concurrent.MVar ( MVar, modifyMVar_, newMVar, readMVar )
import Control.Monad ( filterM, forM_, liftM, mplus, unless, when )
import qualified Data.ByteString as B ( ByteString )
import Data.List ( intercalate, nub, sortBy )
import Data.Maybe ( catMaybes, fromMaybe, listToMaybe )
import System.Directory
    ( createDirectoryIfMissing
    , doesDirectoryExist
    , doesFileExist
    , getDirectoryContents
    , getPermissions
    , removeFile
    , withCurrentDirectory
    )
import qualified System.Directory as SD ( writable )
import System.FilePath.Posix ( dropFileName, joinPath, (</>) )
import System.IO ( hPutStrLn, stderr )
import System.IO.Error ( isAlreadyExistsError )
import System.IO.Unsafe ( unsafePerformIO )
import System.Posix.Files ( createLink, getSymbolicLinkStatus, linkCount )
import Text.Regex.Applicative ( anySym, many, match, string, (<|>) )

import Darcs.Prelude

import Darcs.Util.ByteString ( gzWriteFilePS )
import Darcs.Util.English ( Noun(..), Pronoun(..), englishNum )
import Darcs.Util.Exception ( catchall, handleOnly )
import Darcs.Util.File
    ( Cachable(Cachable)
    , copyFileOrUrl
    , fetchFilePS
    , gzFetchFilePS
    , speculateFileOrUrl
    , withTemp
    )
import Darcs.Util.Global ( darcsdir, defaultRemoteDarcsCmd )
import Darcs.Util.Lock ( gzWriteAtomicFilePS )
import Darcs.Util.Progress ( debugMessage, progressList )
import Darcs.Util.URL ( isHttpUrl, isSshUrl, isValidLocalPath )
import Darcs.Util.ValidHash
    ( ValidHash(..)
    , HashedDir(..)
    , checkHash
    , encodeValidHash
    , okayHash
    , calcValidHash
    )

-- * Caches

hashedDir :: HashedDir -> FilePath
hashedDir HashedPristineDir = "pristine.hashed"
hashedDir HashedPatchesDir = "patches"
hashedDir HashedInventoriesDir = "inventories"

allHashedDirs :: [HashedDir]
allHashedDirs = [ HashedPristineDir
                , HashedPatchesDir
                , HashedInventoriesDir
                ]

data WritableOrNot = Writable
                   | NotWritable
                   deriving ( Eq, Show )

data CacheType = Repo
               | Directory
               deriving ( Eq, Show )

data CacheLoc = Cache
    { cacheType :: !CacheType
    , cacheWritable :: !WritableOrNot
    , cacheSource :: !String
    }

-- | Cache is an abstract type for hiding the underlying cache locations
newtype Cache = Ca [CacheLoc]

-- | Smart constructor for 'Cache'.
mkCache :: [CacheLoc] -> Cache
mkCache = Ca . nub . sortBy compareByLocality

mkDirCache :: FilePath -> Cache
mkDirCache dir = mkCache [Cache Directory Writable dir]

mkRepoCache :: FilePath -> Cache
mkRepoCache dir = mkCache [Cache Repo Writable dir]

cacheEntries :: Cache -> [CacheLoc]
cacheEntries (Ca entries) = entries

-- | Note: this non-structural instance ignores the 'cacheWritable' field. This
-- is so that when we 'nub' a list of locations we retain only one (the first)
-- variant.
instance Eq CacheLoc where
    (Cache aTy _ aSrc) == (Cache bTy _ bSrc) = aTy == bTy && aSrc == bSrc

showCacheLoc :: CacheLoc -> String
showCacheLoc (Cache Repo Writable a) = "thisrepo:" ++ a
showCacheLoc (Cache Repo NotWritable a) = "repo:" ++ a
showCacheLoc (Cache Directory Writable a) = "cache:" ++ a
showCacheLoc (Cache Directory NotWritable a) = "readonly:" ++ a

instance Show Cache where
    show (Ca cs) = intercalate "\n" $ map showCacheLoc cs

parseCacheLoc :: String -> Maybe CacheLoc
parseCacheLoc = match reCacheLoc
  where
    reCacheLoc =
      Cache Repo Writable <$> (string "thisrepo:" *> rest) <|>
      Cache Repo NotWritable <$> (string "repo:" *> rest) <|>
      Cache Directory Writable <$> (string "cache:" *> rest) <|>
      Cache Directory NotWritable <$> (string "readonly:" *> rest)
    rest = many anySym

-- | Filter caches for remote repos. This affects only entries that are locally
-- valid paths (i.e. not network URLs): they are removed if non-existent, or
-- demoted to NotWritable if they are not actually writable in the file system.
filterRemoteCaches :: Cache -> IO Cache
filterRemoteCaches (Ca remote) = mkCache . catMaybes <$> filtered
  where
    filtered = mapM (\x -> mbGetRemoteCacheLoc x `catchall` return Nothing) remote
    mbGetRemoteCacheLoc :: CacheLoc -> IO (Maybe CacheLoc)
    mbGetRemoteCacheLoc c@(Cache t _ url)
        | isValidLocalPath url = do
            ex <- doesDirectoryExist url
            if ex
                then do
                    p <- getPermissions url
                    return $ Just $ if writable c && SD.writable p
                                        then c
                                        else Cache t NotWritable url
                else return Nothing
        | otherwise = return $ Just c

-- | Compares two caches, a remote cache is greater than a local one.
-- The order of the comparison is given by: local < http < ssh
compareByLocality :: CacheLoc -> CacheLoc -> Ordering
compareByLocality (Cache _ w x) (Cache _ z y)
    | isValidLocalPath x && isRemote y  = LT
    | isRemote x && isValidLocalPath y = GT
    | isHttpUrl x && isSshUrl y = LT
    | isSshUrl x && isHttpUrl y = GT
    | isValidLocalPath x && isWritable w
        && isValidLocalPath y && isNotWritable z = LT
    | otherwise = EQ
  where
    isRemote r = isHttpUrl r || isSshUrl r
    isWritable = (==) Writable
    isNotWritable = (==) NotWritable

-- |@fetchFileUsingCache cache dir hash@ receives a list of caches @cache@, the
-- directory for which that file belongs @dir@ and the @hash@ of the file to
-- fetch.  It tries to fetch the file from one of the sources, trying them in
-- order one by one.  If the file cannot be fetched from any of the sources,
-- this operation fails. Otherwise we return the path where we found the file
-- and its content.
fetchFileUsingCache :: ValidHash h => Cache -> h
                    -> IO (FilePath, B.ByteString)
fetchFileUsingCache = fetchFileUsingCachePrivate Anywhere

writable :: CacheLoc -> Bool
writable (Cache _ NotWritable _) = False
writable (Cache _ Writable _) = True

-- | This keeps only 'Repo' 'NotWritable' entries.
dropNonRepos :: Cache -> Cache
dropNonRepos (Ca cache) = Ca $ filter notRepo cache where
  notRepo xs = case xs of
    Cache Directory _ _ -> False
    -- we don't want to write thisrepo: entries to the disk
    Cache Repo Writable _ -> False
    _ -> True

closestWritableDirectory :: Cache -> Maybe String
closestWritableDirectory (Ca cs) =
  listToMaybe . catMaybes .flip map cs $ \case
    Cache Directory Writable x -> Just x
    _ -> Nothing

isThisRepo :: CacheLoc -> Bool
isThisRepo (Cache Repo Writable _) = True
isThisRepo _ = False

bucketFolder :: FilePath -> FilePath
bucketFolder f = take 2 (cleanHash f)
    where
        cleanHash fileName = case dropWhile (/= '-') fileName of
            []  -> fileName
            s   -> drop 1 s

-- | The full filepath of a simple file name inside a given 'CacheLoc'
-- under 'HashedDir'.
hashedFilePath :: CacheLoc -> HashedDir -> FilePath -> FilePath
hashedFilePath (Cache Directory Writable d) s f =
    joinPath [d, hashedDir s, bucketFolder f, f]
hashedFilePath (Cache Directory NotWritable d) s f =
    joinPath [d, hashedDir s, f]
hashedFilePath (Cache Repo _ r) s f =
    joinPath [r, darcsdir, hashedDir s, f]

-- | Return whether the 'Cache' contains a file with the given hash in a
-- writable position.
peekInCache :: ValidHash h => Cache -> h -> IO Bool
peekInCache (Ca cache) sh = cacheHasIt cache `catchall` return False
  where
    subdir = dirofValidHash sh
    cacheHasIt [] = return False
    cacheHasIt (c : cs)
        | not $ writable c = cacheHasIt cs
        | otherwise = do
            ex <- doesFileExist $ hashedFilePath c subdir (encodeValidHash sh)
            if ex then return True else cacheHasIt cs

-- | Add pipelined downloads to the (low-priority) queue, for the rest it is a noop.
speculateFileUsingCache :: ValidHash h => Cache -> h -> IO ()
speculateFileUsingCache c hash = do
    let filename = encodeValidHash hash
    debugMessage $ "Speculating on " ++ filename
    copyFileUsingCache OnlySpeculate c (dirofValidHash hash) filename

-- | Do 'speculateFilesUsingCache' for files not already in a writable cache
-- position.
speculateFilesUsingCache :: ValidHash h => Cache -> [h] -> IO ()
speculateFilesUsingCache _ [] = return ()
speculateFilesUsingCache cache hs = do
    hs' <- filterM (fmap not . peekInCache cache) hs
    forM_ hs' $ speculateFileUsingCache cache

data OrOnlySpeculate = ActuallyCopy
                     | OnlySpeculate
                     deriving ( Eq, Show )

-- | If the first parameter of type 'OrOnlySpeculate' is 'ActuallyCopy', try to
-- ensure that a file with the given name (hash) exists in a writable location
-- (which means in particular that it is stored in the local file system). If
-- it is 'OnlySpeculate', then merely schedule download of that file into such
-- a location (the actual download will be executed asynchronously).
--
-- If the file is already present in some writeable location, or if there is no
-- writable location at all, this procedure does nothing.
--
-- If the copy should occur between two locations of the same filesystem, a
-- hard link is made.
--
-- If the first parameter is 'ActuallyCopy', use 'copyFileOrUrl' and try to
-- find the file in any non-writable location. Otherwise ('OnlySpeculate'), use
-- 'speculateFileOrUrl' and try only the first non-writable location (which
-- makes sense since 'speculateFileOrUrl' is asynchronous and thus can't fail
-- in any interesting way).
copyFileUsingCache :: OrOnlySpeculate -> Cache -> HashedDir -> FilePath -> IO ()
copyFileUsingCache oos (Ca cache) subdir f = do
    debugMessage $ unwords ["copyFileUsingCache:", show oos, hashedDir subdir, f]
    Just stickItHere <- cacheLoc cache
    createDirectoryIfMissing True (dropFileName stickItHere)
    filterBadSources cache >>= sfuc stickItHere
    `catchall`
    return ()
  where
    -- Return last writeable cache/repo location for file 'f'.
    -- Usually returns the global cache unless `--no-cache` is passed.
    -- Throws exception if file already exists in a writable location.
    cacheLoc [] = return Nothing
    cacheLoc (c : cs)
        | not $ writable c = cacheLoc cs
        | otherwise = do
            let attemptPath = hashedFilePath c subdir f
            ex <- doesFileExist attemptPath
            if ex
                then fail "File already present in writable location."
                else do
                    othercache <- cacheLoc cs
                    return $ othercache `mplus` Just attemptPath
    -- Do the actual copy, or hard link, or put file in download queue. This
    -- tries to find the file in all non-writable locations, in order, unless
    -- we have OnlySpeculate.
    sfuc _ [] = return ()
    sfuc out (c : cs)
        | not (writable c) =
            let cacheFile = hashedFilePath c subdir f in
            case oos of
                OnlySpeculate ->
                     speculateFileOrUrl cacheFile out
                     `catchall`
                     checkCacheReachability c
                ActuallyCopy ->
                     do debugMessage $
                          "Copying from " ++ show cacheFile ++ " to  " ++ show out
                        copyFileOrUrl defaultRemoteDarcsCmd cacheFile out Cachable
                     `catchall`
                     (do checkCacheReachability c
                         sfuc out cs) -- try another read-only location
        | otherwise = sfuc out cs

data FromWhere = LocalOnly
               | Anywhere
               deriving ( Eq )

-- | Checks if a given cache entry is reachable or not.  It receives an error
-- caught during execution and the cache entry.  If the caches is not reachable
-- it is blacklisted and not longer tried for the rest of the session. If it is
-- reachable it is whitelisted and future errors with such cache get ignore.
-- To determine reachability:
--  * For a local cache, if the given source doesn't exist anymore, it is
--    blacklisted.
--  * For remote sources if the error is timeout, it is blacklisted, if not,
--    it checks if _darcs/hashed_inventory  exist, if it does, the entry is
--    whitelisted, if it doesn't, it is blacklisted.
checkCacheReachability :: CacheLoc -> IO ()
checkCacheReachability cache
    | isValidLocalPath source = doUnreachableCheck $
        checkFileReachability (doesDirectoryExist source)
    | isHttpUrl source = doUnreachableCheck $
        checkFileReachability (checkHashedInventoryReachability cache)
    | isSshUrl source = doUnreachableCheck $
        checkFileReachability (checkHashedInventoryReachability cache)
    | otherwise = fail $ "unknown transport protocol for: " ++ source
  where
    source = cacheSource cache

    doUnreachableCheck unreachableAction = do
        reachable <- isReachableSource
        unless (reachable source) unreachableAction

    checkFileReachability doCheck = do
        reachable <- doCheck
        if reachable
            then addReachableSource source
            else addBadSource source

-- | Returns a list of reachables cache entries, removing blacklisted entries.
filterBadSources :: [CacheLoc] -> IO [CacheLoc]
filterBadSources cache = do
    badSource <- isBadSource
    return $ filter (not . badSource . cacheSource) cache

-- | Checks if the _darcs/hashed_inventory exist and is reachable
checkHashedInventoryReachability :: CacheLoc -> IO Bool
checkHashedInventoryReachability cache = withTemp $ \tempout -> do
    let f = cacheSource cache </> darcsdir </> "hashed_inventory"
    copyFileOrUrl defaultRemoteDarcsCmd f tempout Cachable
    return True
    `catchall` return False

-- | Get contents of some hashed file taking advantage of the cache system.
-- We have a list of locations (@cache@) ordered from "closest/fastest"
-- (typically, the destination repo) to "farthest/slowest" (typically,
-- the source repo).
-- First, if possible it copies the file from remote location to local.
-- Then, it reads it contents, and links the file across all writeable
-- locations including the destination repository.
fetchFileUsingCachePrivate :: ValidHash h => FromWhere -> Cache -> h
                           -> IO (FilePath, B.ByteString)
fetchFileUsingCachePrivate fromWhere (Ca cache) hash = do
    when (fromWhere == Anywhere) $
        copyFileUsingCache ActuallyCopy (Ca cache) subdir filename
    filterBadSources cache >>= ffuc
  where
    filename = encodeValidHash hash
    subdir = dirofValidHash hash
    ffuc (c : cs)
        | not (writable c) &&
            (Anywhere == fromWhere || isValidLocalPath cacheFile) = do
            -- Looks like `copyFileUsingCache` could not copy the file we
            -- wanted. This can happen if `--no-cache` is NOT passed and the
            -- global cache is not accessible.
            debugMessage $
              "In fetchFileUsingCachePrivate I'm directly grabbing file contents from "
              ++ cacheFile
            x <- gzFetchFilePS cacheFile Cachable
            if not $ checkHash hash x
                then do
                    x' <- fetchFilePS cacheFile Cachable
                    unless (checkHash hash x') $ do
                        hPutStrLn stderr $ "Hash failure in " ++ cacheFile
                        fail $ "Hash failure in " ++ cacheFile
                    return (cacheFile, x')
                else return (cacheFile, x) -- FIXME: create links in caches
            `catchall` do
                -- something bad happened, check if cache became unaccessible
                -- and try other ones
                checkCacheReachability c
                filterBadSources cs >>= ffuc
        | writable c = do
            debugMessage $ "About to gzFetchFilePS from " ++ show cacheFile
            x1 <- gzFetchFilePS cacheFile Cachable
            debugMessage "gzFetchFilePS done."
            x <- if not $ checkHash hash x1
                     then do
                        x2 <- fetchFilePS cacheFile Cachable
                        unless (checkHash hash x2) $ do
                            hPutStrLn stderr $ "Hash failure in " ++ cacheFile
                            removeFile cacheFile
                            fail $ "Hash failure in " ++ cacheFile
                        return x2
                     else return x1
            -- Linking is optional here; the catchall prevents darcs from
            -- failing if repo and cache are on different file systems.
            mapM_ (tryLinking cacheFile filename subdir) cs `catchall` return ()
            return (cacheFile, x)
            `catchall` do
                debugMessage "Caught exception, now attempt creating cache."
                createCache c subdir filename `catchall` return ()
                checkCacheReachability c
                -- fetch file from remaining locations
                (fname, x) <- filterBadSources cs >>= ffuc
                debugMessage $
                  "Attempt creating link from: " ++ show fname ++ " to " ++ show cacheFile
                (createLink fname cacheFile >> debugMessage "successfully created link"
                                            >> return (cacheFile, x))
                  `catchall` do
                    debugMessage $ "Attempt writing file: " ++ show cacheFile
                    -- the following block is usually when files get actually written
                    -- inside of _darcs or global cache.
                    do createDirectoryIfMissing True (dropFileName cacheFile)
                       gzWriteFilePS cacheFile x
                       debugMessage "successfully wrote file"
                       `catchall` return ()
                    -- above block can fail if cache is not writeable
                    return (fname, x)
        | otherwise = ffuc cs
        where
          cacheFile = hashedFilePath c subdir filename

    ffuc [] = fail ("Couldn't fetch " ++ filename ++ "\nin subdir "
                          ++ hashedDir subdir ++ " from sources:\n"
                          ++ show (Ca cache)
                          ++ if subdir == HashedPristineDir
                             then "\nRun `darcs repair` to fix this problem."
                             else "")

tryLinking :: FilePath -> FilePath -> HashedDir -> CacheLoc -> IO ()
tryLinking source filename subdir c =
  when (writable c) $ do
    createCache c subdir filename
    let target = hashedFilePath c subdir filename
    debugMessage $ "Linking " ++ source ++ " to " ++ target
    handleOnly isAlreadyExistsError (return ()) $ createLink source target

createCache :: CacheLoc -> HashedDir -> FilePath -> IO ()
createCache (Cache Directory _ d) subdir filename =
    createDirectoryIfMissing True (d </> hashedDir subdir </> bucketFolder filename)
createCache _ _ _ = return ()

-- | Write file content, except if it is already in the cache, in
-- which case merely create a hard link to that file. The returned value
-- is the size and hash of the content.
writeFileUsingCache
  :: ValidHash h => Cache -> B.ByteString -> IO h
writeFileUsingCache (Ca cache) content = do
    debugMessage $ "writeFileUsingCache "++filename
    (fn, _) <- fetchFileUsingCachePrivate LocalOnly (Ca cache) hash
    mapM_ (tryLinking fn filename subdir) cache
    return hash
    `catchall`
    wfuc cache
    `catchall`
    fail ("Couldn't write " ++ filename ++ "\nin subdir "
               ++ hashedDir subdir ++ " to sources:\n\n"++ show (Ca cache))
  where
    subdir = dirofValidHash hash
    hash = calcValidHash content
    filename = encodeValidHash hash
    wfuc (c : cs)
        | not $ writable c = wfuc cs
        | otherwise = do
            createCache c subdir filename
            let cacheFile = hashedFilePath c subdir filename
            gzWriteAtomicFilePS cacheFile content
            -- create links in all other writable locations
            debugMessage $ "writeFileUsingCache remaining sources:\n"++show (Ca cs)
            -- Linking is optional here; the catchall prevents darcs from
            -- failing if repo and cache are on different file systems.
            mapM_ (tryLinking cacheFile filename subdir) cs `catchall` return ()
            return hash
    wfuc [] = fail $ "No location to write file " ++ (hashedDir subdir </> filename)

cleanCaches :: Cache -> HashedDir -> IO ()
cleanCaches c d = cleanCachesWithHint' c d Nothing

cleanCachesWithHint :: Cache -> HashedDir -> [String] -> IO ()
cleanCachesWithHint c d h = cleanCachesWithHint' c d (Just h)

cleanCachesWithHint' :: Cache -> HashedDir -> Maybe [String] -> IO ()
cleanCachesWithHint' (Ca cs) subdir hint = mapM_ cleanCache cs
  where
    cleanCache (Cache Directory Writable d) =
        withCurrentDirectory (d </> hashedDir subdir) (do
            fs' <- getDirectoryContents "."
            let fs = filter okayHash $ fromMaybe fs' hint
                cleanMsg = "Cleaning cache " ++ d </> hashedDir subdir
            mapM_ clean $ progressList cleanMsg fs)
        `catchall`
        return ()
    cleanCache _ = return ()
    clean f = do
        lc <- linkCount `liftM` getSymbolicLinkStatus f
        when (lc < 2) $ removeFile f
        `catchall`
        return ()

-- | Prints an error message with a list of bad caches.
reportBadSources :: IO ()
reportBadSources = do
    sources <- getBadSourcesList
    let size = length sources
    unless (null sources) $ hPutStrLn stderr $
        concat [ "\nBy the way, I could not reach the following "
               , englishNum size (Noun "location") ":"
               , "\n"
               , intercalate "\n" (map ("  " ++) sources)
               , "\nUnless you plan to restore access to "
               , englishNum size It ", you should delete "
               , "the corresponding "
               , englishNum size (Noun "entry") " from _darcs/prefs/sources."
               ]

-- * Global Variables

badSourcesList :: MVar [String]
badSourcesList = unsafePerformIO $ newMVar []
{-# NOINLINE badSourcesList #-}

addBadSource :: String -> IO ()
addBadSource cache = modifyMVarPure badSourcesList (cache:)

getBadSourcesList :: IO [String]
getBadSourcesList = readMVar badSourcesList

isBadSource :: IO (String -> Bool)
isBadSource = do
    badSources <- getBadSourcesList
    return (`elem` badSources)

reachableSourcesList :: MVar [String]
reachableSourcesList = unsafePerformIO $ newMVar []
{-# NOINLINE reachableSourcesList #-}

addReachableSource :: String -> IO ()
addReachableSource src = modifyMVarPure reachableSourcesList (src:)

getReachableSources :: IO [String]
getReachableSources = readMVar reachableSourcesList

isReachableSource :: IO (String -> Bool)
isReachableSource =  do
    reachableSources <- getReachableSources
    return (`elem` reachableSources)

modifyMVarPure :: MVar a -> (a -> a) -> IO ()
modifyMVarPure mvar f = modifyMVar_ mvar (return . f)
