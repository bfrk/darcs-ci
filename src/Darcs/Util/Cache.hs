module Darcs.Util.Cache
    ( cacheHash
    , okayHash
    , Cache
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
    , writable
    , isThisRepo
    , hashedFilePath
    , allHashedDirs
    , reportBadSources
    , closestWritableDirectory
    , dropNonRepos
    , Compression(..)
    ) where

import Control.Concurrent.MVar ( MVar, modifyMVar_, newMVar, readMVar )
import Control.Monad ( filterM, forM_, liftM, mplus, unless, when )
import qualified Data.ByteString as B ( ByteString, length )
import Data.List ( intercalate, nub, sortBy )
import Data.Maybe ( catMaybes, fromMaybe, listToMaybe )
import System.Directory
    ( createDirectoryIfMissing
    , doesDirectoryExist
    , doesFileExist
    , getDirectoryContents
    , getPermissions
    , removeFile
    )
import qualified System.Directory as SD ( writable )
import System.FilePath.Posix ( dropFileName, joinPath, (</>) )
import System.IO ( hPutStrLn, stderr )
import System.IO.Error ( isAlreadyExistsError )
import System.IO.Unsafe ( unsafePerformIO )
import System.Posix.Files ( createLink, getSymbolicLinkStatus, linkCount )

import Darcs.Prelude

import Darcs.Util.ByteString ( gzWriteFilePS )
import qualified Darcs.Util.Download as Download ( ConnectionError )
import Darcs.Util.English ( Noun(..), Pronoun(..), englishNum )
import Darcs.Util.Exception ( catchall, handleOnly )
import Darcs.Util.External
    ( Cachable(Cachable)
    , copyFileOrUrl
    , fetchFilePS
    , gzFetchFilePS
    , speculateFileOrUrl
    )
import Darcs.Util.File ( withCurrentDirectory )
import Darcs.Util.Global ( darcsdir, defaultRemoteDarcsCmd )
import Darcs.Util.Hash ( sha256sum )
import Darcs.Util.Lock ( gzWriteAtomicFilePS, withTemp, writeAtomicFilePS )
import Darcs.Util.Progress ( debugMessage, progressList )
import Darcs.Util.SignalHandler ( catchNonSignal )
import Darcs.Util.URL ( isHttpUrl, isSshUrl, isValidLocalPath )

data HashedDir = HashedPristineDir
               | HashedPatchesDir
               | HashedInventoriesDir
               deriving Eq

hashedDir :: HashedDir -> String
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

instance Show CacheLoc where
    show (Cache Repo Writable a) = "thisrepo:" ++ a
    show (Cache Repo NotWritable a) = "repo:" ++ a
    show (Cache Directory Writable a) = "cache:" ++ a
    show (Cache Directory NotWritable a) = "readonly:" ++ a

instance Show Cache where
    show (Ca cs) = intercalate "\n" $ map show cs

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

-- | 'cacheHash' computes the cache hash (i.e. filename) of a packed string.
cacheHash :: B.ByteString -> String
cacheHash ps = if sizeStrLen > 10
                   then shaOfPs
                   else replicate (10 - sizeStrLen) '0' ++ sizeStr
                        ++ '-' : shaOfPs
  where
    sizeStr = show $ B.length ps
    sizeStrLen = length sizeStr
    shaOfPs = sha256sum ps

okayHash :: String -> Bool
okayHash s = length s `elem` [64, 75]

checkHash :: String -> B.ByteString -> Bool
checkHash h s
    | length h == 64 = sha256sum s == h
    | length h == 75 =
        B.length s == read (take 10 h) && sha256sum s == drop 11 h
    | otherwise = False

-- |@fetchFileUsingCache cache dir hash@ receives a list of caches @cache@, the
-- directory for which that file belongs @dir@ and the @hash@ of the file to
-- fetch.  It tries to fetch the file from one of the sources, trying them in
-- order one by one.  If the file cannot be fetched from any of the sources,
-- this operation fails. Otherwise we return the path where we found the file
-- and its content.
fetchFileUsingCache :: Cache -> HashedDir -> String
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

bucketFolder :: String -> String
bucketFolder f = take 2 (cleanHash f)
    where
        cleanHash fileName = case dropWhile (/= '-') fileName of
            []  -> fileName
            s   -> drop 1 s

-- | @hashedFilePath cachelocation subdir hash@ returns the physical filename
-- of hash @hash@ in the @subdir@ section of @cachelocation@.
hashedFilePath :: CacheLoc -> HashedDir -> String -> String
hashedFilePath (Cache Directory _ d) s f =
    joinPath [d, hashedDir s, bucketFolder f, f]
hashedFilePath (Cache Repo _ r) s f =
    joinPath [r, darcsdir, hashedDir s, f]

-- | @hashedFilePathReadOnly cachelocation subdir hash@ returns the physical filename
-- of hash @hash@ in the @subdir@ section of @cachelocation@.
-- If directory, assume it is non-bucketed cache (old cache location).
hashedFilePathReadOnly :: CacheLoc -> HashedDir -> String -> String
hashedFilePathReadOnly (Cache Directory _ d) s f =
    d </> hashedDir s </> f
hashedFilePathReadOnly (Cache Repo _ r) s f =
    r </> darcsdir </> hashedDir s </> f

-- | @peekInCache cache subdir hash@ tells whether @cache@ and contains an
-- object with hash @hash@ in a writable position.  Florent: why do we want it
-- to be in a writable position?
peekInCache :: Cache -> HashedDir -> String -> IO Bool
peekInCache (Ca cache) subdir f = cacheHasIt cache `catchall` return False
  where
    cacheHasIt [] = return False
    cacheHasIt (c : cs)
        | not $ writable c = cacheHasIt cs
        | otherwise = do
            ex <- doesFileExist $ hashedFilePath c subdir f
            if ex then return True else cacheHasIt cs

-- | @speculateFileUsingCache cache subdirectory name@ takes note that the file
-- @name@ is likely to be useful soon: pipelined downloads will add it to the
-- (low-priority) queue, for the rest it is a noop.
speculateFileUsingCache :: Cache -> HashedDir -> String -> IO ()
speculateFileUsingCache c sd h = do
    debugMessage $ "Speculating on " ++ h
    copyFileUsingCache OnlySpeculate c sd h

-- | Note that the files are likely to be useful soon: pipelined downloads will
-- add them to the (low-priority) queue, for the rest it is a noop.
speculateFilesUsingCache :: Cache -> HashedDir -> [String] -> IO ()
speculateFilesUsingCache _ _ [] = return ()
speculateFilesUsingCache cache sd hs = do
    debugMessage $ "Thinking about speculating on " ++ unwords hs
    hs' <- filterM (fmap not . peekInCache cache sd) hs
    unless (null hs') $ do
        debugMessage $ "Speculating on " ++ unwords hs'
        copyFilesUsingCache OnlySpeculate cache sd hs'

data OrOnlySpeculate = ActuallyCopy
                     | OnlySpeculate
                     deriving ( Eq, Show )

-- | We hace a list of locations (@cache@) ordered from "closest/fastest"
-- (typically, the destination repo) to "farthest/slowest" (typically,
-- the source repo).
-- @copyFileUsingCache@ first checks whether given file @f@ is present
-- in some writeable location, if yes, do nothing. If no, it copies it
-- to the last writeable location, which would be the global cache
-- by default, or the destination repo if `--no-cache` is passed.
-- Function does nothing if there is no writeable location at all.
-- If the copy should occur between two locations of the same filesystem,
-- a hard link is actually made.
-- TODO document @oos@: what happens when we only speculate?
copyFileUsingCache :: OrOnlySpeculate -> Cache -> HashedDir -> String -> IO ()
copyFileUsingCache oos (Ca cache) subdir f = do
    debugMessage $ unwords ["copyFileUsingCache:", show oos, hashedDir subdir, f]
    Just stickItHere <- cacheLoc cache
    createDirectoryIfMissing True (dropFileName stickItHere)
    filterBadSources cache >>= sfuc stickItHere
    `catchall`
    return ()
  where
    -- return last writeable cache/repo location for file.
    -- usually returns the global cache unless `--no-cache` is passed.
    cacheLoc [] = return Nothing
    cacheLoc (c : cs)
        | not $ writable c = cacheLoc cs
        | otherwise = do
            let attemptPath = hashedFilePath c subdir f
            ex <- doesFileExist attemptPath
            if ex
                then fail $ "File already present in writable location."
                else do
                    othercache <- cacheLoc cs
                    return $ othercache `mplus` Just attemptPath
    -- do the actual copy, or hard link, or put file in download queue
    sfuc _ [] = return ()
    sfuc out (c : cs)
        | not (writable c) =
            let cacheFile = hashedFilePathReadOnly c subdir f in
            if oos == OnlySpeculate
                then speculateFileOrUrl cacheFile out
                     `catchNonSignal`
                     \e -> checkCacheReachability (show e) c
                else do debugMessage $ "Copying from " ++ show cacheFile ++ " to  " ++ show out
                        copyFileOrUrl defaultRemoteDarcsCmd cacheFile out Cachable
                     `catchNonSignal`
                     (\e -> do checkCacheReachability (show e) c
                               sfuc out cs) -- try another read-only location
        | otherwise = sfuc out cs

copyFilesUsingCache :: OrOnlySpeculate -> Cache -> HashedDir -> [String] -> IO ()
copyFilesUsingCache oos cache subdir hs =
    forM_ hs $ copyFileUsingCache oos cache subdir

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
checkCacheReachability :: String -> CacheLoc -> IO ()
checkCacheReachability e cache
    | isValidLocalPath source = doUnreachableCheck $
        checkFileReachability (doesDirectoryExist source)
    | isHttpUrl source =
        doUnreachableCheck $ do
            let err = case dropWhile (/= '(') e of
                          (_ : xs) -> fst (break (==')') xs)
                          _ -> e
            case reads err :: [(Download.ConnectionError, String)] of
                [(_, _)] -> addBadSource source
                _ -> checkFileReachability
                    (checkHashedInventoryReachability cache)
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
    `catchNonSignal` const (return False)

-- | Get contents of some hashed file taking advantage of the cache system.
-- We have a list of locations (@cache@) ordered from "closest/fastest"
-- (typically, the destination repo) to "farthest/slowest" (typically,
-- the source repo).
-- First, if possible it copies the file from remote location to local.
-- Then, it reads it contents, and links the file across all writeable
-- locations including the destination repository.
fetchFileUsingCachePrivate :: FromWhere -> Cache -> HashedDir -> String
                           -> IO (String, B.ByteString)
fetchFileUsingCachePrivate fromWhere (Ca cache) subdir hash = do
    when (fromWhere == Anywhere) $
        copyFileUsingCache ActuallyCopy (Ca cache) subdir hash
    filterBadSources cache >>= ffuc
  where
    ffuc (c : cs)
        | not (writable c) &&
            (Anywhere == fromWhere || isValidLocalPath (hashedFilePathReadOnly c subdir hash)) = do
            let cacheFile = hashedFilePathReadOnly c subdir hash
            -- looks like `copyFileUsingCache` could not copy the file we wanted.
            -- this can happen if `--no-cache` is NOT passed and the global cache is not accessible
            debugMessage $ "In fetchFileUsingCachePrivate I'm directly grabbing file contents from "
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
            `catchNonSignal` \e -> do
                -- something bad happened, check if cache became unaccessible and try other ones
                checkCacheReachability (show e) c
                filterBadSources cs >>= ffuc
        | writable c = let cacheFile = hashedFilePath c subdir hash in do
            debugMessage $ "About to gzFetchFilePS from " ++ show cacheFile
            x1 <- gzFetchFilePS cacheFile Cachable
            debugMessage $ "gzFetchFilePS done."
            x <- if not $ checkHash hash x1
                     then do
                        x2 <- fetchFilePS cacheFile Cachable
                        unless (checkHash hash x2) $ do
                            hPutStrLn stderr $ "Hash failure in " ++ cacheFile
                            removeFile cacheFile
                            fail $ "Hash failure in " ++ cacheFile
                        return x2
                     else return x1
            mapM_ (tryLinking cacheFile hash subdir) cs
            return (cacheFile, x)
            `catchNonSignal` \e -> do
                debugMessage "Caught exception, now attempt creating cache."
                createCache c subdir hash `catchall` return ()
                checkCacheReachability (show e) c
                (fname, x) <- filterBadSources cs >>= ffuc  -- fetch file from remaining locations
                debugMessage $ "Attempt creating link from: " ++ show fname ++ " to " ++ show cacheFile
                (createLink fname cacheFile >> (debugMessage "successfully created link")
                                            >> return (cacheFile, x))
                  `catchall` do
                    debugMessage $ "Attempt writing file: " ++ show cacheFile
                    -- the following block is usually when files get actually written
                    -- inside of _darcs or global cache.
                    do createDirectoryIfMissing True (dropFileName cacheFile)
                       gzWriteFilePS cacheFile x
                       debugMessage $ "successfully wrote file"
                       `catchall` return ()
                    -- above block can fail if cache is not writeable
                    return (fname, x)
        | otherwise = ffuc cs

    ffuc [] = fail ("Couldn't fetch " ++ hash ++ "\nin subdir "
                          ++ hashedDir subdir ++ " from sources:\n"
                          ++ show (Ca cache))

tryLinking :: FilePath -> String -> HashedDir -> CacheLoc -> IO ()
tryLinking source hash subdir c@(Cache Directory Writable _) =
  do createCache c subdir hash
     let target = hashedFilePath c subdir hash
     debugMessage $ "Linking " ++ source ++ " to " ++ target
     handleOnly isAlreadyExistsError (return ()) $ createLink source target
tryLinking source hash subdir c@(Cache Repo Writable _) =
  do let target = hashedFilePath c subdir hash
     debugMessage $ "Linking " ++ source ++ " to " ++ target
     handleOnly isAlreadyExistsError (return ()) $ createLink source target
tryLinking _ _ _ _ = return ()

createCache :: CacheLoc -> HashedDir -> String -> IO ()
createCache (Cache Directory _ d) subdir hash =
    createDirectoryIfMissing True (d </> hashedDir subdir </> bucketFolder hash)
createCache _ _ _ = return ()

data Compression = NoCompression
                 | GzipCompression
    deriving ( Eq, Show )

-- | @write compression filename content@ writes @content@ to the file
-- @filename@ according to the policy given by @compression@.
write :: Compression -> FilePath -> B.ByteString -> IO ()
write NoCompression = writeAtomicFilePS
write GzipCompression = gzWriteAtomicFilePS

-- | @writeFileUsingCache cache compression subdir contents@ write the string
-- @contents@ to the directory subdir, except if it is already in the cache, in
-- which case it is a noop.  Warning (?) this means that in case of a hash
-- collision, writing using writeFileUsingCache is a noop. The returned value
-- is the filename that was given to the string.
writeFileUsingCache :: Cache -> Compression -> HashedDir -> B.ByteString
                    -> IO String
writeFileUsingCache (Ca cache) compr subdir ps = do
    debugMessage $ "writeFileUsingCache "++hash
    (fn, _) <- fetchFileUsingCachePrivate LocalOnly (Ca cache) subdir hash
    mapM_ (tryLinking fn hash subdir) cache
    return hash
    `catchall`
    wfuc cache
    `catchall`
    fail ("Couldn't write " ++ hash ++ "\nin subdir "
               ++ hashedDir subdir ++ " to sources:\n\n"++ show (Ca cache))
  where
    hash = if subdir == HashedPristineDir then sha256sum ps else cacheHash ps
    wfuc (c : cs)
        | not $ writable c = wfuc cs
        | otherwise = do
            createCache c subdir hash
            let cacheFile = hashedFilePath c subdir hash
            write compr cacheFile ps
            -- create links in all other writable locations
            debugMessage $ "writeFileUsingCache remaining sources:\n"++show (Ca cs)
            mapM_ (tryLinking cacheFile hash subdir) cs
            return hash
    wfuc [] = fail $ "No location to write file " ++ (hashedDir subdir </> hash)

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
