{-|
License : GPL-2

A set of functions to identify and find Darcs repositories
from a given @URL@ or a given filesystem path.
-}

module Darcs.Repository.Identify
    ( maybeIdentifyRepository -- exported for darcsden
    , identifyRepository
    , identifyRepositoryFor
    , IdentifyRepo(..) -- exported for darcsden
    , ReadingOrWriting(..)
    , findRepository
    , amInRepository
    , amNotInRepository
    , amInHashedRepository
    , seekRepo
    ) where

import Darcs.Prelude

import Darcs.Repository.Format ( tryIdentifyRepoFormat
                               , readProblem
                               , transferProblem
                               )
import System.Directory ( doesDirectoryExist
                        , setCurrentDirectory
                        , createDirectoryIfMissing
                        , doesFileExist
                        )
import System.IO ( hPutStrLn, stderr )
import System.IO.Error ( catchIOError )
import Data.Maybe ( fromMaybe )

import Darcs.Repository.Old ( oldRepoFailMsg )
import Darcs.Repository.Flags ( UseCache(..), WorkRepo (..) )
import Darcs.Util.Path
    ( toFilePath
    , ioAbsoluteOrRemote
    , toPath
    )
import Darcs.Util.Exception ( catchall )
import Darcs.Util.URL ( isValidLocalPath )
import Darcs.Util.Workaround
    ( getCurrentDirectory
    )
import Darcs.Repository.Paths
    ( hashedInventoryPath
    , oldCurrentDirPath
    , oldPristineDirPath
    )
import Darcs.Repository.Prefs ( getCaches )
import Darcs.Repository.InternalTypes
    ( AccessType(..)
    , PristineType(..)
    , Repository
    , mkRepo
    , repoFormat
    )
import Darcs.Util.Global ( darcsdir )

import System.Mem( performGC )

-- | The status of a given directory: is it a darcs repository?
data IdentifyRepo rt p wU wR
    = BadRepository String -- ^ looks like a repository with some error
    | NonRepository String -- ^ safest guess
    | GoodRepository (Repository rt p wU wR)

-- | Try to identify the repository at a given location, passed as a 'String'.
-- If the lcation is ".", then we assume we are identifying the local repository.
-- Otherwise we assume we are dealing with a remote repo, which could be a URL
-- or an absolute path.
maybeIdentifyRepository :: UseCache -> String -> IO (IdentifyRepo 'RO p wU wR)
maybeIdentifyRepository useCache "." =
    do darcs <- doesDirectoryExist darcsdir
       if not darcs
        then return (NonRepository $ "Missing " ++ darcsdir ++ " directory")
        else do
        repoFormatOrError <- tryIdentifyRepoFormat "."
        here <- ioAbsoluteOrRemote "."
        case repoFormatOrError of
          Left err -> return $ NonRepository err
          Right rf ->
              case readProblem rf of
              Just err -> return $ BadRepository err
              Nothing -> do pris <- identifyPristine
                            cs <- getCaches useCache Nothing
                            return $ GoodRepository $ mkRepo here rf pris cs
maybeIdentifyRepository useCache url' =
 do url <- ioAbsoluteOrRemote url'
    repoFormatOrError <- tryIdentifyRepoFormat (toPath url)
    case repoFormatOrError of
      Left e -> return $ NonRepository e
      Right rf -> case readProblem rf of
                  Just err -> return $ BadRepository err
                  Nothing ->  do cs <- getCaches useCache (Just url)
                                 return $ GoodRepository $ mkRepo url rf NoPristine cs

identifyPristine :: IO PristineType
identifyPristine =
    do pristine <- doesDirectoryExist oldPristineDirPath
       current  <- doesDirectoryExist oldCurrentDirPath
       hashinv  <- doesFileExist      hashedInventoryPath
       case (pristine || current, hashinv) of
           (False, False) -> return NoPristine
           (True,  False) -> return PlainPristine
           (False, True ) -> return HashedPristine
           _ -> fail "Multiple pristine trees."

-- | identifyRepository identifies the repo at 'url'. Warning:
-- you have to know what kind of patches are found in that repo.
identifyRepository :: UseCache -> String -> IO (Repository 'RO p wU wR)
identifyRepository useCache url =
    do er <- maybeIdentifyRepository useCache url
       case er of
         BadRepository s -> fail s
         NonRepository s -> fail s
         GoodRepository r -> return r

data ReadingOrWriting = Reading | Writing

-- | @identifyRepositoryFor repo url@ identifies (and returns) the repo at 'url',
-- but fails if it is not compatible for reading from and writing to.
identifyRepositoryFor :: ReadingOrWriting
                      -> Repository rt p wU wR
                      -> UseCache
                      -> String
                      -> IO (Repository 'RO p vR vU)
identifyRepositoryFor what us useCache them_loc = do
  them <- identifyRepository useCache them_loc
  case
    case what of
      Reading -> transferProblem (repoFormat them) (repoFormat us)
      Writing -> transferProblem (repoFormat us) (repoFormat them) 
    of
      Just e -> fail $ "Incompatibility with repository " ++ them_loc ++ ":\n" ++ e
      Nothing -> return them

amInRepository :: WorkRepo -> IO (Either String ())
amInRepository (WorkRepoDir d) =
  do
    setCurrentDirectory d
    status <- maybeIdentifyRepository YesUseCache "."
    case status of
      GoodRepository _ -> return (Right ())
      BadRepository  e -> return (Left $ "While " ++ d ++ " looks like a repository directory, we have a problem with it:\n" ++ e)
      NonRepository  _ -> return (Left "You need to be in a repository directory to run this command.")
  `catchIOError`
    \e -> return (Left (show e))
  
amInRepository _ =
  fromMaybe (Left "You need to be in a repository directory to run this command.") <$> seekRepo

amInHashedRepository :: WorkRepo -> IO (Either String ())
amInHashedRepository wd
 = do inrepo <- amInRepository wd
      case inrepo of
       Right _ -> do pristine <- identifyPristine
                     case pristine of
                       HashedPristine -> return (Right ())
                       _ -> return (Left oldRepoFailMsg)
       left    -> return left

-- | hunt upwards for the darcs repository
-- This keeps changing up one parent directory, testing at each
-- step if the current directory is a repository or not.
-- The result is:
--   Nothing, if no repository found
--   Just (Left errorMessage), if bad repository found
--   Just (Right ()), if good repository found.
-- WARNING this changes the current directory for good if matchFn succeeds
seekRepo :: IO (Maybe (Either String ()))
seekRepo = getCurrentDirectory >>= helper where
  helper startpwd = do
    status <- maybeIdentifyRepository YesUseCache "."
    case status of
      GoodRepository _ -> return . Just $ Right ()
      BadRepository e -> return . Just $ Left e
      NonRepository _ ->
        catchIOError
          (do cd <- toFilePath `fmap` getCurrentDirectory
              setCurrentDirectory ".."
              cd' <- toFilePath `fmap` getCurrentDirectory
              if cd' /= cd
                then helper startpwd
                else do
                  setCurrentDirectory startpwd
                  return Nothing)
          (\e -> do
             hPutStrLn stderr ("Warning: " ++ show e)
             return Nothing)

-- The performGC in this function is a workaround for a library/GHC bug,
-- http://hackage.haskell.org/trac/ghc/ticket/2924 -- (doesn't seem to be a
-- problem on fast machines, but virtual ones trip this from time to time)
amNotInRepository :: WorkRepo -> IO (Either String ())
amNotInRepository (WorkRepoDir d) = do
    createDirectoryIfMissing False d
       `catchall` (performGC >> createDirectoryIfMissing False d)
    -- note that the above could always fail
    setCurrentDirectory d
    amNotInRepository WorkRepoCurrentDir
amNotInRepository _ = do
       status <- maybeIdentifyRepository YesUseCache "."
       case status of
         GoodRepository _ -> return (Left "You may not run this command in a repository.")
         BadRepository e  -> return (Left $ "You may not run this command in a repository.\nBy the way, we have a problem with it:\n" ++ e)
         NonRepository _  -> return (Right ())

findRepository :: WorkRepo -> IO (Either String ())
findRepository workrepo =
  case workrepo of
    WorkRepoPossibleURL d | isValidLocalPath d -> do
      setCurrentDirectory d
      findRepository WorkRepoCurrentDir
    WorkRepoDir d -> do
      setCurrentDirectory d
      findRepository WorkRepoCurrentDir
    _ -> fromMaybe (Right ()) <$> seekRepo
  `catchIOError` \e ->
    return (Left (show e))
