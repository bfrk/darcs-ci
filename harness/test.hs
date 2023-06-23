{-# LANGUAGE CPP, MultiParamTypeClasses, DeriveDataTypeable, ViewPatterns, OverloadedStrings, ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Main ( main, run, defaultConfig, Config(..) ) where

import Darcs.Prelude

import qualified Darcs.Test.Misc
import qualified Darcs.Test.Patch
import qualified Darcs.Test.Email
import qualified Darcs.Test.Repository.Inventory
import qualified Darcs.Test.HashedStorage
import qualified Darcs.Test.UI
import Darcs.Util.Exception ( die )

import Control.Monad ( filterM )
import Control.Exception ( SomeException )
import Data.Text ( Text, pack, unpack )
import qualified Data.Text as T
import Data.List ( isPrefixOf, isSuffixOf, sort )
import GHC.IO.Encoding ( textEncodingName )
import System.Console.CmdArgs hiding ( args )
import System.Console.CmdArgs.Explicit ( process )
import System.Directory ( doesFileExist )
import System.Environment.FindBin ( getProgPath )
import System.FilePath( takeDirectory, takeBaseName, isAbsolute, makeRelative )
import qualified System.FilePath as Native ( searchPathSeparator, splitSearchPath )
import qualified System.FilePath.Posix as Posix ( searchPathSeparator )
import System.IO( hSetBinaryMode, hSetBuffering, BufferMode( NoBuffering ), stdout, localeEncoding )
import Test.Framework.Providers.API
  ( TestResultlike(..), Testlike(..), Test, runImprovingIO, yieldImprovement, Test(..), liftIO )
import Test.Framework ( defaultMainWithOpts, RunnerOptions'(..), TestOptions'(..)
  , ColorMode(..), Seed(..) )
import Shelly hiding ( liftIO, run, FilePath, path )
import qualified Shelly

doUnit :: IO [Test]
doUnit = return unitTests

-- | TODO make runnable in parallel
doHashed :: IO [Test]
doHashed = return Darcs.Test.HashedStorage.tests

-- | This is the big list of tests that will be run using testrunner.
unitTests :: [Test]
unitTests =
  [ Darcs.Test.Email.testSuite
  , Darcs.Test.Misc.testSuite
  , Darcs.Test.Repository.Inventory.testSuite
  , Darcs.Test.UI.testSuite
  ] ++ Darcs.Test.Patch.testSuite

-- ----------------------------------------------------------------------
-- shell tests
-- ----------------------------------------------------------------------

data Format = Darcs1 | Darcs2 | Darcs3 deriving (Show, Eq, Typeable, Data)
data DiffAlgorithm = Myers | Patience deriving (Show, Eq, Typeable, Data)
data UseIndex = NoIndex | WithIndex deriving (Show, Eq, Typeable, Data)
data UseCache = NoCache | WithCache deriving (Show, Eq, Typeable, Data)
data Running = Running deriving Show
data Result = Success | Skipped | Failed String

instance Show Result where
  show Success = "Success"
  show Skipped = "Skipped"
  show (Failed f) = unlines (map ("| " ++) $ lines f)

instance TestResultlike Running Result where
  testSucceeded Success = True
  testSucceeded Skipped = True
  testSucceeded _ = False

data ShellTest = ShellTest { format :: Format
                           , testfile :: FilePath
                           , testdir  :: Maybe FilePath -- ^ only if you want to set it explicitly
                           , _darcspath :: FilePath
                           , diffalgorithm :: DiffAlgorithm
                           , useindex :: UseIndex
                           , usecache :: UseCache
                           }
                 deriving Typeable

-- |Environment variable values may need translating depending
-- on whether we are setting them directly or writing out a shell script
-- to set them, and depending on the kind of value and the platform.
-- This type captures the different kinds of values.
data EnvItem
  = EnvString String  -- ^ A normal string that won't need conversion
  | EnvFilePath Shelly.FilePath -- ^ A path on disk that may need conversion for the platform
  | EnvSearchPath [Shelly.FilePath] -- ^ A list of paths on disk, for the PATH variable

runtest' :: ShellTest -> Text -> Sh Result
runtest' (ShellTest fmt _ _ dp da ui uc) srcdir =
  do wd <- pwd
     p <- unpack <$> get_env_text "PATH"
     let pathToUse = map (fromText . pack) $ takeDirectory dp:Native.splitSearchPath p
     let env =
          [ ("HOME", EnvFilePath wd)
          -- in case someone has XDG_CACHE_HOME set:
          , ("XDG_CACHE_HOME", EnvFilePath (wd </> ".cache"))
          , ("TESTDATA", EnvFilePath (srcdir </> "tests" </> "data"))
          , ("TESTBIN", EnvFilePath (srcdir </> "tests" </> "bin"))
          , ("DARCS_TESTING_PREFS_DIR", EnvFilePath $ wd </> ".darcs")
          , ("EMAIL", EnvString "tester")
          , ("GIT_AUTHOR_NAME", EnvString "tester")
          , ("GIT_AUTHOR_EMAIL", EnvString "tester")
          , ("GIT_COMMITTER_NAME", EnvString "tester")
          , ("GIT_COMMITTER_EMAIL", EnvString "tester")
          , ("DARCS_DONT_COLOR", EnvString "1")
          , ("DARCS_DONT_ESCAPE_ANYTHING", EnvString "1")
          , ("PATH", EnvSearchPath pathToUse)
          -- the DARCS variable is passed to the tests purely so they can
          -- double-check that the darcs on the path is the expected one,
          -- so is passed as a string directly without any translation
          , ("DARCS", EnvString dp)
          , ("GHC_VERSION", EnvString $ show (__GLASGOW_HASKELL__ :: Int))
          ]
     -- we write the variables to a shell script and source them from there in ./lib,
     -- so that it's easy to reproduce a test failure after running the harness with -d.
     writefile "env" $ T.unlines $
        map (\(k,v) -> T.concat ["export ", k, "=", envItemForScript v]) env
     -- just in case the test script doesn't source ./lib:
     mapM_ (\(k,v) -> setenv k (envItemForEnv v)) env

     mkdir ".darcs"
     writefile ".darcs/defaults" defaults
     _ <- onCommandHandles (initOutputHandles (\h -> hSetBinaryMode h True)) $
          Shelly.run "bash" [ "test" ]
     return Success
   `catch_sh` \(_::SomeException)
                 -> do code <- lastExitCode
                       case code of
                        200 -> return Skipped
                        _   -> Failed <$> unpack <$> lastStderr
  where defaults = pack $ unlines $
          [ "ALL " ++ fmtstr
          , "send no-edit-description"
          , "ALL " ++ uif
          , "ALL " ++ daf
          ]
          ++ ucf
        fmtstr = case fmt of
                  Darcs3 -> "darcs-3"
                  Darcs2 -> "darcs-2"
                  Darcs1 -> "darcs-1"
        daf = case da of
                Patience -> "patience"
                Myers -> "myers"
        uif = case ui of
                WithIndex -> "no-ignore-times"
                NoIndex -> "ignore-times"
        ucf = case uc of
                WithCache -> []
                NoCache -> ["ALL no-cache"]

        -- convert an 'EnvItem' to a string you can put in the environment directly
        envItemForEnv :: EnvItem -> Text
        envItemForEnv (EnvString v) = pack v
        envItemForEnv (EnvFilePath v) = toTextIgnore v
        envItemForEnv (EnvSearchPath vs) =
          T.intercalate (T.singleton Native.searchPathSeparator) $ map toTextIgnore vs

        -- convert an 'EnvItem' to a string that will evaluate to the right value
        -- when embedded in a bash script
        envItemForScript :: EnvItem -> Text
        envItemForScript (EnvString v) = quoteForShell (pack v)
        envItemForScript (EnvFilePath v) = filePathForScript v
        envItemForScript (EnvSearchPath vs) =
           -- note the use of the Posix search path separator (':') regardless of platform
           T.intercalate (T.singleton Posix.searchPathSeparator) $ map filePathForScript vs

        -- add quotes around a 'Shelly.FilePath'
        quotedFilePath :: Shelly.FilePath -> Text
        quotedFilePath = quoteForShell . toTextIgnore

        quoteForShell :: Text -> Text
        quoteForShell = surround '\'' . T.replace "'" "'\\''"
          where surround c t = T.cons c $ T.snoc t c

        -- convert a 'Shelly.FilePath' into a string that will evaluate to the right
        -- value when put in a bash script
        filePathForScript :: Shelly.FilePath -> Text
#ifdef WIN32
        -- we have a native Windows path, but we are going to put it in an bash script
        -- run in an environment like msys2 which works with an illusion of a Unix style
        -- filesystem. Calling cygpath at runtime does the necessary translation.
        filePathForScript v = T.concat ["$(cygpath ", quotedFilePath v, ")"]
#else
        filePathForScript v = quotedFilePath v
#endif

takeTestName :: FilePath -> Shelly.FilePath
takeTestName n =
  let n' = makeRelative "tests" n in
    takeBaseName (takeDirectory n') </> takeBaseName n'

runtest :: ShellTest -> Sh Result
runtest t =
 withTmp $ \dir -> do
  cp "tests/lib" dir
  cp "tests/network/sshlib" dir
  cp "tests/network/httplib" dir
  cp (fromText $ pack $ testfile t) (dir </> "test")
  srcdir <- pwd
  silently $ sub $ cd dir >> runtest' t (toTextIgnore srcdir)
 where
  withTmp =
   case testdir t of
     Just dir -> \job -> do
       let d = dir </> show (format t) </> show (diffalgorithm t) </>
               show (useindex t) </> show (usecache t) </> takeTestName (testfile t)
       mkdir_p d
       job d
     Nothing  -> withTmpDir

instance Testlike Running Result ShellTest where
  testTypeName _ = "Shell"
  runTest _ test = runImprovingIO $ do yieldImprovement Running
                                       liftIO (shelly $ runtest test)

shellTest :: FilePath -> Format -> Maybe FilePath -> String -> DiffAlgorithm -> UseIndex -> UseCache ->Test
shellTest dp fmt tdir file da ui uc =
  Test
    (toString (takeTestName file) ++
     " (" ++ show fmt ++ "," ++ show da ++ "," ++ show ui ++ "," ++ show uc ++ ")") $
  ShellTest fmt file tdir dp da ui uc

toString :: Shelly.FilePath -> String
toString = unpack . toTextIgnore

findShell :: FilePath -> Text -> Maybe FilePath -> Bool -> [DiffAlgorithm] -> [Format] -> [UseIndex] -> [UseCache] -> Sh [Test]
findShell dp sdir tdir isFailing diffAlgorithms repoFormats useidx usecaches =
  do files <- ls (fromText sdir)
     let test_files = sort $ filter relevant $ filter (hasExt "sh") files
     return [ shellTest dp fmt tdir file da ui uc
            | file <- map toString test_files
            , fmt <- repoFormats
            , da <- diffAlgorithms
            , ui <- useidx
            , uc <- usecaches ]
  where relevant = (if isFailing then id else not) . ("failing-" `isPrefixOf`) . takeBaseName . toString

-- ----------------------------------------------------------------------
-- harness
-- ----------------------------------------------------------------------

data Config = Config { suites :: String
                     , formats :: String
                     , diffalgs :: String
                     , index :: String
                     , cache :: String
                     , full :: Bool
                     , darcs :: String
                     , tests :: [String]
                     , testDir :: Maybe FilePath
                     , plain :: Bool
                     , hideSuccesses :: Bool
                     , threads :: Int
                     , qcCount :: Int
                     , replay :: Maybe Int
                     }
            deriving (Data, Typeable, Eq, Show)


defaultConfigAnn :: Annotate Ann
defaultConfigAnn
 = record Config{}
     [ suites        := "snu"    += help "Select which test suites to run: (s=shell, n=network, u=unit, f=failing, h=hashed) [snu]" += typ "SET"
     , formats       := "123"    += help "Select which darcs formats to test: (1=darcs-1, 2=darcs-2, 3=darcs-3) [123]" += name "f" += typ "SET"
     , diffalgs      := "p"      += help "Select which diff alorithms to use (p=patience, m=myers) [p]" += name "a" += typ "SET"
     , index         := "y"      += help "Select whether to use the index (n=no, y=yes) [y]" += typ "SET"
     , cache         := "y"      += help "Select whether to use the cache (n=no, y=yes) [y]" += typ "SET"
     , full          := False    += help "Shortcut for -s=snu -f=123 -a=mp -c=yn -i=yn"
     , darcs         := ""       += help "Darcs binary path" += typ "PATH"
     , tests         := []       += help "Pattern to limit the tests to run" += typ "PATTERN" += name "t"
     , testDir       := Nothing  += help "Directory to run tests in" += typ "PATH" += name "d"
     , plain         := False    += help "Use plain-text output [no]"
     , hideSuccesses := False    += help "Hide successes [no]"
     , threads       := 1        += help "Number of threads [1]" += name "j"
     , qcCount       := 100      += help "Number of QuickCheck iterations per test [100]" += name "q"
     , replay        := Nothing  += help "Replay QC tests with given seed" += typ "SEED"
     ]
   += summary "Darcs test harness"
   += program "darcs-test"

defaultConfig :: Config
defaultConfig =
  case process (cmdArgsMode_ defaultConfigAnn) [] of
    Right r -> cmdArgsValue r
    Left _ -> error "impossible"

run :: Config -> IO ()
run conf = do
    case testDir conf of
       Nothing -> return ()
       Just d  -> do e <- shelly (test_e (fromText $ pack d))
                     when e $ die ("Directory " ++ d ++ " already exists. Cowardly exiting")
    darcsBin <-
        case darcs conf of
            "" -> do
                path <- getProgPath
                let candidates =
                      -- if darcs-test lives in foo/something, look for foo/darcs[.exe]
                      -- for example if we've done cabal install -ftest, there'll be a darcs-test and darcs in the cabal
                      -- installation folder
                      [path </> ("darcs" ++ exeSuffix)] ++
                      -- if darcs-test lives in foo/darcs-test/something, look for foo/darcs/darcs[.exe]
                      -- for example after cabal build we can run dist/build/darcs-test/darcs-test and it'll find
                      -- the darcs in dist/build/darcs/darcs
                      [takeDirectory path </> "darcs" </> ("darcs" ++ exeSuffix) | takeBaseName path == "darcs-test" ] ++
                      -- nowadays cabal v2-build produces more complicated structures:
                      -- t/darcs-test/build/darcs-test/darcs-test and x/darcs/build/darcs/darcs
                      [takeDirectory path </> ".." </> ".." </> ".." </> "x"
                                          </> "darcs" </> "build" </> "darcs" </> ("darcs" ++ exeSuffix)
                            | takeBaseName path == "darcs-test" ] ++
                      [takeDirectory path </> ".." </> ".." </> ".." </> ".." </> "x"
                                          </> "darcs" </> "noopt" </> "build" </> "darcs" </> ("darcs" ++ exeSuffix)
                            | takeBaseName path == "darcs-test" ]
                availableCandidates <- filterM doesFileExist (map toString candidates)
                case availableCandidates of
                     (darcsBin:_) -> do
                         putStrLn $ "Using darcs executable in " ++ takeDirectory darcsBin
                         return darcsBin
                     [] -> die ("No darcs specified or found nearby. Tried:\n" ++ unlines (map toString candidates))
            v -> return v

    let hashed   = 'h' `elem` suites conf
        failing  = 'f' `elem` suites conf
        shell    = 's' `elem` suites conf
        network  = 'n' `elem` suites conf
        unit     = 'u' `elem` suites conf

        darcs1   = '1' `elem` formats conf
        darcs2   = '2' `elem` formats conf
        darcs3   = '3' `elem` formats conf

        myers    = 'm' `elem` diffalgs conf
        patience = 'p' `elem` diffalgs conf

        noindex   = 'n' `elem` index conf
        withindex = 'y' `elem` index conf

        nocache   = 'n' `elem` cache conf
        withcache = 'y' `elem` cache conf

    when (shell || network || failing) $ do
      unless (isAbsolute $ darcsBin) $
        die ("Argument to --darcs should be an absolute path")
      unless (exeSuffix `isSuffixOf` darcsBin) $
        putStrLn $ "Warning: --darcs flag does not end with " ++ exeSuffix ++ " - some tests may fail (case does matter)"

    putStrLn $ "Locale encoding is " ++ textEncodingName localeEncoding

    let repoFormat    = (if darcs1 then (Darcs1:) else id)
                      . (if darcs2 then (Darcs2:) else id)
                      . (if darcs3 then (Darcs3:) else id)
                      $ []
    let diffAlgorithm = (if myers then (Myers:) else id)
                      . (if patience then (Patience:) else id)
                      $ []
    let useIndex      = (if noindex then (NoIndex:) else id)
                      . (if withindex then (WithIndex:) else id)
                      $ []
    let useCache      = (if nocache then (NoCache:) else id)
                      . (if withcache then (WithCache:) else id)
                      $ []

    stests <- shelly $
      if shell
        then findShell darcsBin "tests" (testDir conf) failing diffAlgorithm repoFormat useIndex useCache
        else return []
    utests <- if unit then doUnit else return []
    ntests <- shelly $
      if network
        then findShell darcsBin "tests/network" (testDir conf) failing diffAlgorithm repoFormat useIndex useCache
        else return []
    hstests <- if hashed then doHashed else return []
    let testRunnerOptions = RunnerOptions
          { ropt_threads = Just (threads conf)
          , ropt_test_options = Just $ TestOptions
              { topt_seed = FixedSeed <$> replay conf
              , topt_maximum_generated_tests = Just (qcCount conf)
              , topt_maximum_unsuitable_generated_tests = Just (7 * qcCount conf)
              , topt_maximum_test_size = Nothing
              , topt_maximum_test_depth = Nothing
              , topt_timeout = Nothing
              }
          , ropt_test_patterns = if null (tests conf) then Nothing else Just (map read (tests conf))
          , ropt_xml_output = Nothing
          , ropt_xml_nested = Nothing
          , ropt_color_mode = if plain conf then Just ColorNever else Nothing
          , ropt_hide_successes = Just (hideSuccesses conf)
          , ropt_list_only = Nothing
          }
    defaultMainWithOpts (stests ++ utests ++ ntests ++ hstests) testRunnerOptions
       where
          exeSuffix :: String
#ifdef WIN32
          exeSuffix = ".exe"
#else
          exeSuffix = ""
#endif

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          clp  <- cmdArgs_ defaultConfigAnn
          run $
            if full clp then clp
              { suites   = "snu" -- not with 'h' because they arent' re-entrant
              , formats  = "123"
              , diffalgs = "mp"
              , index = "yn"
              , cache = "yn"
              }
            else clp
