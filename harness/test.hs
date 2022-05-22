{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module Main ( main, run, defaultConfig, Config(..) ) where

import Darcs.Prelude

import qualified Darcs.Test.Email
import qualified Darcs.Test.HashedStorage
import qualified Darcs.Test.Misc
import qualified Darcs.Test.Patch
import qualified Darcs.Test.Repository.Inventory
import Darcs.Test.Shell
import qualified Darcs.Test.UI
import Darcs.Util.Exception ( die )

import Control.Monad ( filterM, unless, when )
import Data.List ( isSuffixOf )
import GHC.IO.Encoding ( textEncodingName )
import System.Console.CmdArgs hiding ( args )
import System.Console.CmdArgs.Explicit ( process )
import System.Directory ( doesFileExist, doesPathExist, exeExtension )
import System.Environment.FindBin ( getProgPath )
import System.FilePath ( isAbsolute, takeBaseName, takeDirectory, (</>) )
import System.IO ( BufferMode(NoBuffering), hSetBuffering, localeEncoding, stdout )
import Test.Framework
    ( ColorMode(..)
    , RunnerOptions'(..)
    , Seed(..)
    , TestOptions'(..)
    , defaultMainWithOpts
    )

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
     , full          := False    += help "Shortcut for -s=snu -f=123 -a=mp -i=yn"
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
Right defaultConfig = fmap cmdArgsValue $ process (cmdArgsMode_ defaultConfigAnn) []

run :: Config -> IO ()
run conf = do
    case testDir conf of
       Nothing -> return ()
       Just d  -> do e <- doesPathExist d -- shelly (test_e (fromText $ T.pack d))
                     when e $ die ("Directory " ++ d ++ " already exists. Cowardly exiting")
    darcsBin <-
        case darcs conf of
            "" -> do
                path <- getProgPath
                let candidates =
                      -- if darcs-test lives in foo/something, look for foo/darcs[.exe]
                      -- for example if we've done cabal install -ftest, there'll be a darcs-test and darcs in the cabal
                      -- installation folder
                      [path </> ("darcs" ++ exeExtension)] ++
                      -- if darcs-test lives in foo/darcs-test/something, look for foo/darcs/darcs[.exe]
                      -- for example after cabal build we can run dist/build/darcs-test/darcs-test and it'll find
                      -- the darcs in dist/build/darcs/darcs
                      [takeDirectory path </> "darcs" </> ("darcs" ++ exeExtension) | takeBaseName path == "darcs-test" ] ++
                      -- nowadays cabal v2-build produces more complicated structures:
                      -- t/darcs-test/build/darcs-test/darcs-test and x/darcs/build/darcs/darcs
                      [takeDirectory path </> ".." </> ".." </> ".." </> "x"
                                          </> "darcs" </> "build" </> "darcs" </> ("darcs" ++ exeExtension)
                            | takeBaseName path == "darcs-test" ] ++
                      [takeDirectory path </> ".." </> ".." </> ".." </> ".." </> "x"
                                          </> "darcs" </> "noopt" </> "build" </> "darcs" </> ("darcs" ++ exeExtension)
                            | takeBaseName path == "darcs-test" ]
                availableCandidates <- filterM doesFileExist candidates
                case availableCandidates of
                     (darcsBin:_) -> do
                         putStrLn $ "Using darcs executable in " ++ takeDirectory darcsBin
                         return darcsBin
                     [] -> die ("No darcs specified or found nearby. Tried:\n" ++ unlines candidates)
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
      unless (exeExtension `isSuffixOf` darcsBin) $
        putStrLn $ "Warning: --darcs flag does not end with " ++ exeExtension ++ " - some tests may fail (case does matter)"

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

    stests <-
      if shell
        then findShell darcsBin "tests" (testDir conf) failing diffAlgorithm repoFormat useIndex useCache
        else return []
    ntests <-
      if network
        then findShell darcsBin "tests/network" (testDir conf) failing diffAlgorithm repoFormat useIndex useCache
        else return []
    let utests =
          if unit then
            [ Darcs.Test.Email.testSuite
            , Darcs.Test.Misc.testSuite
            , Darcs.Test.Repository.Inventory.testSuite
            , Darcs.Test.UI.testSuite
            ] ++
            Darcs.Test.Patch.testSuite
          else []
        hstests = if hashed then Darcs.Test.HashedStorage.tests else []

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

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          clp  <- cmdArgs_ defaultConfigAnn
          run $
            if full clp then clp
              { suites   = "snuh"
              , formats  = "123"
              , diffalgs = "mp"
              , index = "yn"
              , cache = "yn"
              }
            else clp
