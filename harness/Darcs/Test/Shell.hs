{-# LANGUAGE CPP, OverloadedStrings, ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Darcs.Test.Shell
    ( Format(..)
    , DiffAlgorithm(..)
    , UseIndex(..)
    , UseCache(..)
    , findShell
    ) where

import Darcs.Prelude

import Control.Exception ( SomeException )
import Data.Data ( Data, Typeable )
import Data.Text ( Text, pack, unpack )
import qualified Data.Text as T
import qualified Shelly ( FilePath, run )
import Shelly
    ( Sh
    , catch_sh
    , cd
    , cp
    , fromText
    , get_env_text
    , initOutputHandles
    , lastExitCode
    , lastStderr
    , mkdir
    , mkdir_p
    , onCommandHandles
    , pwd
    , setenv
    , shelly
    , silently
    , sub
    , toTextIgnore
    , withTmpDir
    , writefile
    , (</>)
    )
import qualified System.FilePath as Native ( searchPathSeparator, splitSearchPath )
import System.FilePath ( makeRelative, takeBaseName, takeDirectory )
import qualified System.FilePath.Posix as Posix ( searchPathSeparator )
import System.IO ( hSetBinaryMode )
import Test.Framework.Providers.API
    ( Test(..)
    , TestResultlike(..)
    , Testlike(..)
    , liftIO
    , runImprovingIO
    , yieldImprovement
    )

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

data ShellTest = ShellTest
  { format :: Format
  , testfile :: FilePath
  , testdir :: Maybe FilePath -- ^ only if you want to set it explicitly
  , darcspath :: FilePath
  , diffalgorithm :: DiffAlgorithm
  , useindex :: UseIndex
  , usecache :: UseCache
  } deriving (Typeable)

instance Testlike Running Result ShellTest where
  testTypeName _ = "Shell"
  runTest _ test =
    runImprovingIO $ do
      yieldImprovement Running
      liftIO (shelly $ runtest test)

-- | Environment variable values may need translating depending on whether we
-- are setting them directly or writing out a shell script to set them, and
-- depending on the kind of value and the platform. This type captures the
-- different kinds of values.
data EnvItem
  = EnvString String
    -- ^ A normal string that won't need conversion
  | EnvFilePath Shelly.FilePath
    -- ^ A path on disk that may need conversion for the platform
  | EnvSearchPath [Shelly.FilePath]
    -- ^ A list of paths on disk, for the PATH variable

runtest' :: ShellTest -> Text -> Sh Result
runtest' (ShellTest fmt _ _ dp da ui uc) srcdir =
  do
    wd <- pwd
    p  <- unpack <$> get_env_text "PATH"
    let pathToUse =
          map (fromText . pack) $ takeDirectory dp : Native.splitSearchPath p
    let env =
          [ ("HOME"                      , EnvFilePath wd)
         -- in case someone has XDG_CACHE_HOME set:
          , ("XDG_CACHE_HOME"            , EnvFilePath (wd </> ".cache"))
          , ("TESTDATA", EnvFilePath (srcdir </> "tests" </> "data"))
          , ("TESTBIN", EnvFilePath (srcdir </> "tests" </> "bin"))
          , ("DARCS_TESTING_PREFS_DIR"   , EnvFilePath $ wd </> ".darcs")
          , ("EMAIL"                     , EnvString "tester")
          , ("GIT_AUTHOR_NAME"           , EnvString "tester")
          , ("GIT_AUTHOR_EMAIL"          , EnvString "tester")
          , ("GIT_COMMITTER_NAME"        , EnvString "tester")
          , ("GIT_COMMITTER_EMAIL"       , EnvString "tester")
          , ("DARCS_DONT_COLOR"          , EnvString "1")
          , ("DARCS_DONT_ESCAPE_ANYTHING", EnvString "1")
          , ("PATH"                      , EnvSearchPath pathToUse)
         -- the DARCS variable is passed to the tests purely so they can
         -- double-check that the darcs on the path is the expected one,
         -- so is passed as a string directly without any translation
          , ("DARCS"                     , EnvString dp)
          , ("GHC_VERSION", EnvString $ show (__GLASGOW_HASKELL__ :: Int))
         -- https://www.joshkel.com/2018/01/18/symlinks-in-windows/
          , ("MSYS"                      , EnvString "winsymlinks:nativestrict")
          ]
    -- we write the variables to a shell script and source them from there in
    -- ./lib, so that it's easy to reproduce a test failure after running the
    -- harness with -d.
    writefile "env" $ T.unlines $ map
      (\(k, v) -> T.concat ["export ", k, "=", envItemForScript v])
      env
    -- just in case the test script doesn't source ./lib:
    mapM_ (\(k, v) -> setenv k (envItemForEnv v)) env

    mkdir ".darcs"
    writefile ".darcs/defaults" defaults
    _ <-
      onCommandHandles (initOutputHandles (\h -> hSetBinaryMode h True))
        $ Shelly.run "bash" ["test"]
    return Success
  `catch_sh` \(_ :: SomeException) -> do
               code <- lastExitCode
               case code of
                 200 -> return Skipped
                 _   -> Failed <$> unpack <$> lastStderr
  where
    defaults =
      pack
        $  unlines
        $  [ "ALL " ++ fmtstr
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
      Myers    -> "myers"
    uif = case ui of
      WithIndex -> "no-ignore-times"
      NoIndex   -> "ignore-times"
    ucf = case uc of
      WithCache -> []
      NoCache   -> ["ALL no-cache"]

    -- convert an 'EnvItem' to a string you can put in the environment directly
    envItemForEnv :: EnvItem -> Text
    envItemForEnv (EnvString   v) = pack v
    envItemForEnv (EnvFilePath v) = toTextIgnore v
    envItemForEnv (EnvSearchPath vs) =
      T.intercalate (T.singleton Native.searchPathSeparator) $ map toTextIgnore vs

    -- convert an 'EnvItem' to a string that will evaluate to the right value
    -- when embedded in a bash script
    envItemForScript :: EnvItem -> Text
    envItemForScript (EnvString   v) = quoteForShell (pack v)
    envItemForScript (EnvFilePath v) = filePathForScript v
    envItemForScript (EnvSearchPath vs) =
      -- note the use of the Posix search path separator (':') regardless of platform
      T.intercalate (T.singleton Posix.searchPathSeparator)
        $ map filePathForScript vs

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
    -- we have a native Windows path, but we are going to put it in an bash
    -- script run in an environment like msys2 which works with an illusion
    -- of a Unix style filesystem. Calling cygpath at runtime does the
    -- necessary translation.
    filePathForScript v = T.concat ["$(cygpath ", quotedFilePath v, ")"]
#else
    filePathForScript v = quotedFilePath v
#endif

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
        Just dir ->
          \job -> do
            let d =
                  dir </> show (format t) </> show (diffalgorithm t) </>
                  show (useindex t) </>
                  show (usecache t) </>
                  takeTestName (testfile t)
            mkdir_p d
            job d
        Nothing -> withTmpDir

findShell
  :: FilePath
  -> [FilePath]
  -> Maybe FilePath
  -> [DiffAlgorithm]
  -> [Format]
  -> [UseIndex]
  -> [UseCache]
  -> IO [Test]
findShell dp files tdir diffAlgorithms repoFormats useidx usecaches =
  do
    return
      [ shellTest
          ShellTest
            { format = fmt
            , testfile = file
            , testdir = tdir
            , darcspath = dp
            , diffalgorithm = da
            , useindex = ui
            , usecache = uc
            }
      | file <- files
      , fmt <- repoFormats
      , da <- diffAlgorithms
      , ui <- useidx
      , uc <- usecaches
      ]

shellTest :: ShellTest -> Test
shellTest test =
    Test name test
  where
    name =
      concat
        [ toString (takeTestName (testfile test))
        , " ("
        , show (format test)
        , ","
        , show (diffalgorithm test)
        , ","
        , show (useindex test)
        , ","
        , show (usecache test)
        , ")"
        ]

takeTestName :: FilePath -> Shelly.FilePath
takeTestName n =
  let n' = makeRelative "tests" n
   in takeBaseName (takeDirectory n') </> takeBaseName n'

toString :: Shelly.FilePath -> String
toString = unpack . toTextIgnore
