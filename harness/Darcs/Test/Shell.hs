{-# LANGUAGE CPP, OverloadedStrings, ExtendedDefaultRules, RecordWildCards #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Darcs.Test.Shell
    ( Format(..)
    , DiffAlgorithm(..)
    , UseIndex(..)
    , UseCache(..)
    , genShellTests
    ) where

import Darcs.Prelude
import Darcs.Repository.Prefs ( globalCacheDir )

import Control.Exception ( SomeException )
import Data.List ( intercalate )
import Data.Maybe ( fromJust )
import Data.Tagged ( Tagged(..) )
import Data.Text ( Text, pack, unpack )
import qualified Data.Text as T
import Data.Typeable ( Typeable )
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
    , liftIO
    , mkdir
    , mkdir_p
    , onCommandHandles
    , pwd
    , shelly
    , silently
    , sub
    , toTextIgnore
    , withTmpDir
    , writefile
    , (</>)
    )
import qualified System.FilePath as Native ( splitSearchPath )
import System.FilePath ( makeRelative, takeBaseName, takeDirectory )
import qualified System.FilePath.Posix as Posix ( searchPathSeparator )
import System.IO ( hSetBinaryMode )
import Test.Tasty.Options ( IsOption(..) )
import Test.Tasty ( testGroup )
import Test.Tasty.Providers
    ( IsTest(..)
    , TestTree
    , singleTest
    , testFailed
    , testPassed
    )

data Format = Darcs1 | Darcs2 | Darcs3 deriving (Show, Eq, Typeable)
data DiffAlgorithm = Myers | Patience deriving (Show, Eq, Typeable)
data UseIndex = NoIndex | WithIndex deriving (Show, Eq, Typeable)
data UseCache = NoCache | WithCache deriving (Show, Eq, Typeable)

data ShellTest = ShellTest
  { format :: Format
  , testfile :: FilePath
  , testdir :: Maybe FilePath -- ^ only if you want to set it explicitly
  , darcspath :: FilePath
  , ghcflags :: String
  , diffalgorithm :: DiffAlgorithm
  , useindex :: UseIndex
  , usecache :: UseCache
  } deriving (Typeable)

data Running = Running deriving Show
data Result = Success | Skipped | Failed String

newtype TestDir = TestDir (Maybe FilePath)
instance IsOption TestDir where
  defaultValue = TestDir Nothing
  parseValue s = Just (TestDir (Just s))
  optionName = Tagged "d"
  optionHelp = Tagged "Directory to run tests in"

instance IsTest ShellTest where
  testOptions = Tagged []
  run _opts test _progress = resultToTasty <$> shelly (runtest test)
    where
      resultToTasty Success = testPassed ""
      resultToTasty Skipped = testPassed "Skipped"
      resultToTasty (Failed msg) = testFailed msg

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
runtest' ShellTest{..} srcdir =
  do
    wd <- pwd
    p  <- unpack <$> get_env_text "PATH"
    cacheDir <- liftIO globalCacheDir
    let pathToUse =
          map (fromText . pack) $ takeDirectory darcspath : Native.splitSearchPath p
    let env =
          [ ("HOME"                      , EnvFilePath wd)
          -- in case someone has XDG_CACHE_HOME set:
          , ("XDG_CACHE_HOME"            , EnvFilePath (wd </> ".cache"))
          , ("TESTDATA", EnvFilePath (srcdir </> "tests" </> "data"))
          , ("TESTBIN", EnvFilePath (srcdir </> "tests" </> "bin"))
          , ("DARCS_TESTING_PREFS_DIR"   , EnvFilePath $ wd </> ".darcs")
          , ("DARCS_TESTING_CACHE_DIR"   , EnvFilePath $ fromJust cacheDir)
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
          , ("DARCS"                     , EnvString darcspath)
          , ("GHC_FLAGS"                 , EnvString ghcflags)
          , ("GHC_VERSION", EnvString $ show (__GLASGOW_HASKELL__ :: Int))
          -- https://www.joshkel.com/2018/01/18/symlinks-in-windows/
          , ("MSYS"                      , EnvString "winsymlinks:nativestrict")
#ifdef WIN32
          , ("OS"                        , EnvString "windows")
#endif
          ]
    -- we write the variables to a shell script and source them from there in
    -- ./lib, so that it's easy to reproduce a test failure after running the
    -- harness with -d.
    writefile "env" $ T.unlines $ map
      (\(k, v) -> T.concat ["export ", k, "=", envItemForScript v])
      env

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
    fmtstr = case format of
      Darcs3 -> "darcs-3"
      Darcs2 -> "darcs-2"
      Darcs1 -> "darcs-1"
    daf = case diffalgorithm of
      Patience -> "patience"
      Myers    -> "myers"
    uif = case useindex of
      WithIndex -> "no-ignore-times"
      NoIndex   -> "ignore-times"
    ucf = case usecache of
      WithCache -> []
      NoCache   -> ["ALL no-cache"]

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
runtest test@ShellTest{..} =
  withTmp $ \dir -> do
    cp "tests/lib" dir
    cp "tests/network/sshlib" dir
    cp "tests/network/httplib" dir
    cp (fromText $ pack $ testfile) (dir </> "test")
    srcdir <- pwd
    silently $ sub $ cd dir >> runtest' test (toTextIgnore srcdir)
  where
    withTmp =
      case testdir of
        Just dir ->
          \job -> do
            let d =
                  dir </> show format </> show diffalgorithm </>
                  show useindex </>
                  show usecache </>
                  takeTestName testfile
            mkdir_p d
            job d
        Nothing -> withTmpDir

genShellTests
  :: FilePath
  -> [FilePath]
  -> Maybe FilePath
  -> String
  -> [DiffAlgorithm]
  -> [Format]
  -> [UseIndex]
  -> [UseCache]
  -> [TestTree]
genShellTests dp files tdir ghcflags diffAlgorithms repoFormats useindexs usecaches =
  [ testGroup file
    [ singleTest variant
      ShellTest
        { format = fmt
        , testfile = file
        , testdir = tdir
        , darcspath = dp
        , ghcflags = ghcflags
        , diffalgorithm = da
        , useindex = ui
        , usecache = uc
        }
    | fmt <- repoFormats
    , da <- diffAlgorithms
    , ui <- useindexs
    , uc <- usecaches
    , let variant = intercalate "," [show fmt, show da, show ui, show uc]
    ]
  | file <- files
  ]

takeTestName :: FilePath -> Shelly.FilePath
takeTestName n =
  let n' = makeRelative "tests" n
   in takeBaseName (takeDirectory n') </> takeBaseName n'
