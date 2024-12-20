-- vim:fdm=marker
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{- | Console reporter ingredient. Adapted to Darcs from
Test.Tasty.Providers.ConsoleReporter.

Differences to the original:

  * User customizable colors via environment variables, see 'getFormat'
  * Use terminal width (if available) to
    * truncate overlong test or group names
    * align test result and timings
    This means option AnsiTricks is not needed
  * Remove support for option Quiet
  * Connect test name and result with dots (easier to read with wide terminal)
  * Remove the hook to display the test pattern for failed tests
  * Do not change stdout to buffering

-}
module Darcs.Test.Util.ConsoleReporter
  ( consoleTestReporter
    -- re-exports
  , HideSuccesses(..)
  , UseColor(..)
  ) where

import Darcs.Prelude hiding ( EQ, fail )
import Darcs.Test.Util.ConsoleFormat ( ConsoleFormatType(..), getFormat )

import Control.Exception ( bracket_ )
import Control.Monad ( unless, void, when )
import Control.Monad.IO.Class ( liftIO )
import Control.Monad.Trans.Reader ( Reader, ask, runReader )
import Control.Monad.Trans.State ( evalStateT, get, modify, put )
import Data.Maybe ( isJust )
import Data.Monoid ( Any(..) )
import Data.Proxy ( Proxy(..) )
import Foreign.C.Types ( CInt(..) )
import System.Console.ANSI
    ( clearLine
    , cursorUpLine
    , hSupportsANSI
    , hSupportsANSIColor
    , hideCursor
    , setCursorColumn
    , showCursor
    )
import qualified System.Console.Terminal.Size as TS
import System.IO ( hFlush, stdout )
import Test.Tasty.Ingredients.ConsoleReporter
    ( HideSuccesses(..)
    , Statistics(..)
    , TestOutput(..)
    , UseColor(..)
    , computeStatistics
    , foldTestOutput
    , useColor
    , withConsoleFormat
    )
import Test.Tasty.Options ( OptionDescription(..), OptionSet, lookupOption )
import Test.Tasty.Providers ( TestName )
import Test.Tasty.Providers.ConsoleFormat ( ResultDetailsPrinter(..) )
import Test.Tasty.Runners
    ( Ap(..)
    , FailureReason(..)
    , Ingredient(..)
    , Outcome(..)
    , Result(..)
    , StatusMap
    , TestTree
    , Time
    , Traversal(..)
    , foldGroup
    , foldSingle
    , foldTestTree
    , formatMessage
    , resultSuccessful
    , trivialFold
    )
import Text.Printf ( printf )

type Level = Int

-- | Build the 'TestOutput' for a 'TestTree' and 'OptionSet'. The @colors@
-- ImplicitParam controls whether the output is colored.
--
-- @since 0.11.3
buildTestOutput
  :: (?colors::Bool) => Maybe Int -> OptionSet -> TestTree -> TestOutput
buildTestOutput width opts tree =
  let
    extraSpace = 13 -- for result and time
    -- Do not retain the reference to the tree more than necessary
    !alignment =
      case width of
        Just w -> w - extraSpace
        Nothing -> computeAlignment opts tree

    runSingleTest
      :: (?colors :: Bool)
      => OptionSet -> TestName -> t -> Ap (Reader Level) TestOutput
    runSingleTest _opts name _test = Ap $ do
      level <- ask

      let
        spaceForName = alignment - indentSize * level

        printTestName = do
          printf "%s%s"
            (indent level)
            (truncateName width (indentSize * level + extraSpace) name)
          hFlush stdout

        printTestResult result = do
          printf " %s " (replicate (spaceForName - stringWidth name) '.')

          rDesc <- formatMessage $ resultDescription result

          -- use an appropriate printing function
          let
            printFn =
              case resultOutcome result of
                Success -> ok
                Failure TestDepFailed -> skipped
                _ -> fail
            time = resultTime result
          printFn (resultShortDescription result)
          -- print time only if it's significant
          when (time >= 0.01) $
            printFn (printf " (%.2fs)" time)
          printFn "\n"

          when (not $ null rDesc) $
            (if resultSuccessful result then infoOk else infoFail) $
              printf "%s%s\n" (indent $ level + 1) (formatDesc (level+1) rDesc)
          case resultDetailsPrinter result of
            ResultDetailsPrinter action -> action level withConsoleFormat

      return $ PrintTest name printTestName printTestResult

    runGroup :: OptionSet -> TestName -> Ap (Reader Level) TestOutput -> Ap (Reader Level) TestOutput
    runGroup _opts name grp = Ap $ do
      level <- ask
      let
        printHeading =
          printf ("%s%s\n") (indent level) (truncateName width (indentSize * level) name)
        printBody = runReader (getApp grp) (level + 1)
      return $ PrintHeading name printHeading printBody

  in
    flip runReader 0 $ getApp $
      foldTestTree
        trivialFold
          { foldSingle = runSingleTest
          , foldGroup = runGroup
          }
          opts tree

-- }}}

--------------------------------------------------
-- TestOutput modes
--------------------------------------------------
-- {{{
consoleOutput :: TestOutput -> StatusMap -> IO ()
consoleOutput toutput smap =
  getTraversal . fst $ foldTestOutput foldTest foldHeading toutput smap
  where
    foldTest _name printName getResult printResult =
      ( Traversal $ do
          printName :: IO ()
          r <- getResult
          printResult r
      , Any True)
    foldHeading _name printHeading (printBody, Any nonempty) =
      ( Traversal $ do
          when nonempty $ do printHeading :: IO (); getTraversal printBody
      , Any nonempty
      )

consoleOutputHidingSuccesses :: TestOutput -> StatusMap -> IO ()
consoleOutputHidingSuccesses toutput smap =
  void . getApp $ foldTestOutput foldTest foldHeading toutput smap
  where
    foldTest _name printName getResult printResult =
      Ap $ do
          printName :: IO ()
          r <- getResult
          if resultSuccessful r
            then do clearThisLine; return $ Any False
            else do printResult r :: IO (); return $ Any True

    foldHeading _name printHeading printBody =
      Ap $ do
        printHeading :: IO ()
        Any failed <- getApp printBody
        unless failed clearAboveLine
        return $ Any failed

    clearAboveLine = do cursorUpLine 1; clearThisLine
    clearThisLine = do clearLine; setCursorColumn 0

streamOutputHidingSuccesses :: TestOutput -> StatusMap -> IO ()
streamOutputHidingSuccesses toutput smap =
  void . flip evalStateT [] . getApp $
    foldTestOutput foldTest foldHeading toutput smap
  where
    foldTest _name printName getResult printResult =
      Ap $ do
          r <- liftIO $ getResult
          if resultSuccessful r
            then return $ Any False
            else do
              stack <- get
              put []

              liftIO $ do
                sequence_ $ reverse stack
                printName :: IO ()
                printResult r :: IO ()

              return $ Any True

    foldHeading _name printHeading printBody =
      Ap $ do
        modify (printHeading :)
        Any failed <- getApp printBody
        unless failed $
          modify $ \stack ->
            case stack of
              _:rest -> rest
              [] -> [] -- shouldn't happen anyway
        return $ Any failed

-- }}}

--------------------------------------------------
-- Statistics
--------------------------------------------------
-- {{{

reportStatistics :: (?colors :: Bool) => Statistics -> IO ()
reportStatistics st = case statFailures st of
    0 -> ok $ printf "All %d tests passed" (statTotal st)
    fs -> fail $ printf "%d out of %d tests failed" fs (statTotal st)

-- | @printStatistics@ reports test success/failure statistics and time it took
-- to run. The 'Time' results is intended to be filled in by the 'TestReporter'
-- callback. The @colors@ ImplicitParam controls whether coloured output is
-- used.
--
-- @since 0.11.3
printStatistics :: (?colors :: Bool) => Statistics -> Time -> IO ()
printStatistics st time = do
  printf "\n"
  reportStatistics st
  case statFailures st of
    0 -> ok $ printf " (%.2fs)\n" time
    _ -> fail $ printf " (%.2fs)\n" time

-- }}}

--------------------------------------------------
-- Console test reporter
--------------------------------------------------
-- {{{

consoleTestReporterOptions :: [OptionDescription]
consoleTestReporterOptions =
  [ Option (Proxy :: Proxy HideSuccesses)
  , Option (Proxy :: Proxy UseColor)
  ]

consoleTestReporter :: Ingredient
consoleTestReporter =
  TestReporter consoleTestReporterOptions consoleTestReportImplementation

consoleTestReportImplementation
  :: OptionSet -> TestTree -> Maybe (StatusMap -> IO (Double -> IO Bool))
consoleTestReportImplementation opts tree = Just $ \smap -> do
  let
    whenColor = lookupOption opts
    HideSuccesses hideSuccesses = lookupOption opts
  terminalWidth <- fmap TS.width <$> TS.hSize stdout
  isTerm <- hSupportsANSI stdout
  isTermColor <- hSupportsANSIColor stdout
  bracket_ (when isTerm hideCursor) (when isTerm showCursor) $ do
    let ?colors = useColor whenColor isTermColor
    let toutput = buildTestOutput terminalWidth opts tree
    if
      | hideSuccesses && isTerm && isJust terminalWidth ->
          consoleOutputHidingSuccesses toutput smap
      | hideSuccesses ->
          streamOutputHidingSuccesses toutput smap
      | otherwise -> consoleOutput toutput smap
    return $ \time -> do
      stats <- computeStatistics smap
      printStatistics stats time
      return $ statFailures stats == 0

-- }}}

--------------------------------------------------
-- Formatting
--------------------------------------------------
-- {{{

indentSize :: Int
indentSize = 2

indent :: Int -> String
indent n = replicate (indentSize * n) ' '

truncateName :: Maybe Int -> Int -> String -> String
truncateName Nothing _ s = s
truncateName (Just w) extra s =
  case splitAt (w - extra) s of
    (short,rest)
      | length rest > 0 -> take (w - extra - 5) short ++ "[...]"
      | otherwise -> short

-- handle multi-line result descriptions properly
formatDesc
  :: Int -- indent
  -> String
  -> String
formatDesc n desc =
  let
    -- remove all trailing linebreaks
    chomped = reverse . dropWhile (== '\n') . reverse $ desc

    multiline = '\n' `elem` chomped

    -- we add a leading linebreak to the description, to start it on a new
    -- line and add an indentation
    paddedDesc = flip concatMap chomped $ \c ->
      if c == '\n'
        then c : indent n
        else [c]
  in
    if multiline
      then paddedDesc
      else chomped

data Maximum a
  = Maximum a
  | MinusInfinity

instance Ord a => Semigroup (Maximum a) where
  Maximum a <> Maximum b = Maximum (a `max` b)
  MinusInfinity <> a = a
  a <> MinusInfinity = a
instance Ord a => Monoid (Maximum a) where
  mempty = MinusInfinity
  mappend = (<>)

-- | Compute the amount of space needed to align \"OK\"s and \"FAIL\"s
computeAlignment :: OptionSet -> TestTree -> Int
computeAlignment opts =
  fromMonoid .
  foldTestTree
    trivialFold
      { foldSingle = \_ name _ level -> Maximum (stringWidth name + level)
      , foldGroup = \_opts _ m -> m . (+ indentSize)
      }
    opts
  where
    fromMonoid m =
      case m 0 of
        MinusInfinity -> 0
        Maximum x -> x

-- | Compute the length/width of the string as it would appear in a monospace
--   terminal.
stringWidth :: String -> Int
stringWidth = sum . map charWidth where
  charWidth c = case wcwidth9 (fromIntegral (fromEnum c)) of
        -1 -> 1  -- non-printable, combining or unassigned character
        -2 -> 1  -- ambiguous width character
        -3 -> 1  -- private-use character
        w  -> fromIntegral w
foreign import capi safe "wcwidth9.h wcwidth9" wcwidth9 :: CInt -> CInt

-- (Potentially) colorful output
ok, fail, skipped, infoOk, infoFail :: (?colors :: Bool) => String -> IO ()
fail     = output FormatFail
ok       = output FormatOk
skipped  = output FormatSkipped
infoOk   = output FormatInfoOk
infoFail = output FormatInfoFail

output
  :: (?colors :: Bool)
  => ConsoleFormatType
  -> String
  -> IO ()
output formatType str = do
  format <- getFormat formatType
  withConsoleFormat format (putStr str)

-- }}}
