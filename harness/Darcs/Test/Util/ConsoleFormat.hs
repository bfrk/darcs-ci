module Darcs.Test.Util.ConsoleFormat
  ( ConsoleFormatType(..)
  , stdFormat
  , getFormat
  )
where

import Darcs.Prelude

import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import Test.Tasty.Providers.ConsoleFormat
    ( ConsoleFormat(..)
    , failFormat
    , infoFailFormat
    , infoOkFormat
    , okFormat
    , skippedFormat
    )

-- | Enumeration of supported 'ConsoleFormat's
--
-- @since 1.5.1
data ConsoleFormatType
  = FormatFail
  | FormatInfoFail
  | FormatOk
  | FormatInfoOk
  | FormatSkipped

-- | Default 'ConsoleFormat's
--
-- @since 1.5.1
stdFormat :: ConsoleFormatType -> ConsoleFormat
stdFormat FormatFail = failFormat
stdFormat FormatInfoFail = infoFailFormat
stdFormat FormatOk = okFormat
stdFormat FormatInfoOk = infoOkFormat
stdFormat FormatSkipped = skippedFormat

-- | If the appropriate environment variable has been set,
-- and can be parsed, return the 'ConsoleFormat' that it describes,
-- otherwise use the standard format ('stdFormat').
--
-- An environment variable consists of three words that
-- describe the 'ConsoleIntensity', 'ColorIntensity', and 'Color'
-- of the format. Here is the definition of the standard formats:
--
-- > TASTY_FORMAT_FAIL="BoldIntensity Vivid Red"
-- > TASTY_FORMAT_INFO_FAIL="NormalIntensity Dull Red"
-- > TASTY_FORMAT_OK="NormalIntensity Dull Green"
-- > TASTY_FORMAT_INFO_OK="NormalIntensity Dull White"
-- > TASTY_FORMAT_SKIPPED="NormalIntensity Dull Magenta"
--
-- @since 1.5.1
getFormat :: ConsoleFormatType -> IO ConsoleFormat
getFormat t = do
  mpal <- lookupEnv (formatName t)
  case mpal of
    Nothing -> return (stdFormat t)
    Just pal ->
      case parseFormatVal pal of
        Just fmt -> return fmt
        Nothing -> return (stdFormat t)
  where
    formatName :: ConsoleFormatType -> String
    formatName FormatFail = "TASTY_FORMAT_FAIL"
    formatName FormatInfoFail = "TASTY_FORMAT_INFO_FAIL"
    formatName FormatOk = "TASTY_FORMAT_OK"
    formatName FormatInfoOk = "TASTY_FORMAT_INFO_OK"
    formatName FormatSkipped = "TASTY_FORMAT_SKIPPED"

    parseFormatVal :: String -> Maybe ConsoleFormat
    parseFormatVal str =
      case words str of
        [w1, w2, w3] ->
          ConsoleFormat <$> readMaybe w1 <*> readMaybe w2 <*> readMaybe w3
        _ -> Nothing
