module Darcs.Util.Exception
    ( firstJustIO
    , catchall
    , clarifyErrors
    , prettyException
    , prettyError
    , die
    , handleOnly
    , handleOnlyIOError
    , catchDoesNotExistError
    , handleDoesNotExistError
    , ifIOError
    , ifDoesNotExistError
    ) where


import Darcs.Prelude

import Control.Exception
    ( Exception(fromException)
    , SomeException
    , catch
    , handle
    , throwIO
    )
import Data.Maybe ( isJust )

import System.Exit ( exitFailure )
import System.IO ( stderr, hPutStrLn )
import System.IO.Error
    ( catchIOError
    , ioeGetErrorString
    , ioeGetFileName
    , isDoesNotExistError
    , isUserError
    )

import Darcs.Util.Global ( debugMessage )

-- | This handles /all/ 'IOException's
catchall :: IO a -> IO a -> IO a
a `catchall` b = a `catchIOError` (\e -> debugMessage ("catchall: "++show e) >> b)

-- | Run the elements of a list of monadic actions until a 'Just' result is
-- obtained. Return that result or 'Nothing' if all actions do.
firstJustM :: Monad m => [m (Maybe a)] -> m (Maybe a)
firstJustM [] = return Nothing
firstJustM (e:es) = e >>= (\v -> if isJust v then return v else firstJustM es)

-- | A slight modification to 'firstJustM': the entries in the list must be IO
-- actions and any 'IOException's are converted to a 'Nothing' result.
firstJustIO :: [IO (Maybe a)] -> IO (Maybe a)
firstJustIO = firstJustM . map (`catchall` return Nothing)

-- | Convert any exception thrown by the action into an 'ExitFailure' and
-- combine the pretty-printed exception with the given error message.
clarifyErrors :: IO a -> String -> IO a
clarifyErrors a e = a `catch` (\x -> die $ unlines [prettyException x,e])

-- | Like 'show', but with rewordings for some selected exceptions.
prettyException :: SomeException -> String
prettyException e
  | Just ioe <- fromException e, isUserError ioe = ioeGetErrorString ioe
  | Just ioe <- fromException e, isDoesNotExistError ioe =
    case ioeGetFileName ioe of
      Just f  -> f ++ " does not exist"
      Nothing -> show e
  | otherwise = show e

-- | Like 'show', except for 'UserError's where we return only the error string
-- itself.
prettyError :: IOError -> String
prettyError e
  | isUserError e = ioeGetErrorString e
  | otherwise = show e

-- | Terminate the program with an error message.
die :: String -> IO a
die msg = hPutStrLn stderr msg >> exitFailure

-- | Handle only actual IO exceptions i.e. not "user errors" e.g. those raised
-- by calling 'fail'.
--
-- We use 'fail' all over the place to signify erroneous conditions and we
-- normally don't want to handle such errors.
handleOnlyIOError :: IO a -> IO a -> IO a
handleOnlyIOError = handleOnly (not . isUserError)

-- | Handle only non-existence.
handleDoesNotExistError :: IO a -> IO a -> IO a
handleDoesNotExistError = handleOnly isDoesNotExistError

-- | Handle only non-existence.
catchDoesNotExistError :: IO a -> IO a -> IO a
catchDoesNotExistError = flip handleDoesNotExistError

-- | Like 'handleOnlyIOError' but restricted to returning a given value.
ifIOError :: a -> IO a -> IO a
ifIOError use_instead = handleOnlyIOError (return use_instead)

-- | Like 'ifIOError' but restricted to handling non-existence.
ifDoesNotExistError :: a -> IO a -> IO a
ifDoesNotExistError use_instead = handleOnly isDoesNotExistError (return use_instead)

-- | Handle only a those exceptions for which the predicate succeeds.
handleOnly :: Exception e => (e -> Bool) -> IO a -> IO a -> IO a
handleOnly pred handler = handle (\e -> if pred e then handler else throwIO e)
