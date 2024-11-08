{-# LANGUAGE CPP #-}
module Darcs.Util.HTTP
    ( Cachable(..)
    , copyRemote
    , copyRemoteLazy
    , speculateRemote
    , postUrl
    , configureHttpConnectionManager
    ) where

import Control.Concurrent.Async ( async, cancel, poll )
import Control.Exception ( catch )
import Control.Monad ( void , (>=>) )
import Crypto.Random ( seedNew, seedToInteger )
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC

import Data.Conduit.Combinators ( sinkLazy )

import Foreign.C.Types ( CInt )

import Network.HTTP.Simple
    ( HttpException(..)
    , Request
    , httpBS
    , httpSink
    , httpNoBody
    , getResponseBody
    , setRequestHeaders
    , setRequestMethod
    , setRequestResponseTimeout
    )
import Network.HTTP.Conduit
    ( ResponseTimeout
    , parseUrlThrow
    , responseTimeoutDefault
    , responseTimeoutMicro
    )
import Network.HTTP.Types.Header
    ( hCacheControl
    , hPragma
    , hContentType
    , hAccept
    , hContentLength
    )

#ifdef HAVE_CRYPTON_CONNECTION
import Data.Default ( def )
import qualified Network.Connection as NC
import Network.HTTP.Client.TLS
    ( mkManagerSettings
    , newTlsManagerWith
    , setGlobalManager
    )
import qualified Network.TLS as TLS
#endif

import Numeric ( showHex )
import System.Directory ( renameFile )
import System.Environment ( lookupEnv )
import Text.Read ( readMaybe )

import Darcs.Prelude

import Darcs.Util.AtExit ( atexit )
import Darcs.Util.Global ( debugMessage )

data Cachable
  = Cachable
  | Uncachable
  | MaxAge !CInt
  deriving (Show, Eq)

darcsResponseTimeout :: IO ResponseTimeout
darcsResponseTimeout =
  lookupEnv "DARCS_CONNECTION_TIMEOUT" >>= \case
    Just s | Just n <- readMaybe s ->
      return $ responseTimeoutMicro $ 1000000 * n
    _ -> return responseTimeoutDefault -- 30 s, seems a bit long

copyRemote :: String -> FilePath -> Cachable -> IO ()
copyRemote url path cachable = do
  debugMessage $ "copyRemote: " ++ url
  junk <- flip showHex "" <$> seedToInteger <$> seedNew
  let tmppath = path ++ ".new_" ++ junk
  tmo <- darcsResponseTimeout
  handleHttpAndUrlExn url
    (httpBS . setRequestResponseTimeout tmo . addCacheControl cachable >=>
     B.writeFile tmppath . getResponseBody)
  renameFile tmppath path

-- TODO instead of producing a lazy ByteString we should re-write the
-- consumer (Darcs.Repository.Packs) to use proper streaming (e.g. conduit)
copyRemoteLazy :: String -> Cachable -> IO (BL.ByteString)
copyRemoteLazy url cachable = do
  debugMessage $ "copyRemoteLazy: " ++ url
  handleHttpAndUrlExn url
    (flip httpSink (const sinkLazy) . addCacheControl cachable)

speculateRemote :: String -> FilePath -> IO ()
speculateRemote url path = do
  r <- async $ do
    debugMessage $ "Start speculating on " ++ url
    -- speculations are always Cachable
    copyRemote url path Cachable
    debugMessage $ "Completed speculating on " ++ url
  atexit $ do
    result <- poll r
    case result of
      Just (Right ()) ->
        debugMessage $ "Already completed speculating on " ++ url
      Just (Left e) ->
        debugMessage $ "Speculating on " ++ url ++ " failed: " ++ show e
      Nothing -> do
        debugMessage $ "Abort speculating on " ++ url
        cancel r

postUrl
  :: String -- ^ url
  -> BC.ByteString -- ^ body
  -> String -- ^ mime type
  -> IO () -- ^ result
postUrl url body mime =
    handleHttpAndUrlExn url (void . httpNoBody . setMethodAndHeaders)
  where
    setMethodAndHeaders =
      setRequestMethod (BC.pack "POST") .
      setRequestHeaders
        [ (hContentType, BC.pack mime)
        , (hAccept, BC.pack "text/plain")
        , (hContentLength, BC.pack $ show $ B.length body)
        ]

addCacheControl :: Cachable -> Request -> Request
addCacheControl Uncachable =
  setRequestHeaders [(hCacheControl, noCache), (hPragma, noCache)]
addCacheControl (MaxAge seconds) | seconds > 0 =
  setRequestHeaders [(hCacheControl, BC.pack $ "max-age=" ++ show seconds)]
addCacheControl _ = id

noCache :: BC.ByteString
noCache = BC.pack "no-cache"

handleHttpAndUrlExn :: String -> (Request -> IO a) -> IO a
handleHttpAndUrlExn url action =
  catch (parseUrlThrow url >>= action) (\case
    InvalidUrlException _ reason ->
      fail $ "Invalid URI: " ++ url ++ ", reason: " ++ reason
    HttpExceptionRequest _ hec {- :: HttpExceptionContent -}
     -> fail $ "Error getting " ++ show url ++ ": " ++ show hec)

-- | To be called from main program in order to set up a connection manager
-- with changed TLS settings. Particularly, since tls-2.0 the default value for
-- 'TLS.supportedExtendedMainSecret' was changed from 'TLS.AllowEMS' to
-- 'TLS.RequireEMS', which is currently (2024-05-19) not yet supported by
-- hub.darcs.net.
configureHttpConnectionManager :: IO ()
#ifdef HAVE_CRYPTON_CONNECTION
configureHttpConnectionManager = do
  let tlsSettings =
        NC.TLSSettingsSimple False False False
          def { TLS.supportedExtendedMainSecret = TLS.AllowEMS }
  manager <- newTlsManagerWith $ mkManagerSettings tlsSettings Nothing
  setGlobalManager manager
#else
configureHttpConnectionManager = return ()
#endif
