module Aws.Lambda.Runtime.ApiInfo
  ( Event(..)
  , fetchEvent
  ) where

import qualified Text.Read as Read
import qualified Control.Monad as Monad

import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Types.Header as Http
import qualified Data.ByteString.Lazy.Char8 as Lazy
import qualified Data.ByteString.Char8 as ByteString
import Data.ByteString.Char8 (ByteString)
import Control.Exception.Safe.Checked
import Control.Exception (IOException)

import qualified Aws.Lambda.Runtime.API.Endpoints as Endpoints
import qualified Aws.Lambda.Runtime.Error as Error

data Event = Event
  { deadlineMs :: !Int
  , traceId :: !String
  , awsRequestId :: !String
  , invokedFunctionArn :: !String
  , event :: !Lazy.ByteString
  }

fetchEvent :: Throws Error.Parsing => Http.Manager -> String -> IO Event
fetchEvent manager lambdaApi = do
  response <- fetchApiData manager lambdaApi
  let body = Http.responseBody response
  let headers = Http.responseHeaders response
  Monad.foldM reduceEvent (initialEvent body) headers

fetchApiData :: Http.Manager -> String -> IO (Http.Response Lazy.ByteString)
fetchApiData manager lambdaApi = do
  let Endpoints.Endpoint endpoint = Endpoints.nextInvocation lambdaApi
  request <- Http.parseRequest endpoint
  keepRetrying $ Http.httpLbs request manager

reduceEvent :: Throws Error.Parsing => Event -> (Http.HeaderName, ByteString) -> IO Event
reduceEvent event header =
  case header of
    ("Lambda-Runtime-Deadline-Ms", value) ->
      case Read.readMaybe $ ByteString.unpack value of
        Just ms -> pure event { deadlineMs = ms }
        Nothing -> throw (Error.Parsing "deadlineMs" $ ByteString.unpack value)

    ("Lambda-Runtime-Trace-Id", value) ->
      pure event { traceId = ByteString.unpack value }

    ("Lambda-Runtime-Aws-Request-Id", value) ->
      pure event { awsRequestId = ByteString.unpack value }

    ("Lambda-Runtime-Invoked-Function-Arn", value) ->
      pure event { invokedFunctionArn = ByteString.unpack value }

    _ ->
      pure event

initialEvent :: Lazy.ByteString -> Event
initialEvent body = Event
  { deadlineMs = 0
  , traceId = ""
  , awsRequestId = ""
  , invokedFunctionArn = ""
  , event = body
  }

keepRetrying :: IO (Http.Response Lazy.ByteString) -> IO (Http.Response Lazy.ByteString)
keepRetrying action = do
  result <- try action :: IO (Either IOException (Http.Response Lazy.ByteString))
  case result of
    Right success -> pure success
    _ -> keepRetrying action