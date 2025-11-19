module Lib
    ( restClient
    , restClientWithBody
    ) where

import Network.HTTP.Conduit
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Client ()
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8

-- | Curried REST request builder that defers IO until the final call.
restClient :: String -> String -> String -> IO L8.ByteString
restClient httpMethod url endpoint =
    restClientWithBody httpMethod url endpoint Nothing

-- | Variant that accepts an optional JSON request body (used for POST/PUT).
restClientWithBody :: String -> String -> String -> Maybe L8.ByteString -> IO L8.ByteString
restClientWithBody httpMethod url endpoint maybeBody = do
    let fullUrl = url ++ endpoint
    manager <- newManager tlsManagerSettings
    req <- parseRequest fullUrl
    let baseRequest = req { method = B8.pack httpMethod }
        request = maybe baseRequest (attachJsonBody baseRequest) maybeBody
    response <- httpLbs request manager
    pure (responseBody response)

attachJsonBody :: Request -> L8.ByteString -> Request
attachJsonBody req body =
    req
        { requestBody = RequestBodyLBS body
        , requestHeaders = (hContentType, B8.pack "application/json; charset=utf-8") : filteredHeaders
        }
  where
    filteredHeaders = filter ((/= hContentType) . fst) (requestHeaders req)
