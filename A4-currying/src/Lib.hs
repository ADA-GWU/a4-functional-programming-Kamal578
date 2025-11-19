module Lib
    ( restClient
    ) where

import Network.HTTP.Conduit
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8

-- | Curried REST request builder that defers IO until the final call.
restClient :: String -> String -> String -> IO L8.ByteString
restClient httpMethod url endpoint = do
    let fullUrl = url ++ endpoint
    manager <- newManager tlsManagerSettings
    req <- parseRequest fullUrl
    let request = req { method = B8.pack httpMethod }
    response <- httpLbs request manager
    pure (responseBody response)
