{-# LANGUAGE OverloadedStrings #-}

-- app/Main.hs

module Main (main) where

import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as L8

import Lib (restClient, restClientWithBody)

jsonPlaceholder :: String
jsonPlaceholder = "https://jsonplaceholder.typicode.com"

main :: IO ()
main = do
    putStrLn "GET /posts/1 response:"
    response <- restClient "GET" jsonPlaceholder "/posts/1"
    L8.putStrLn response

    putStrLn "\nPOST /posts response:"
    postResponse <- restClientWithBody "POST" jsonPlaceholder "/posts" (Just postPayload)
    L8.putStrLn postResponse

postPayload :: L8.ByteString
postPayload =
    encode $
        object
            [ "title" .= ("ASP Assignment" :: String)
            , "body" .= ("Functional Programming (Currying)" :: String)
            , "userId" .= (1 :: Int)
            ]
