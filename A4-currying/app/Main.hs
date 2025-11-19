-- app/Main.hs

module Main (main) where

import qualified Data.ByteString.Lazy.Char8 as L8

import Lib (restClient)

main :: IO ()
main = do
    response <- restClient "GET" "https://jsonplaceholder.typicode.com" "/posts/1"
    L8.putStrLn response
