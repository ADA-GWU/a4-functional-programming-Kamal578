-- app/Main.hs

module Main (main) where
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as B8

-- Curried function to build the request
restClient :: String -> String -> String -> IO L8.ByteString
restClient httpMethod url endpoint = do
    let fullUrl = url ++ endpoint
    manager <- newManager tlsManagerSettings
    req <- parseRequest fullUrl
    let request = req { method = B8.pack httpMethod } 
    response <- httpLbs request manager
    return (responseBody response)

main :: IO ()
main = do
    response <- restClient "GET" "https://jsonplaceholder.typicode.com" "/posts/1"
    L8.putStrLn response
