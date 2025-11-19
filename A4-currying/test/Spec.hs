{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (FromJSON (parseJSON), eitherDecode, encode, object, withObject, (.:), (.=))
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import Lib (restClient, restClientWithBody)
import Test.Hspec

data Post = Post
    { postUserId :: Int
    , postId :: Int
    , postTitle :: Text
    , postBody :: Text
    }
    deriving (Eq, Show)

instance FromJSON Post where
    parseJSON = withObject "Post" $ \v ->
        Post <$> v .: "userId"
             <*> v .: "id"
             <*> v .: "title"
             <*> v .: "body"

main :: IO ()
main = hspec $
    describe "restClient JSONPlaceholder integration" $ do
        it "fetches the first post" $ do
            post <- fetchPost 1
            post `shouldBe` post1

        it "fetches the second post" $ do
            post <- fetchPost 2
            post `shouldBe` post2

        it "creates a post via POST request" $ do
            post <- createPost
            post `shouldBe` createdPost

fetchPost :: Int -> IO Post
fetchPost n = do
    response <- restClient "GET" baseUrl ("/posts/" ++ show n)
    case decodePost response of
        Left err -> expectationFailure ("Failed to decode JSON: " ++ err) >> fail err
        Right post -> pure post

createPost :: IO Post
createPost = do
    let payload =
            encode $
                object
                    [ "title" .= ("ASP Assignment" :: String)
                    , "body" .= ("Functional Programming (Currying)" :: String)
                    , "userId" .= (1 :: Int)
                    ]
    response <- restClientWithBody "POST" baseUrl "/posts" (Just payload)
    case decodePost response of
        Left err -> expectationFailure ("Failed to decode JSON: " ++ err) >> fail err
        Right post -> pure post

decodePost :: LBS.ByteString -> Either String Post
decodePost = eitherDecode

baseUrl :: String
baseUrl = "https://jsonplaceholder.typicode.com"

post1 :: Post
post1 =
    Post
        { postUserId = 1
        , postId = 1
        , postTitle = "sunt aut facere repellat provident occaecati excepturi optio reprehenderit"
        , postBody =
            "quia et suscipit\n\
            \suscipit recusandae consequuntur expedita et cum\n\
            \reprehenderit molestiae ut ut quas totam\n\
            \nostrum rerum est autem sunt rem eveniet architecto"
        }

post2 :: Post
post2 =
    Post
        { postUserId = 1
        , postId = 2
        , postTitle = "qui est esse"
        , postBody =
            "est rerum tempore vitae\n\
            \sequi sint nihil reprehenderit dolor beatae ea dolores neque\n\
            \fugiat blanditiis voluptate porro vel nihil molestiae ut reiciendis\n\
            \qui aperiam non debitis possimus qui neque nisi nulla"
        }

createdPost :: Post
createdPost =
    Post
        { postUserId = 1
        , postId = 101
        , postTitle = "ASP Assignment"
        , postBody = "Functional Programming (Currying)"
        }
