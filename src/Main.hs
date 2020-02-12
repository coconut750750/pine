{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Main where

import Web.Scotty
import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Control.Monad.IO.Class



data User = User { userId :: Int, userName :: String } deriving (Show, Generic)

instance ToJSON User
instance FromJSON User


bob :: User
bob = User { userId = 1, userName = "bob" }

jenny :: User
jenny = User { userId = 2, userName = "jenny" }

allUsers :: [User]
allUsers = [bob, jenny]

{-|
 - Things that are easy to understand and are a 5 minute google away:
 -      What do the $ symbols mean?
 -      What are the <> symbols?
 -      How does the ":name" work? (look up Route Parameters for Scotty)
 -
 - Things that are maybe 15 minute google adventures:
 -      What are the 'do' statements? 
 -
 - Things that we may never know why but work:
 -      liftIO
-}

main = do
    putStrLn "Starting Server..."
    scotty 3000 $ do
        get "/testpost/:name" $ do
            name <- param "name"
            text ("hello " <> name <> "!")
        post "/testpost" $ do
            bodyBytes <- body
            liftIO $ print $ decodeUtf8 bodyBytes
            text "hey there this is backend reply"
        get "/" $
            file "./frontend/index.html"


