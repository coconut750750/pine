{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Main where

import Web.Scotty
import GHC.Generics
import Data.Aeson (FromJSON, ToJSON, decode)
import Data.Text.Lazy (pack)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Control.Monad.IO.Class

import Types (CodeSubmission(..), parseJSON)


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
            text (pack ( show(addCodeSubmission (decode bodyBytes))))
        get "/" $
            file "./frontend/index.html"


-- parseJsonAndAdd :: ByteString -> Int
-- parseJsonAndAdd jsonString = 
--     addCodeSubmission (decode jsonString)

addCodeSubmission :: Maybe CodeSubmission -> Int
addCodeSubmission (Just (CodeSubmission num1 num2 _)) =
    num1 + num2
addCodeSubmission Nothing =
    0
