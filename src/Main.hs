{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Main where

import Web.Scotty
import GHC.Generics
import Data.Aeson (FromJSON, ToJSON, decode)
import Data.Text.Lazy (pack, Text)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Control.Monad.IO.Class
import Language.Haskell.Interpreter hiding (get)
import System.Environment (lookupEnv)

import Data.Text.Internal (showText)

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
evaluateStr :: String -> IO Text
evaluateStr haskellStr = do
    r <- runInterpreter $ setImports ["Prelude"] >> eval haskellStr
    case r of
        Left err -> return $ pack (show err)
        Right str -> return $ pack (str)
        
extractPort :: Maybe String -> Int
extractPort portMaybe = case portMaybe of
    Just value -> read value
    Nothing -> 3000

main = do
    putStrLn "Starting Server..."
    port <- extractPort <$> (lookupEnv "PORT")
    scotty port $ do
        get "/testpost/:name" $ do
            name <- param "name"
            test <- liftIO $ evaluateStr $ name
            text ("" <> test <> "")
        post "/testpost" $ do
            bodyBytes <- body
            -- decoded <- decodeUtf8 $ liftIO $ bodyBytes
            test <- liftIO $ evaluateStr $ getCode $ decode bodyBytes
            -- test <- liftIO $ evaluateStr $ unpack $ decodeUtf8 bodyBytes
            -- liftAndCatchIO $ evaluateStr $ show $ decodeUtf8 bodyBytes
            text (test)
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

getCode :: Maybe CodeSubmission -> String
getCode (Just (CodeSubmission _ _ code)) = code
