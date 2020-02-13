{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass, DataKinds, TypeApplications #-}

{- | Generates Elm types from Haskell @types@ internal library.
The generated files can be found in the @elm-example/src@ folder.
-}

module GenerateElm where

import Data.Aeson (FromJSON, ToJSON, toJSON, parseJSON)
import Data.Text (Text)
import Elm (Elm, defaultSettings, generateElm, elmStreetToJson, elmStreetParseJson)
import GHC.Generics


type Types =
   '[ CodeSubmission ]
    
generate :: IO ()
generate = generateElm @Types $ defaultSettings "frontend/src" ["Generated"]
