{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass, DataKinds, TypeApplications #-}

{- | Generates Elm types from Haskell @types@ internal library.
-}

module GenerateElm where

import Data.Aeson (FromJSON, ToJSON, toJSON, parseJSON)
import Data.Text (Text)
import Elm (Elm, defaultSettings, generateElm, elmStreetToJson, elmStreetParseJson)
import GHC.Generics
import Types (CodeSubmission)


type Types =
   '[ CodeSubmission ]
    
generate :: IO ()
generate = generateElm @Types $ defaultSettings "frontend/src" ["GeneratedTypes"]
