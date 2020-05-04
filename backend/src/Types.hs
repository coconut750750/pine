{-# LANGUAGE OverloadedStrings, DerivingStrategies, DeriveGeneric, DeriveAnyClass, DataKinds, TypeApplications #-}

module Types(CodeSubmission(..), parseJSON) where 

import Data.Aeson (FromJSON, ToJSON, toJSON, parseJSON)
import Data.Text (Text)
import Elm (Elm, defaultSettings, generateElm, elmStreetToJson, elmStreetParseJson)
import GHC.Generics

data CodeSubmission = CodeSubmission
    { codeText      :: Text
    } deriving (Generic)
      deriving anyclass (Elm)

instance ToJSON   CodeSubmission where 
    toJSON = elmStreetToJson

instance FromJSON CodeSubmission where 
    parseJSON = elmStreetParseJson
