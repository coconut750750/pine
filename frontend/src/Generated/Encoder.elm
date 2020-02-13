module Generated.Encoder exposing (..)

import Iso8601 as Iso
import Json.Encode as E exposing (..)

import Generated.ElmStreet exposing (..)
import Generated.Types as T


encodeCodeSubmission : T.CodeSubmission -> Value
encodeCodeSubmission x = E.object
    [ ("tag", E.string "CodeSubmission")
    , ("count1", E.int x.count1)
    , ("count2", E.int x.count2)
    , ("text", E.string x.text)
    ]
