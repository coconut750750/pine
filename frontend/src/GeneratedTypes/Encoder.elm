module GeneratedTypes.Encoder exposing (..)

import Iso8601 as Iso
import Json.Encode as E exposing (..)

import GeneratedTypes.ElmStreet exposing (..)
import GeneratedTypes.Types as T


encodeCodeSubmission : T.CodeSubmission -> Value
encodeCodeSubmission x = E.object
    [ ("tag", E.string "CodeSubmission")
    , ("text", (E.list (E.string << String.fromChar)) x.text)
    ]
