module GeneratedTypes.Decoder exposing (..)

import Iso8601 as Iso
import Json.Decode as D exposing (..)
import Json.Decode.Pipeline as D exposing (required)

import GeneratedTypes.ElmStreet exposing (..)
import GeneratedTypes.Types as T


decodeCodeSubmission : Decoder T.CodeSubmission
decodeCodeSubmission = D.succeed T.CodeSubmission
    |> required "text" (D.list elmStreetDecodeChar)
