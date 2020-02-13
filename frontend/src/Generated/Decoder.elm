module Generated.Decoder exposing (..)

import Iso8601 as Iso
import Json.Decode as D exposing (..)
import Json.Decode.Pipeline as D exposing (required)

import Generated.ElmStreet exposing (..)
import Generated.Types as T


decodeCodeSubmission : Decoder T.CodeSubmission
decodeCodeSubmission = D.succeed T.CodeSubmission
    |> required "count1" D.int
    |> required "count2" D.int
    |> required "text" D.string
