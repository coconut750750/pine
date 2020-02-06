module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model = {
    content : String,
    count: Int
  }


init : Model
init = {
    content = "",
    count = 1
  }



-- UPDATE


type Msg
  = Increment
  | Decrement
  | TextUpdate String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      { model | count = model.count + 1 }

    Decrement ->
      { model | count = model.count - 1 }

    TextUpdate newcontent ->
      { model | content = newcontent }



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ input [ placeholder "Enter some text", value model.content, onInput TextUpdate ] []
    , div [] [ text (model.content)]
    , button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (String.fromInt model.count) ]
    , button [ onClick Increment ] [ text "+" ]
    ]