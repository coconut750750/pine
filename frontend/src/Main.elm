module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import Element exposing (Element, el, row, alignRight, fill, width, rgb255, spacing, centerY, padding)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input

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
    [ Element.layout [] (myInput model.content)
    , Element.layout [] (myCounter model.count)
    ]

myCounter : Int -> Element Msg
myCounter count = 
  row []
    [ Input.button [ padding 30 ]
        { onPress = Just Decrement
        , label = Element.text "-"
        }
    , el
        [ Border.rounded 3
        , padding 30
        ]
        (Element.text (String.fromInt count))
    , Input.button [ padding 30 ]
        { onPress = Just Increment
        , label = Element.text "+"
        }
    ]

myInput : String -> Element Msg
myInput content = Input.text []
  { onChange = TextUpdate 
  , text = content
  , placeholder = Just (Input.placeholder [] (Element.text "enter some text"))
  , label = Input.labelAbove [] (Element.text "")
  }