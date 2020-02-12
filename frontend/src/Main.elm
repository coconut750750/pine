module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Events exposing (onClick, onInput)
import Http exposing (stringBody)
import Json.Encode



-- MAIN


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }



-- MODEL


type alias Model =
    { content : String
    , count : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { content = "", count = 1 }, Cmd.none )



-- UPDATE


type Msg
    = Increment
    | Decrement
    | TextUpdate String
    | SendPost
    | GotText (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | count = model.count + 1 }, Cmd.none )

        Decrement ->
            ( { model | count = model.count - 1 }, Cmd.none )

        TextUpdate newcontent ->
            ( { model | content = newcontent }, Cmd.none )

        SendPost ->
            ( model, Http.post { url = "testpost", body = stringBody "text/plain" (Json.Encode.encode 0 (Json.Encode.string "hi there this is HTML body")), expect = Http.expectString GotText } )

        GotText result ->
            case result of
                Ok fullText ->
                    ( { model | content = fullText }, Cmd.none )

                Err _ ->
                    ( { model | content = "FAILUERE" }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout
        [ width fill, height fill ]
        (column [ centerX, centerY ]
            [ myInput model.content
            , myCounter model.count
            , postButton
            ]
        )


postButton : Element Msg
postButton =
    Input.button [ padding 30 ]
        { onPress = Just SendPost
        , label = Element.text "SEND POST"
        }


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
myInput content =
    Input.text []
        { onChange = TextUpdate
        , text = content
        , placeholder = Just (Input.placeholder [] (Element.text "enter some text"))
        , label = Input.labelAbove [] (Element.text "")
        }
