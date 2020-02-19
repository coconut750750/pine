module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event exposing (on)
import Element.Font as Font
import Element.Input as Input
import Generated.Decoder exposing (decodeCodeSubmission)
import Generated.Encoder exposing (encodeCodeSubmission)
import Generated.Types exposing (CodeSubmission)
import Html exposing (Html)
import Html.Events exposing (onClick, onInput)
import Http exposing (stringBody)
import Json.Encode



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { someCode : String
    , count1 : Int
    , count2 : Int
    , backendReply : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { someCode = ""
      , count1 = 0
      , count2 = 0
      , backendReply = ""
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = UpdateCount1 String
    | UpdateCount2 String
    | TextUpdate String
    | SendPost
    | GotReply (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateCount1 newCount ->
            if newCount == "" then
                ( { model | count1 = 0 }, Cmd.none )

            else
                case String.toInt newCount of
                    Just integer ->
                        ( { model | count1 = integer }, Cmd.none )

                    Nothing ->
                        ( model, Cmd.none )

        UpdateCount2 newCount ->
            if newCount == "" then
                ( { model | count2 = 0 }, Cmd.none )

            else
                case String.toInt newCount of
                    Just integer ->
                        ( { model | count2 = integer }, Cmd.none )

                    Nothing ->
                        ( model, Cmd.none )

        TextUpdate newcontent ->
            ( { model | someCode = newcontent }, Cmd.none )

        SendPost ->
            ( model
            , Http.post
                { url = "testpost"
                , body =
                    Http.jsonBody
                        (encodeCodeSubmission
                            (CodeSubmission
                                model.count1
                                model.count2
                                model.someCode
                            )
                        )
                , expect = Http.expectString GotReply
                }
            )

        GotReply result ->
            case result of
                Ok fullText ->
                    ( { model | backendReply = fullText }, Cmd.none )

                Err _ ->
                    ( { model | backendReply = "FAILURE" }, Cmd.none )



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
            [ myInput model.someCode
            , intInputs model.count1 model.count2
            , postButton
            , backendOutput model.backendReply
            ]
        )


myInput : String -> Element Msg
myInput content =
    Input.text []
        { onChange = TextUpdate
        , text = content
        , placeholder = Just (Input.placeholder [] (Element.text "enter some text"))
        , label = Input.labelAbove [] (Element.text "")
        }


intInputs : Int -> Int -> Element Msg
intInputs count1 count2 =
    row
        [ Border.rounded 3
        , padding 30
        ]
        [ Input.text []
            { label = Input.labelAbove [] (Element.text "")
            , onChange = UpdateCount1
            , placeholder = Just (Input.placeholder [] (Element.text "enter some text"))
            , text = String.fromInt count1
            }
        , Input.text []
            { onChange = UpdateCount2
            , text = String.fromInt count2
            , placeholder = Just (Input.placeholder [] (Element.text "enter some text"))
            , label = Input.labelAbove [] (Element.text "")
            }
        ]


postButton : Element Msg
postButton =
    Input.button [ padding 30, Border.rounded 3, Border.width 1, centerX ]
        { onPress = Just SendPost
        , label = Element.text "SEND POST"
        }


backendOutput : String -> Element Msg
backendOutput reply =
    el [ padding 20, width fill ]
        (paragraph [ padding 20, Border.rounded 3, Border.solid, Border.width 1, width fill ] [ text reply ])
