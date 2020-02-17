module Main exposing (..)

import Browser
import Css exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Generated.Decoder exposing (decodeCodeSubmission)
import Generated.Encoder exposing (encodeCodeSubmission)
import Generated.Types exposing (CodeSubmission)
import Html exposing (Html, div, textarea, input, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onInput, preventDefaultOn)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events
import Html.Styled
import Http exposing (stringBody)
import Json.Encode
import Json.Decode as Decode
import Debug exposing (log)

-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view >> Html.Styled.toUnstyled
        }



-- MODEL


type alias Model =
    { mainCode : String
    , count1 : Int
    , count2 : Int
    , backendReply : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { mainCode = ""
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
    | TabDown


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
            ( { model | mainCode = newcontent }, Cmd.none )

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
                                model.mainCode
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

        TabDown ->
            ( { model | mainCode = model.mainCode ++ "  " }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- VIEW

theme : { primary : Css.Color, secondary : Css.Color, text: Css.Color }
theme =
    { primary = Css.hex "20252d"
    , secondary = Css.rgb 250 240 230
    , text = Css.hex "ffffff"
    }

view : Model -> Html.Styled.Html Msg
view model =
    Html.Styled.div [ css [ Css.height (Css.pct 100) ]]
    [ (mainInput model.mainCode)
    --, Html.Styled.div [ style "width" "50%", style "float" "left" ] 
    --  [ Element.layout [ Element.width Element.fill, Element.height Element.fill ]
    --      (Element.row [ Element.width Element.fill, Element.height Element.fill ]
    --          [ (Element.column [ Element.height Element.fill ]
    --              [ mainOutput [] "asdf"
    --              , intInputs model.count1 model.count2
    --              , postButton
    --              , backendOutput model.backendReply
    --              ]
    --          )]
    --      )
    --  ]
    ]

mainInput : String -> Html.Styled.Html Msg
mainInput code = 
  Html.Styled.textarea 
    [ Html.Styled.Events.onInput TextUpdate
    , onTab TabDown
    , Html.Styled.Attributes.value code 
    , css 
      [ fontFamily monospace
      , fontSize (Css.px 14)
      , margin (Css.px 0)
      , border (Css.px 0)
      , Css.padding (Css.px 0)
      , Css.minHeight (Css.vh 100)
      , Css.width (Css.pct 50)
      , Css.resize Css.none
      , Css.color theme.text
      , Css.backgroundColor theme.primary
      , Css.float left
      , Css.focus
        [ outline zero
        ]
      ]
  ] []


onTab : msg -> Html.Styled.Attribute msg
onTab msg =
  let _ = Debug.log "onTab" Html.Events.keyCode
  in
  let
    isTabKey keyCode =
      if keyCode == 9 then
        Decode.succeed msg
      else
        Decode.fail "silent failure :)"
  in
  Html.Events.keyCode
  |> Decode.andThen isTabKey
  |> Decode.map (\x -> { message = x, stopPropagation = True, preventDefault = True })
  |> Html.Styled.Events.custom "keydown"

mainOutput : List (Attribute Msg) -> String -> Element Msg
mainOutput attrs output = 
    Element.el attrs (Element.text output)

intInputs : Int -> Int -> Element Msg
intInputs count1 count2 =
    Element.row
        [ Border.rounded 3
        , Element.padding 30
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
    Input.button [ Element.padding 30, Border.rounded 3, Border.width 1, centerX ]
        { onPress = Just SendPost
        , label = Element.text "SEND POST"
        }


backendOutput : String -> Element Msg
backendOutput reply =
    el [ Element.padding 20, Element.width Element.fill ]
        (paragraph [ Element.padding 20, Border.rounded 3, Border.solid, Border.width 1, Element.width Element.fill ] [ Element.text reply ])
