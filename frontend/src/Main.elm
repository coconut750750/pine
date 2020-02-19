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
import Utils.CssUtils as CssUtils

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
    , output : String
    , count1 : Int
    , count2 : Int
    , backendReply : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { mainCode = ""
      , output = "output"
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
                                1
                                2
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

theme : { primary : Css.Color, secondary : Css.Color, border: Css.Color, text: Css.Color }
theme =
    { primary = Css.hex "20252d"
    , secondary = Css.hex "1c2027"
    , border = Css.rgb 100 100 120
    , text = Css.hex "ffffff"
    }

view : Model -> Html.Styled.Html Msg
view model =
    Html.Styled.div [ css [ Css.height (Css.pct 100) ]]
    [ Html.Styled.div [ 
        css [ Css.minHeight (Css.vh 10)
              , Css.width (Css.vw 100)
              , Css.float left
            ]
        ] 
        [ ( mainHeader [] (Css.vh 10) )
        ]
    , Html.Styled.div [ 
        css [ Css.minHeight (Css.vh 90)
              , Css.width (Css.vw 50)
              , Css.float left
            ]
        ] 
        [ ( mainInput [ Css.minHeight (Css.vh 90) ] model.mainCode )
        ]
    , Html.Styled.div [ 
        css [ Css.minHeight (Css.vh 90)
              , Css.width (Css.vw 50)
              , Css.float left
            ]
        ] 
        [ ( mainOutput [ Css.minHeight (Css.vh 90) ] model.backendReply )
        ]
    ]

mainHeader : List (Css.Style) -> Css.Calc val -> Html.Styled.Html Msg
mainHeader attrs height =
  let
    borderSize = (Css.px 0.25)
    heightSize = CssUtils.calcDimension height (Css.px 0) borderSize
  in
  Html.Styled.div [
    css ([ Css.backgroundColor theme.secondary 
         , Css.minHeight heightSize
         , border (Css.px 0)
         , borderBottom3 borderSize Css.solid theme.border
         ] ++ attrs)
    ]
    [
      Html.Styled.button 
        [ Html.Styled.Events.onClick SendPost
        ]
        [ 
        ]
    ]

mainInput : List (Css.Style) ->  String -> Html.Styled.Html Msg
mainInput attrs code = 
  let
    paddingSize = (Css.px 2)
    borderSize = (Css.px 0.125)
    widthSize = CssUtils.calcDimension (Css.pct 100) paddingSize borderSize
  in
  Html.Styled.textarea 
    [ Html.Styled.Events.onInput TextUpdate
    , onTab TabDown
    , Html.Styled.Attributes.value code 
    , css 
      ([ fontFamily monospace
       , fontSize (Css.pct 100)
       , margin (Css.px 0)
       , border (Css.px 0)
       , borderRight3 borderSize Css.solid theme.border
       , Css.width widthSize
       , Css.padding paddingSize
       , Css.resize Css.none
       , Css.color theme.text
       , Css.backgroundColor theme.primary
       , Css.focus 
         [ outline zero
         ]
       ] ++ attrs)
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

mainOutput : List (Css.Style) -> String -> Html.Styled.Html Msg
mainOutput attrs output = 
  let
    paddingSize = (Css.px 2)
    borderSize = (Css.px 0.125)
    widthSize = CssUtils.calcDimension (Css.pct 100) paddingSize borderSize
  in
  Html.Styled.p
    [ css
      ([ fontFamily monospace
        , fontSize (Css.px 14)
        , Css.width widthSize
        , Css.padding paddingSize
        , Css.margin (Css.px 0)
        , border (Css.px 0)
        , borderRight3 borderSize Css.solid theme.border
        , Css.backgroundColor theme.primary
        , Css.color theme.text
        ] ++ attrs)
    ]
    [Html.Styled.text output]

--mainRepl : 

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