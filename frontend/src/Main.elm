module Main exposing (..)

import Browser
import Css exposing (..)
import Css.Global exposing (..)
import Debug exposing (log)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Generated.Decoder exposing (decodeCodeSubmission)
import Generated.Encoder exposing (encodeCodeSubmission)
import Generated.Types exposing (CodeSubmission)
import Html exposing (Html, div, input, text, textarea)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onInput, preventDefaultOn)
import Html.Styled
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events
import Http exposing (stringBody)
import Json.Decode as Decode
import Json.Encode
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
    , codeOutput : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { mainCode = ""
      , codeOutput = ""
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = TextUpdate String
    | SendPost
    | GotReply (Result Http.Error String)
    | TabDown


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
                    ( { model | codeOutput = fullText }, Cmd.none )

                Err _ ->
                    ( { model | codeOutput = "FAILURE" }, Cmd.none )

        TabDown ->
            ( { model | mainCode = model.mainCode ++ "  " }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Style


theme : { primary : Css.Color, secondary : Css.Color, border : Css.Color, text : Css.Color }
theme =
    { primary = Css.hex "20252d"
    , secondary = Css.hex "1c2027"
    , border = Css.rgb 100 100 120
    , text = Css.hex "ffffff"
    }



-- VIEW


view : Model -> Html.Styled.Html Msg
view model =
    let
        headerHeight =
            Css.vh 10

        replHeight =
            Css.calc (Css.vh 100) Css.minus headerHeight
    in
    Html.Styled.div [ css [ Css.height (Css.pct 100) ] ]
        [ global
            [ class "repl"
                [ fontFamily monospace
                , fontSize (Css.px 14)
                , margin (Css.px 0)
                , Css.color theme.text
                , Css.backgroundColor theme.primary
                , Css.focus
                    [ outline zero
                    ]
                ]
            ]
        , Html.Styled.div
            [ css
                [ Css.height headerHeight
                , Css.width (Css.vw 100)
                , Css.float left
                ]
            ]
            [ mainHeader []
            ]
        , Html.Styled.div
            [ css
                [ Css.height replHeight
                , Css.width (Css.vw 50)
                , Css.float left
                ]
            ]
            [ mainInput [] model.mainCode
            ]
        , Html.Styled.div
            [ css
                [ Css.height replHeight
                , Css.width (Css.vw 50)
                , Css.float left
                ]
            ]
            [ mainOutput [] model.codeOutput
            ]
        ]


mainHeader : List Css.Style -> Html.Styled.Html Msg
mainHeader attrs =
    let
        borderSize =
            Css.px 0.25

        heightSize =
            CssUtils.calcDimension (Css.pct 100) (Css.px 0) borderSize
    in
    Html.Styled.div
        [ css
            ([ Css.backgroundColor theme.secondary
             , Css.minHeight heightSize
             , border (Css.px 0)
             , borderBottom3 borderSize Css.solid theme.border
             ]
                ++ attrs
            )
        ]
        [ Html.Styled.button
            [ Html.Styled.Events.onClick SendPost
            ]
            [ Html.Styled.text "Run" ]
        ]


mainInput : List Css.Style -> String -> Html.Styled.Html Msg
mainInput attrs code =
    let
        paddingSize =
            Css.px 2

        borderSize =
            Css.px 0.125

        widthSize =
            CssUtils.calcDimension (Css.pct 100) paddingSize borderSize

        heightSize =
            CssUtils.calcDimension (Css.pct 100) paddingSize (Css.px 0)
    in
    Html.Styled.textarea
        [ Html.Styled.Events.onInput TextUpdate
        , onTab TabDown
        , Html.Styled.Attributes.value code
        , Html.Styled.Attributes.autocomplete False
        , Html.Styled.Attributes.spellcheck False
        , Html.Styled.Attributes.classList [ ( "repl", True ) ]
        , css
            ([ Css.width widthSize
             , Css.height heightSize
             , Css.padding paddingSize
             , Css.resize Css.none
             , border (Css.px 0)
             , borderRight3 borderSize Css.solid theme.border
             ]
                ++ attrs
            )
        ]
        []


onTab : msg -> Html.Styled.Attribute msg
onTab msg =
    let
        _ =
            Debug.log "onTab" Html.Events.keyCode
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


mainOutput : List Css.Style -> String -> Html.Styled.Html Msg
mainOutput attrs output =
    let
        paddingSize =
            Css.px 2

        borderSize =
            Css.px 0.125

        widthSize =
            CssUtils.calcDimension (Css.pct 100) paddingSize borderSize

        heightSize =
            CssUtils.calcDimension (Css.pct 100) paddingSize (Css.px 0)
    in
    Html.Styled.p
        [ Html.Styled.Attributes.classList [ ( "repl", True ) ]
        , css
            ([ Css.width widthSize
             , Css.height heightSize
             , Css.padding paddingSize
             , border (Css.px 0)
             , borderLeft3 borderSize Css.solid theme.border
             ]
                ++ attrs
            )
        ]
        [ Html.Styled.text output ]
