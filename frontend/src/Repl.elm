module Repl exposing (main)

import Browser
import Css exposing (..)
import Css.Global exposing (..)
import Editor exposing (Editor, EditorConfig, EditorMsg)
import Editor.Config exposing (WrapOption(..), default)
import Editor.Update as E
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import GeneratedTypes.Decoder exposing (decodeCodeSubmission)
import GeneratedTypes.Encoder exposing (encodeCodeSubmission)
import GeneratedTypes.Types exposing (CodeSubmission)
import Html exposing (Html, div, input, text, textarea)
import Html.Attributes exposing (style)
import Html.Events exposing (on, onClick, onInput, preventDefaultOn)
import Html.Styled
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events
import Http exposing (stringBody)
import Json.Decode as Decode
import Json.Encode
import Svg.Styled exposing (svg)
import Svg.Styled.Attributes exposing (height, viewBox, width)



-- A Droplet server running for testing


defaultUrl : String
defaultUrl =
    "http://localhost:3000"



-- Editor config


config : EditorConfig Msg
config =
    { editorMsg = EditorMsg
    , width = 410
    , height = 300
    , lineHeight = 16.0
    , showInfoPanel = False
    , wrapParams = { maximumWidth = 55, optimalWidth = 50, stringWidth = String.length }
    , wrapOption = DoWrap
    , fontProportion = 1.0
    , lineHeightFactor = 1.0
    }



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
    { prefix : String
    , mainCode : String
    , suffix : String
    , codeOutput : String
    , haskellInterpreter : String
    , editor : Editor
    }


init : Decode.Value -> ( Model, Cmd Msg )
init flag =
    ( { prefix = "let concatString :: String -> String -> String"
      , mainCode = "    concatString str1 str2 = str1 ++ str2"
      , suffix = "in concatString \"Hello\" \"World\""
      , codeOutput = ""
      , haskellInterpreter =
            case Decode.decodeValue Decode.string flag of
                Ok url ->
                    url

                Err _ ->
                    defaultUrl
      , editor =
            Editor.init config "testString"
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = TextUpdate String
    | SendPost
    | GotReply (Result Http.Error String)
    | TabDown
    | EditorMsg EditorMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TextUpdate newcontent ->
            ( { model | mainCode = newcontent }, Cmd.none )

        SendPost ->
            let
                fullCode =
                    model.prefix ++ "\n" ++ model.mainCode ++ "\n" ++ model.suffix
            in
            ( model
            , Http.post
                { url = model.haskellInterpreter
                , body =
                    Http.jsonBody
                        (encodeCodeSubmission
                            (CodeSubmission fullCode)
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
            ( { model | mainCode = model.mainCode ++ "\t" }, Cmd.none )

        EditorMsg editorMsg ->
            let
                ( editor, cmd ) =
                    Editor.update editorMsg model.editor
            in
            ( { model | editor = editor }, Cmd.map EditorMsg cmd )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Style


theme : { primary : Css.Color, secondary : Css.Color, light : Css.Color, border : Css.Color, text : Css.Color }
theme =
    { primary = Css.hex "20252d"
    , secondary = Css.hex "1c2027"
    , light = Css.hex "404a5a"
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
    Html.Styled.div []
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
            , class "main_input"
                [ Css.backgroundColor theme.light
                ]
            ]
        , Html.Styled.div
            [ css
                [ Css.height headerHeight
                , Css.width (Css.pct 100)
                , Css.float left
                ]
            ]
            [ mainHeader []
            ]
        , Html.Styled.div
            [ css [ Css.height replHeight ] ]
            [ Html.Styled.div
                [ css
                    [ Css.height (Css.pct 100)
                    , Css.width (Css.pct 50)
                    , Css.float left
                    , Css.backgroundColor theme.primary
                    , Css.overflowY scroll
                    , Css.overflowX hidden
                    ]
                ]
                [ hardCoded [ Css.height auto ] model.prefix
                , Html.Styled.div
                    [ css [ Css.color theme.text ] ]
                    [ Html.Styled.fromUnstyled (Editor.embedded config model.editor) ]
                , hardCoded [ Css.height auto ] model.suffix
                ]
            , Html.Styled.div
                [ css
                    [ Css.height replHeight
                    , Css.width (Css.pct 50)
                    , Css.float left
                    ]
                ]
                [ mainOutput [] model.codeOutput ]
            ]
        ]


mainHeader : List Css.Style -> Html.Styled.Html Msg
mainHeader attrs =
    let
        borderSize =
            Css.px 0.25

        heightSize =
            Css.pct 100
    in
    Html.Styled.div
        [ css
            ([ Css.backgroundColor theme.secondary
             , Css.minHeight heightSize
             , Css.height heightSize
             , border (Css.px 0)
             , borderBottom3 borderSize Css.solid theme.border
             ]
                ++ attrs
            )
        ]
        [ Html.Styled.div
            [ css
                [ Css.displayFlex
                , Css.height (Css.pct 100)
                ]
            ]
            [ Html.Styled.a
                [ Html.Styled.Events.onClick SendPost
                , css
                    [ Css.cursor Css.pointer
                    , Css.color theme.text
                    , Css.fontFamily monospace
                    , Css.displayFlex
                    , Css.alignItems Css.center
                    , Css.marginLeft (Css.vw 1)
                    ]
                ]
                [ playButton []
                , Html.Styled.text "Run"
                ]
            ]
        ]


playButton : List Css.Style -> Html.Styled.Html Msg
playButton attrs =
    let
        borderSize =
            Css.px 0.25
    in
    Svg.Styled.svg
        [ Svg.Styled.Attributes.width "16"
        , Svg.Styled.Attributes.height "16"
        , Svg.Styled.Attributes.viewBox "0 0 24 24"
        , Svg.Styled.Attributes.fill "none"
        , Svg.Styled.Attributes.stroke "white"
        , Svg.Styled.Attributes.strokeWidth "1.4"
        , Svg.Styled.Attributes.strokeLinecap "round"
        ]
        [ Svg.Styled.polygon
            [ Svg.Styled.Attributes.points "5,3,19,12,5,21,5,3" ]
            []
        ]


getHardcodeDisplay : String -> Css.Style
getHardcodeDisplay code =
    if String.length code == 0 then
        Css.display Css.none

    else
        Css.display Css.block


hardCoded : List Css.Style -> String -> Html.Styled.Html Msg
hardCoded attrs code =
    let
        borderSize =
            Css.px 0.125

        widthSize =
            Css.pct 100

        heightSize =
            Css.pct 100

        rows =
            List.length (String.lines code)

        displayStyle =
            getHardcodeDisplay code
    in
    Html.Styled.textarea
        [ Html.Styled.Attributes.value code
        , Html.Styled.Attributes.classList [ ( "repl", True ) ]
        , Html.Styled.Attributes.readonly True
        , Html.Styled.Attributes.rows rows
        , css
            ([ Css.width widthSize
             , Css.height heightSize
             , Css.resize Css.none
             , Css.verticalAlign top
             , border (Css.px 0)
             , displayStyle
             ]
                ++ attrs
            )
        ]
        []


mainInput : List Css.Style -> String -> Html.Styled.Html Msg
mainInput attrs code =
    let
        borderSize =
            Css.px 0.125

        widthSize =
            Css.pct 100

        heightSize =
            Css.auto

        minHeightSize =
            Css.pct 50

        rows =
            List.length (String.lines code)
    in
    Html.Styled.textarea
        [ Html.Styled.Events.onInput TextUpdate
        , onTab TabDown
        , Html.Styled.Attributes.value code
        , Html.Styled.Attributes.autocomplete False
        , Html.Styled.Attributes.spellcheck False
        , Html.Styled.Attributes.classList [ ( "repl", True ), ( "main_input", True ) ]
        , Html.Styled.Attributes.rows rows
        , css
            ([ Css.width widthSize
             , Css.height heightSize
             , Css.minHeight minHeightSize
             , Css.resize Css.none
             , Css.verticalAlign top
             , border (Css.px 0)
             , Css.backgroundColor theme.light
             ]
                ++ attrs
            )
        ]
        []


onTab : msg -> Html.Styled.Attribute msg
onTab msg =
    let
        isTabKey keyCode =
            if keyCode == 9 then
                Decode.succeed msg

            else
                Decode.fail "It's not a tab key :)"
    in
    Html.Events.keyCode
        |> Decode.andThen isTabKey
        |> Decode.map (\x -> { message = x, stopPropagation = True, preventDefault = True })
        |> Html.Styled.Events.custom "keydown"


mainOutput : List Css.Style -> String -> Html.Styled.Html Msg
mainOutput attrs output =
    let
        borderSize =
            Css.px 0.125

        widthSize =
            Css.pct 100

        heightSize =
            Css.pct 100
    in
    Html.Styled.p
        [ Html.Styled.Attributes.classList [ ( "repl", True ) ]
        , css
            ([ Css.width widthSize
             , Css.height heightSize
             , border (Css.px 0)
             , borderLeft3 borderSize Css.solid theme.border
             ]
                ++ attrs
            )
        ]
        [ Html.Styled.text output ]
