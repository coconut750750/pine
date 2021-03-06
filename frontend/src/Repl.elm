module Repl exposing (main)

import Browser
import Css exposing (..)
import Css.Global exposing (..)
import Dict
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
import Html.Events exposing (custom, on, onClick, onInput, preventDefaultOn)
import Html.Styled
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events
import Http exposing (stringBody)
import Json.Decode as Decode
import Json.Encode
import Regex
import Svg.Styled exposing (svg)
import Svg.Styled.Attributes exposing (height, viewBox, width)


{-| The URL of a DigitalOcean server running for test/demoing
-}
defaultUrl : String
defaultUrl =
    "http://159.203.88.220:3000"


{-| Change this to Css.column if you want the output window to go below the input,
rather than having it next to the input.
-}
replOrientation : Css.FlexDirectionOrWrap (Css.FlexDirection {})
replOrientation =
    Css.row



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
    { prefixCode : String
    , infixCode : String
    , suffixCode : String
    , haskellInterpreter : String
    , outputLines : List String
    }


{-| This initializes our Repl's model. It decodes any flags passed in as a dictionary, and
sets corresponding model values to either the input or default.
-}
init : Decode.Value -> ( Model, Cmd Msg )
init flags =
    let
        flagsDict =
            case Decode.decodeValue (Decode.dict Decode.string) flags of
                Ok dict ->
                    dict

                Err _ ->
                    Dict.empty
    in
    ( { prefixCode =
            case Dict.get "prefix" flagsDict of
                Just prefix ->
                    prefix

                Nothing ->
                    ""
      , infixCode =
            case Dict.get "infix" flagsDict of
                Just infix ->
                    infix

                Nothing ->
                    ""
      , suffixCode =
            case Dict.get "suffix" flagsDict of
                Just suffix ->
                    suffix

                Nothing ->
                    ""
      , haskellInterpreter =
            case Dict.get "interpreter" flagsDict of
                Just interpreter ->
                    interpreter

                Nothing ->
                    defaultUrl
      , outputLines = []
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = TextUpdate String
    | SendPost
    | GotReply (Result Http.Error String)
    | TabDown Decode.Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TextUpdate newcontent ->
            ( { model | infixCode = newcontent }, Cmd.none )

        SendPost ->
            let
                fullCode =
                    model.prefixCode ++ "\n" ++ model.infixCode ++ "\n" ++ model.suffixCode
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
                    ( { model | outputLines = String.split "\\n" (unescape fullText) }, Cmd.none )

                Err err ->
                    ( { model | outputLines = [ stringFromError err ] }, Cmd.none )

        TabDown event ->
            let
                ( start, end ) =
                    getSelectionRange event

                left =
                    String.left start model.infixCode

                right =
                    String.dropLeft end model.infixCode
            in
            if start == end then
                ( { model | infixCode = left ++ "    " ++ right }, Cmd.none )

            else
                ( { model | infixCode = "    " ++ model.infixCode }, Cmd.none )


{-| The response from the haskell server has some escaped unicode characters.
An example response might be:

    WontCompile [ GhcError { errMsg = "<hint>:5:22: error: parse error on input \\8216;\\8217" } ]

(Actual response has single backslashes, but the Elm formatter keeps forces double backslashes)

However, the string we want to show the user wouldn't have the escaped Unicode character codes.

    WontCompile [ GhcError { errMsg = "<hint>:5:22: error: parse error on input ‘;’" } ]

And this function 'unescapes' the characters from the first string to the second.

-}
unescape : String -> String
unescape rawString =
    case Regex.fromString "\\\\\\d\\d\\d\\d" of
        Nothing ->
            rawString

        Just regex ->
            Regex.replace regex (.match >> toInt2 >> Char.fromCode >> String.fromChar) rawString


toInt2 match =
    case String.toInt (String.dropLeft 1 match) of
        Just charCode ->
            charCode

        Nothing ->
            -1


{-| Convert Http.Error into a printable string to show the user
-}
stringFromError : Http.Error -> String
stringFromError err =
    case err of
        Http.BadUrl url ->
            "Bad url: " ++ url

        Http.Timeout ->
            "Timed out waiting for server response"

        Http.NetworkError ->
            "Network Error"

        Http.BadStatus statusCode ->
            "Returned bad status code: " ++ String.fromInt statusCode

        Http.BadBody errString ->
            errString


{-| Decode a keyDown event Value to extract the event.target.selectionStart and selectionEnd properties
-}
getSelectionRange : Decode.Value -> ( Int, Int )
getSelectionRange event =
    let
        startIdx =
            case Decode.decodeValue (Decode.maybe (Decode.at [ "target", "selectionStart" ] Decode.int)) event of
                Ok (Just selectionStart) ->
                    selectionStart

                _ ->
                    0

        endIdx =
            case Decode.decodeValue (Decode.maybe (Decode.at [ "target", "selectionEnd" ] Decode.int)) event of
                Ok (Just selectionEnd) ->
                    selectionEnd

                _ ->
                    0
    in
    ( startIdx, endIdx )



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
            Css.em 2

        replHeight =
            Css.pct 100
    in
    Html.Styled.div
        [ css
            [ Css.height (Css.pct 100)
            , Css.width (Css.pct 100)
            , Css.displayFlex
            , Css.flexDirection Css.column
            ]
        ]
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
            [ css
                [ Css.height replHeight
                , Css.displayFlex
                , Css.flexDirection replOrientation
                ]
            ]
            [ Html.Styled.span
                [ css
                    [ Css.backgroundColor theme.primary
                    , Css.overflowY hidden
                    , Css.overflowX hidden
                    , Css.flex (Css.num 2)
                    ]
                ]
                [ readOnly [ Css.height auto ] model.prefixCode
                , mainInput [ Css.height auto ] model.infixCode
                , readOnly [ Css.height auto ] model.suffixCode
                ]
            , Html.Styled.span
                [ css
                    [ Css.backgroundColor theme.primary
                    , Css.overflowY hidden
                    , Css.flex (Css.num 1)
                    ]
                ]
                (mainOutput [] model.outputLines)
            ]
        ]


{-| The top header of the REPL that contains the "Run" button
-}
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


{-| SVG play icon for the "Run" button
-}
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


{-| Helper function to remove the readOnly displays when they don't have code to present.
-}
getReadonlyDisplay : String -> Css.Style
getReadonlyDisplay code =
    if String.length code == 0 then
        Css.display Css.none

    else
        Css.display Css.block


{-| The read-only panels in the REPL with prepared code
-}
readOnly : List Css.Style -> String -> Html.Styled.Html Msg
readOnly attrs code =
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
            getReadonlyDisplay code
    in
    Html.Styled.textarea
        [ Html.Styled.Attributes.value code
        , Html.Styled.Attributes.classList [ ( "repl", True ) ]
        , Html.Styled.Attributes.readonly True
        , Html.Styled.Attributes.rows rows
        , css
            ([ Css.width widthSize
             , Css.height auto
             , Css.resize Css.none
             , Css.verticalAlign top
             , border (Css.px 0)
             , displayStyle
             ]
                ++ attrs
            )
        ]
        []


{-| The input panel in the REPL
-}
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
             , Css.resize Css.none
             , Css.verticalAlign top
             , border (Css.px 0)
             , Css.backgroundColor theme.light
             ]
                ++ attrs
            )
        ]
        []


{-| Html attribute to listen for keyDown events and emit the given Msg if tab is pressed
-}
onTab : (Decode.Value -> msg) -> Html.Styled.Attribute msg
onTab toMsg =
    let
        isTabKey keyCode =
            if keyCode == 9 then
                Decode.value

            else
                Decode.fail "It's not a tab key :)"
    in
    Html.Events.keyCode
        |> Decode.andThen isTabKey
        |> Decode.map toMsg
        |> Decode.map (\x -> { message = x, stopPropagation = True, preventDefault = True })
        |> custom "keydown"
        |> Html.Styled.Attributes.fromUnstyled


{-| The panel in the REPL that shows the haskell output
-}
mainOutput : List Css.Style -> List String -> List (Html.Styled.Html Msg)
mainOutput attrs lineStrings =
    List.map (htmlFromLine attrs) lineStrings


htmlFromLine : List Css.Style -> String -> Html.Styled.Html Msg
htmlFromLine attrs line =
    let
        borderSize =
            Css.px 0.125

        widthSize =
            Css.pct 100

        heightSize =
            Css.auto
    in
    Html.Styled.p
        [ Html.Styled.Attributes.classList [ ( "repl", True ) ]
        , css
            ([ Css.width widthSize
             , border (Css.px 0)
             , borderLeft3 borderSize Css.solid theme.border
             ]
                ++ attrs
            )
        ]
        [ Html.Styled.text line ]
