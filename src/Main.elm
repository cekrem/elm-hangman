module Main exposing (..)

import Array
import Ascii exposing (hangmanParts)
import Browser
import Browser.Events
import Html exposing (Html, button, div, pre, span, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Maybe exposing (withDefault)
import Set exposing (Set)



---- MODEL ----


type Case
    = Lower
    | Upper


type GameState
    = Playing { phrase : String, guesses : Set String }
    | ChangePhrase String


type alias ModelOld =
    { guesses : Set String, phrase : String, textCase : Case }


type alias Model =
    { textCase : Case, gameState : GameState }


init : ( Model, Cmd Msg )
init =
    ( { textCase = Lower, gameState = Playing { guesses = Set.empty, phrase = "Pappa er lur" } }, Cmd.none )



---- UPDATE ----


type KeyEvent
    = Char String
    | Enter
    | Backspace
    | Space


type Msg
    = Input KeyEvent
    | SetCase Case
    | Restart
    | Start
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Restart ->
            ( { model | gameState = ChangePhrase "" }, Cmd.none )

        SetCase textCase ->
            ( { model | textCase = textCase }, Cmd.none )

        Input keyEvent ->
            case model.gameState of
                Playing prevState ->
                    case keyEvent of
                        Char char ->
                            ( { model | gameState = Playing { prevState | guesses = Set.insert char prevState.guesses } }, Cmd.none )

                        Enter ->
                            update Restart model

                        _ ->
                            update NoOp model

                ChangePhrase phrase ->
                    case keyEvent of
                        Char char ->
                            ( { model | gameState = ChangePhrase (phrase ++ char) }, Cmd.none )

                        Enter ->
                            update Start model

                        Backspace ->
                            ( { model | gameState = ChangePhrase <| String.dropRight 1 phrase }, Cmd.none )

                        Space ->
                            ( { model
                                | gameState =
                                    ChangePhrase ((phrase |> String.trim) ++ " ")
                              }
                            , Cmd.none
                            )

        Start ->
            case model.gameState of
                ChangePhrase phrase ->
                    let
                        trimmedPhrase =
                            String.trim phrase
                    in
                    if trimmedPhrase |> String.isEmpty then
                        update NoOp model

                    else
                        ( { model | gameState = Playing { phrase = phrase |> String.trim, guesses = Set.empty } }, Cmd.none )

                Playing _ ->
                    update NoOp model

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


buttonStyle =
    [ style "height" "4rem"
    , style "width" "4rem"
    , style "margin" "1rem"
    ]


generateHangman content =
    pre [ style "font-size" "0.4em" ] [ text content ]


view : Model -> Html Msg
view model =
    let
        caseTransform =
            if model.textCase == Upper then
                String.toUpper

            else
                String.toLower

        switchStyle textCase =
            if textCase /= model.textCase then
                [ style "color"
                    "rgba(0,0,0,0.2)"
                , style "cursor" "pointer"
                , onClick (SetCase textCase)
                ]

            else
                [ style "cursor" "default"
                ]

        switchHtml =
            div
                [ style "font-size" "1rem"
                , style "position" "fixed"
                , style "top" "1rem"
                , style "right" "1rem"
                ]
                [ span
                    (switchStyle Upper)
                    [ text "A" ]
                , text "/"
                , span
                    (switchStyle Lower)
                    [ text "a" ]
                ]

        hangmanTitle =
            div [] [ text <| caseTransform "hangman" ]

        initialHangmanText =
            withDefault ""
                (Array.get 0 hangmanParts)

        rootDiv color content =
            div
                [ style "text-align" "center"
                , style "font-family" "monospace"
                , style "font-size" "3rem"
                , style "color" color
                , style "display" "flex"
                , style "flex-direction" "column"
                , style "align-items" "center"
                , style "justify-content" "space-between"
                , style "box-sizing" "border-box"
                , style "height" "100vh"
                , style "padding" "10vw"
                ]
                (hangmanTitle :: switchHtml :: content)
    in
    case model.gameState of
        Playing record ->
            let
                phraseList =
                    record.phrase
                        |> String.toLower
                        |> String.split ""

                phraseMapped =
                    phraseList
                        |> List.map
                            (\char ->
                                if char == " " then
                                    " "

                                else if Set.member (String.toLower char) record.guesses then
                                    char

                                else
                                    "_"
                            )

                mistakes =
                    record.guesses
                        |> Set.filter (\char -> char /= "")
                        |> Set.filter (\char -> not (String.contains char record.phrase))

                hasWon =
                    phraseMapped == phraseList && not (String.isEmpty record.phrase)

                hasLost =
                    Set.size mistakes == (Array.length hangmanParts - 1)

                hangmanText =
                    withDefault initialHangmanText
                        (Array.get (Set.size mistakes) hangmanParts)

                mistakesHtml =
                    mistakes
                        |> Set.toList
                        |> List.map
                            (\char ->
                                span
                                    [ style "color" "red"
                                    ]
                                    [ text <| caseTransform char ]
                            )
                        |> div []

                phraseHtml =
                    phraseMapped
                        |> List.map
                            (\char ->
                                span [] [ text <| caseTransform char ]
                            )
                        |> div
                            [ style "font-size" <| String.fromInt (min (80 // String.length record.phrase) 10) ++ "vw"
                            , style "letter-spacing" "0.2em"
                            , style "white-space" "nowrap"
                            ]

                restartButtonHtml =
                    button
                        (onClick Restart :: buttonStyle)
                        [ text "↺" ]
            in
            rootDiv
                (if hasWon then
                    "green"

                 else if hasLost then
                    "red"

                 else
                    "black"
                )
                [ generateHangman hangmanText
                , phraseHtml
                , mistakesHtml
                , restartButtonHtml
                ]

        ChangePhrase phrase ->
            let
                emptyPhrase =
                    String.isEmpty phrase
            in
            rootDiv "black"
                [ generateHangman initialHangmanText
                , div []
                    [ text <| caseTransform "Type in new phrase:"
                    ]
                , div
                    [ style "font-size" <|
                        String.fromInt (min (80 // String.length phrase) 10)
                            ++ "vw"
                    , style "letter-spacing" "0.2em"
                    , style "white-space" "nowrap"
                    ]
                    [ text
                        (phrase
                            |> String.split ""
                            |> List.map
                                (\char ->
                                    if char == " " then
                                        "|"

                                    else
                                        "_"
                                )
                            |> String.join ""
                        )
                    ]
                , button
                    (onClick Start :: buttonStyle)
                    [ text "Start" ]
                ]



---- PROGRAM ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyDown keyDecoder


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


toKey : String -> Msg
toKey string =
    case String.uncons string of
        Just ( char, "" ) ->
            let
                charString =
                    String.fromChar char
            in
            if charString == " " then
                Input Space

            else
                Input <| Char charString

        _ ->
            if string == "Backspace" then
                Input Backspace

            else if string == "Enter" then
                Input Enter

            else
                NoOp


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
