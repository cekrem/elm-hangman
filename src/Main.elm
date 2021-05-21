module Main exposing (..)

import Array
import Browser
import Html exposing (Html, button, div, input, pre, span, text)
import Html.Attributes exposing (disabled, hidden, placeholder, style, type_)
import Html.Events exposing (onClick, onInput)
import Maybe exposing (withDefault)
import Set exposing (Set)


hangmanParts =
    Array.fromList
        [ """
  +---+
  |   |
      |
      |
      |
      |
=========""", """
  +---+
  |   |
  O   |
      |
      |
      |
=========""", """
  +---+
  |   |
  O   |
  |   |
      |
      |
=========""", """
  +---+
  |   |
  O   |
 /|   |
      |
      |
=========""", """
  +---+
  |   |
  O   |
 /|\\  |
      |
      |
=========""", """
  +---+
  |   |
  O   |
 /|\\  |
 /    |
      |
=========""", """
  +---+
  |   |
  O   |
 /|\\  |
 / \\  |
      |
=========""" ]



---- MODEL ----


type Case
    = Lower
    | Upper


type alias Model =
    { guesses : Set String, phrase : String, textCase : Case }


init : ( Model, Cmd Msg )
init =
    ( { guesses = Set.empty, phrase = "Pappa er lur", textCase = Lower }, Cmd.none )



---- UPDATE ----


type Msg
    = Guess String
    | Input String
    | SetCase Case
    | Restart


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Guess char ->
            ( { model | guesses = Set.insert char model.guesses }, Cmd.none )

        Restart ->
            ( { model | guesses = Set.empty }, Cmd.none )

        Input string ->
            ( { model | guesses = Set.empty, phrase = string }, Cmd.none )

        SetCase textCase ->
            ( { model | textCase = textCase }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        caseTransform =
            if model.textCase == Upper then
                String.toUpper

            else
                String.toLower

        phraseList =
            model.phrase
                |> String.toLower
                |> String.split ""

        phraseMapped =
            phraseList
                |> List.map
                    (\char ->
                        if char == " " then
                            " "

                        else if Set.member (String.toLower char) model.guesses then
                            char

                        else
                            "_"
                    )

        mistakes =
            model.guesses
                |> Set.filter (\char -> char /= "")
                |> Set.filter (\char -> not (String.contains char model.phrase))

        hasWon =
            phraseMapped == phraseList && not (String.isEmpty model.phrase)

        hasLost =
            Set.size mistakes == (Array.length hangmanParts - 1)

        hangmanText =
            withDefault "" <|
                Array.get (Set.size mistakes) hangmanParts

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
                    [ style "font-size" <| String.fromInt (min (100 // String.length model.phrase) 20) ++ "vw"
                    , style "letter-spacing" "0.2em"
                    , style "white-space" "nowrap"
                    ]

        buttonsHtml =
            "abcdefghijklmnopqrstuvwxyzæøå"
                |> String.split ""
                |> List.map
                    (\char ->
                        button
                            [ onClick (Guess char)
                            , disabled (Set.member char model.guesses || hasWon || hasLost)
                            , style "width" "4rem"
                            , style "height" "4rem"
                            , style "margin" "0.5rem"
                            , style "color"
                                (if not <| Set.member char model.guesses then
                                    "black"

                                 else if Set.member char mistakes then
                                    "red"

                                 else
                                    "lightgreen"
                                )
                            , hidden <| hasWon && not (Set.member char model.guesses)
                            ]
                            [ text <| caseTransform char ]
                    )
                |> div
                    [ style "display" "flex"
                    , style "margin" "2rem auto"
                    , style "max-width" "40rem"
                    , style "justify-content" "flex-start"
                    , style "flex-wrap" "wrap"
                    ]

        inputHtml =
            input
                [ placeholder "Enter phrase"
                , onInput Input
                , type_ "password"
                ]
                []

        restartButtonHtml =
            button
                [ onClick Restart
                , style "height" "4rem"
                , style "width" "4rem"
                ]
                [ text "↺" ]

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
                ]
                [ span
                    (switchStyle Upper)
                    [ text "A" ]
                , text "/"
                , span
                    (switchStyle Lower)
                    [ text "a" ]
                ]
    in
    div
        [ style "text-align" "center"
        , style "font-family" "monospace"
        , style "font-size" "3rem"
        , style "color"
            (if hasWon then
                "lightgreen"

             else if hasLost then
                "red"

             else
                "black"
            )
        ]
        [ div [] [ text <| caseTransform "hangman" ]
        , pre [ style "font-size" "0.2em" ] [ text hangmanText ]
        , switchHtml
        , inputHtml
        , phraseHtml
        , buttonsHtml
        , mistakesHtml
        , restartButtonHtml
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
