module Main exposing (..)

import Array
import Ascii exposing (hangmanParts)
import Browser
import Browser.Events
import Html exposing (Html, button, div, pre, span, text)
import Html.Attributes exposing (disabled, style)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, int, string)
import Json.Decode.Pipeline exposing (required)
import Maybe exposing (withDefault)
import Random
import Set exposing (Set)



---- MODEL ----


type Case
    = Lower
    | Upper


type GameState
    = Playing Record
    | ChangePhrase String


type alias Record =
    { phrase : String, guesses : Set String }


wrongChars : Record -> Set String
wrongChars record =
    record.guesses
        |> Set.filter (\char -> char /= "")
        |> Set.filter (\char -> not (String.contains char record.phrase))


hasWon : Record -> Bool
hasWon record =
    record.phrase
        |> String.split ""
        |> List.all
            (\char -> char == " " || Set.member char record.guesses)


hasLost : Record -> Bool
hasLost record =
    Set.size (wrongChars record) == (Array.length hangmanParts - 1)


gameOver : Record -> Bool
gameOver record =
    hasWon record || hasLost record


type alias Model =
    { textCase : Case, loading : Bool, gameState : GameState }


initialModel : Model
initialModel =
    { textCase = Lower, loading = False, gameState = ChangePhrase "" }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



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
    | Fetch
    | FetchSeed Int
    | Fetched (Result Http.Error Post)


type alias Post =
    { id : Int
    , title : String
    }


phraseUrl : String
phraseUrl =
    "https://jsonplaceholder.typicode.com/posts/"


postDecoder : Decoder Post
postDecoder =
    Decode.succeed Post
        |> required "id" int
        |> required "title" string


fetchPhrase : Int -> Cmd Msg
fetchPhrase postNum =
    Http.get
        { url = phraseUrl ++ String.fromInt postNum
        , expect = Http.expectJson Fetched postDecoder
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Restart ->
            ( { model | gameState = ChangePhrase "" }, Cmd.none )

        SetCase textCase ->
            ( { model | textCase = textCase }, Cmd.none )

        Input keyEvent ->
            case model.gameState of
                Playing record ->
                    case keyEvent of
                        Char char ->
                            if gameOver record then
                                update NoOp model

                            else
                                ( { model | gameState = Playing { record | guesses = Set.insert char record.guesses } }, Cmd.none )

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
                            if phrase == "" then
                                update NoOp model

                            else
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

        Fetch ->
            ( { model | loading = True }, Random.generate FetchSeed (Random.int 1 100) )

        FetchSeed seed ->
            ( model, fetchPhrase seed )

        Fetched (Ok post) ->
            ( { model | loading = False, gameState = Playing { phrase = post.title, guesses = Set.empty } }, Cmd.none )

        Fetched (Err httpError) ->
            let
                _ =
                    Debug.log "http error" httpError
            in
            ( initialModel, Cmd.none )



---- VIEW ----


buttonGroupStyle =
    [ style "display" "flex"
    , style "justify-content" "center"
    ]


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

        rootDiv loading color content =
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
                , style "transition" "linear opacity 0.1s"
                , style "opacity" <|
                    if loading then
                        "0.2"

                    else
                        "1"
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
                    wrongChars record

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

                gameOverText =
                    if hasWon record then
                        "du vant!"

                    else if hasLost record then
                        "du tapte!"

                    else
                        ""
            in
            rootDiv model.loading
                (if hasWon record then
                    "green"

                 else if hasLost record then
                    "red"

                 else
                    "black"
                )
                [ generateHangman hangmanText
                , div [] [ text <| caseTransform gameOverText ]
                , phraseHtml
                , mistakesHtml
                , restartButtonHtml
                ]

        ChangePhrase phrase ->
            let
                renderedPhrase =
                    if String.isEmpty phrase then
                        "skriv inn ord/setning:"

                    else
                        phrase
                            |> String.split ""
                            |> List.map
                                (\char ->
                                    if char == " " then
                                        "|"

                                    else
                                        "_"
                                )
                            |> String.join ""
            in
            rootDiv model.loading
                "black"
                [ generateHangman initialHangmanText
                , div
                    [ style "font-size" <|
                        String.fromInt (min (80 // String.length renderedPhrase) 10)
                            ++ "vw"
                    , style "letter-spacing" "0.2em"
                    , style "white-space" "nowrap"
                    ]
                    [ text renderedPhrase
                    ]
                , div buttonGroupStyle
                    [ button
                        (disabled (String.isEmpty phrase) :: (onClick Start :: buttonStyle))
                        [ text "Start" ]
                    , button
                        (onClick Fetch :: buttonStyle)
                        [ text "Generer setning" ]
                    ]
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
