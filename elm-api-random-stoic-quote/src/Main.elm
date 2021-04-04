module Main exposing (Model(..), main)

import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, map2, string)



-- MAIN


main : Program () Model Message
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Quote =
    { text : String
    , author : String
    }


type Model
    = Failure
    | Loading
    | Success Quote


init : () -> ( Model, Cmd Message )
init _ =
    ( Loading, getRandomStoicQuote )



-- UPDATE


type Message
    = AnotherQuotePlease
    | GotQuote (Result Http.Error Quote)


update : Message -> Model -> ( Model, Cmd Message )
update message _ =
    case message of
        AnotherQuotePlease ->
            ( Loading, getRandomStoicQuote )

        GotQuote result ->
            case result of
                Ok resp ->
                    ( Success resp, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Message
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Message
view model =
    div []
        [ h2 [] [ text "Random stoic quotes" ]
        , viewQuote model
        ]


viewQuote : Model -> Html Message
viewQuote model =
    case model of
        Failure ->
            div []
                [ h3 [] [ text "I was unable to load a random stoic quote. " ]
                , button [ onClick AnotherQuotePlease ] [ text "Try again" ]
                ]

        Loading ->
            text "Loading..."

        Success response ->
            div []
                [ button [ onClick AnotherQuotePlease ] [ text "Get another quote" ]
                , h3
                    []
                    [ text (response.text ++ " - " ++ formatAuthor response.author)
                    ]
                ]


getRandomStoicQuote : Cmd Message
getRandomStoicQuote =
    Http.get
        { url = "https://stoic-quotes.com/api/quote"
        , expect = Http.expectJson GotQuote quoteDecoder
        }


quoteDecoder : Decoder Quote
quoteDecoder =
    map2 Quote
        (field "text" string)
        (field "author" string)


formatAuthor : String -> String
formatAuthor author =
    case author of
        "Seneca" ->
            "Seneca the Younger"

        _ ->
            author
