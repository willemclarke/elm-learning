module Main exposing (..)

import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (height, placeholder, src, style, value, width)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (required)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias ImdbResponse =
    { title : String
    , year : String
    , genre : String
    , plot : String
    , actors : String
    , director : String
    , poster : String
    , imdbRating : String
    , imdbVotes : String
    }


type MovieRequest
    = Idle
    | Failure
    | Loading
    | Success ImdbResponse


type alias Model =
    { searchTerm : String
    , movie : MovieRequest
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { searchTerm = "", movie = Idle }, Cmd.none )



-- UPDATE


type Msg
    = ChangeInput String
    | FetchMovie
    | GotMovie (Result Http.Error ImdbResponse)


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        ChangeInput newInput ->
            ( { model | searchTerm = newInput }, Cmd.none )

        FetchMovie ->
            ( { model | movie = Loading }, getMovie model.searchTerm )

        GotMovie result ->
            case result of
                Ok resp ->
                    ( { model | movie = Success resp }, Cmd.none )

                Err _ ->
                    ( { model | movie = Failure }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "padding-top" "10px"
        , style "height" "100vh"
        , style "background-color" "#F7FAFC"
        ]
        [ Html.form
            [ onSubmit FetchMovie ]
            [ input [ placeholder "Enter movie/tv title", value model.searchTerm, onInput ChangeInput ] []
            , if String.isEmpty model.searchTerm then
                h4 [ style "padding-top" "-2" ] [ text "Enter a movie/tv series title" ]

              else
                viewMovie model.movie
            ]
        ]


viewMovie : MovieRequest -> Html Msg
viewMovie movieRequest =
    case movieRequest of
        Idle ->
            text ""

        Failure ->
            h3 [] [ text "Failed to find/load resource" ]

        Loading ->
            h3 [] [ text "Loading..." ]

        Success response ->
            div
                [ style "width" "500px"
                , style "align-items" "center"
                ]
                [ h4 [] [ text ("Title: " ++ response.title) ]
                , img [ src response.poster, width 500, height 400 ] []
                , h4 [] [ text ("Genres: " ++ response.genre) ]
                , h4 [] [ text ("Plot: " ++ response.plot) ]
                , h4 [] [ text ("Year made: " ++ response.year) ]
                , h4 [] [ text ("Actors: " ++ response.actors) ]
                , h4 [] [ text ("Director: " ++ response.director) ]
                , h4 [] [ text ("Rated: " ++ response.imdbRating ++ " with " ++ response.imdbVotes ++ " votes") ]
                ]



-- Fix this up later


getMovie : String -> Cmd Msg
getMovie title =
    Http.get
        { url = String.concat [ "http://www.omdbapi.com/?t=", title, "&plot=full&apikey=cb69da09" ]
        , expect = Http.expectJson GotMovie movieDecoder
        }



-- Fix this, not mapped for imdb response


movieDecoder : Decoder ImdbResponse
movieDecoder =
    Decode.succeed ImdbResponse
        |> required "Title" string
        |> required "Plot" string
        |> required "Genre" string
        |> required "Year" string
        |> required "Actors" string
        |> required "Director" string
        |> required "Poster" string
        |> required "imdbRating" string
        |> required "imdbVotes" string
