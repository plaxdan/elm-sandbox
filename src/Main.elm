module Main exposing (Model, Msg(..), init, main, update, view)

import Browser exposing (Document)
import Browser.Navigation as Navigation exposing (Key)
import Element exposing (..)
import Element.Font as Font
import Html exposing (Html)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)


type alias Model =
    { page : Page, key : Key }


type alias Flags =
    ()


type Page
    = HomePage
    | NotFound


type Route
    = Home


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url


view : Model -> Document Msg
view model =
    case model.page of
        HomePage ->
            viewHome

        NotFound ->
            viewNotFound


viewLayout : Html msg
viewLayout =
    layout [] <|
        el [ centerX, centerY, padding 50 ] <|
            paragraph
                [ Font.size 48, Font.center ]
                [ text "Welcome to "
                , el [ Font.italic ] <| text "this"
                , text " page!"
                ]


viewHome : Document Msg
viewHome =
    { title = "Elm Starter | Home"
    , body = [ viewLayout ]
    }


viewNotFound : Document Msg
viewNotFound =
    { title = "Elm Starter | Not Found"
    , body = [ layout [] <| text "Not Found" ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked (Browser.Internal url) ->
            ( model, Navigation.pushUrl model.key (Url.toString url) )

        LinkClicked (Browser.External url) ->
            ( model, Navigation.load url )

        UrlChanged url ->
            updateUrl url model


updateUrl : Url -> Model -> ( Model, Cmd Msg )
updateUrl url model =
    case Parser.parse routeParser url of
        Just Home ->
            ( { model | page = HomePage }, Cmd.none )

        Nothing ->
            ( { model | page = NotFound }, Cmd.none )


routeParser : Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map Home Parser.top
        ]


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    updateUrl url { page = HomePage, key = key }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
