module Main exposing (Model, Msg(..), init, main, update, view)

import Array exposing (Array)
import Browser exposing (Document)
import Browser.Navigation as Navigation exposing (Key)
import Html exposing (Html, button, div, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder)
import Random exposing (Generator)
import Random.Array
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)


jsonAddress : String
jsonAddress =
    "https://gist.githubusercontent.com/plaxdan/351914a273548314b328328f4516d97b/raw/ed07210e8fbcaab92ed8b319e333558137d25a84/questions.json"


type alias Question =
    { categoryId : String
    , description : String
    }


type alias Model =
    { page : Maybe Page
    , key : Key
    , diceValues : Array Int
    , questions : List Question
    }


type alias Flags =
    ()


type Page
    = HomePage


type Route
    = HomeRoute


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | RollDice
    | DiceRolled (Array Int)
    | FetchJson
    | JsonFetched (Result Http.Error (List Question))


view : Model -> Document Msg
view model =
    case model.page of
        Just HomePage ->
            viewHome model

        Nothing ->
            viewNotFound


viewDice : Int -> Html msg
viewDice int =
    div [ class "font-mono px-4 py-2 rounded bg-black text-white" ]
        [ text (String.fromInt int) ]


viewDiceCard : Array Int -> Html Msg
viewDiceCard dice =
    viewCard
        [ viewCardTitle "Roll Six Dice"
        , viewButton RollDice "Let's Roll!"
        , div [ class "grid grid-flow-row grid-cols-3 grid-rows-2 gap-3" ]
            (Array.map viewDice dice |> Array.toList)
        ]


viewButton : Msg -> String -> Html Msg
viewButton action label =
    button
        [ class "bg-gray-300 px-6 py-2 rounded-full hover:bg-gray-500 hover:text-white hover:rounded-lg"
        , onClick action
        ]
        [ text label ]


viewCardTitle : String -> Html Msg
viewCardTitle title =
    div
        [ class "text-xl font-medium text-black" ]
        [ p [] [ text title ] ]


viewCard : List (Html Msg) -> Html Msg
viewCard children =
    div [ class "flex flex-col space-y-6 p-6 max-w-lg mx-auto bg-gray-100 rounded-xl shadow-md space-x-4" ]
        children


viewQuestion : Question -> Html Msg
viewQuestion { categoryId, description } =
    div [ class "flex flex-col space-y-6 p-6 max-w-lg mx-auto bg-white rounded-xl space-x-4" ]
        [ p [] [ text categoryId ]
        , p [] [ text description ]
        ]


viewQuestions : List Question -> Html Msg
viewQuestions questions =
    div [ class "flex flex-col space-y-6" ] <|
        List.map viewQuestion questions


viewQuestionsCard : List Question -> Html Msg
viewQuestionsCard questions =
    viewCard
        [ viewCardTitle "Fetch and Decode Question JSON"
        , viewButton FetchJson "Fetch JSON"
        , viewQuestions questions
        ]


viewHome : { a | diceValues : Array Int, questions : List Question } -> Document Msg
viewHome { diceValues, questions } =
    { title = "Elm Starter | Home"
    , body =
        [ div
            [ class "p-20 space-y-4" ]
            [ viewDiceCard diceValues
            , viewQuestionsCard questions
            ]
        ]
    }


viewNotFound : Document Msg
viewNotFound =
    { title = "Elm Starter | Not Found"
    , body =
        [ text "Not Found"
        ]
    }


rollDice : Generator (Array Int)
rollDice =
    Random.Array.array 6 (Random.int 1 6)


questionDecoder : Decoder Question
questionDecoder =
    Json.Decode.field "question" <|
        Json.Decode.map2 Question
            (Json.Decode.field "category" Json.Decode.string)
            (Json.Decode.field "description" Json.Decode.string)


rootQuestionGroupDecoder : Decoder (List Question)
rootQuestionGroupDecoder =
    Json.Decode.field
        "rootQuestionGroup"
        (Json.Decode.list questionDecoder)


fetchJson : List Question -> Cmd Msg
fetchJson questions =
    if List.length questions > 0 then
        Cmd.none

    else
        Http.get
            { url = jsonAddress
            , expect = Http.expectJson JsonFetched rootQuestionGroupDecoder
            }


handleJsonResult : { a | questions : value } -> Result error value -> { a | questions : value }
handleJsonResult model result =
    case result of
        Ok questions ->
            { model | questions = questions }

        Err _ ->
            -- TODO display error
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked (Browser.Internal link) ->
            ( model, Navigation.pushUrl model.key (Url.toString link) )

        LinkClicked (Browser.External link) ->
            ( model, Navigation.load link )

        UrlChanged url ->
            updateUrl url model

        RollDice ->
            ( model, Random.generate DiceRolled rollDice )

        DiceRolled diceValues ->
            ( { model | diceValues = diceValues }, Cmd.none )

        FetchJson ->
            ( model, fetchJson model.questions )

        JsonFetched result ->
            ( handleJsonResult model result, Cmd.none )


updateUrl : Url -> Model -> ( Model, Cmd Msg )
updateUrl url model =
    case Parser.parse routeParser url of
        Just HomeRoute ->
            ( { model | page = Just HomePage }, Cmd.none )

        Nothing ->
            ( { model | page = Nothing }, Cmd.none )


routeParser : Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map HomeRoute Parser.top
        ]


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    updateUrl url
        { page = Just HomePage
        , key = key
        , diceValues = Array.initialize 6 ((+) 1)
        , questions = []
        }


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
