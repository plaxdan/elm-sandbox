module Main exposing (Model, Msg(..), init, main, update, view)

import Array exposing (Array)
import Browser exposing (Document)
import Browser.Navigation as Navigation exposing (Key)
import Css
import Css.Global
import Html.Styled as Html exposing (Html, button, div, p, text)
import Html.Styled.Attributes as Attr
import Html.Styled.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder)
import Random exposing (Generator)
import Random.Array
import Tailwind.Utilities as Tw
import TestVisualizer as TestVis
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
    , testModel : TestVis.TvModel
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
      --
    | RollDice
    | DiceRolled (Array Int)
      --
    | FetchJson
    | JsonFetched (Result Http.Error (List Question))
      --
    | TestVisualizerMsg TestVis.TvMsg


view : Model -> Document Msg
view model =
    case model.page of
        Just HomePage ->
            viewHomePage model

        Nothing ->
            viewNotFoundPage


viewDice : Int -> Html msg
viewDice int =
    div
        [ Attr.css
            [ Tw.font_mono
            , Tw.px_4
            , Tw.py_2
            , Tw.rounded
            , Tw.bg_black
            , Tw.text_white
            ]
        ]
        [ text (String.fromInt int) ]


viewDiceCard : Array Int -> Html Msg
viewDiceCard dice =
    viewCard
        [ viewCardTitle "Roll Six Dice"
        , viewButton RollDice "Let's Roll!"
        , div
            [ Attr.css
                [ Tw.grid
                , Tw.grid_flow_row
                , Tw.grid_cols_3
                , Tw.grid_rows_2
                , Tw.gap_3
                ]
            ]
            (Array.map viewDice dice |> Array.toList)
        ]


viewButton : Msg -> String -> Html Msg
viewButton action label =
    button
        [ Attr.css
            [ Tw.bg_gray_300
            , Tw.px_6
            , Tw.py_2
            , Tw.rounded_full
            , Css.hover [ Tw.bg_gray_500, Tw.text_white, Tw.rounded_lg ]
            ]
        , onClick action
        ]
        [ text label ]


viewCardTitle : String -> Html Msg
viewCardTitle title =
    div
        [ Attr.css [ Tw.text_xl, Tw.font_medium, Tw.text_black ] ]
        [ p [] [ text title ] ]


viewCard : List (Html Msg) -> Html Msg
viewCard children =
    div
        [ Attr.css
            [ Tw.flex
            , Tw.flex_col
            , Tw.m_3
            , Tw.space_y_6
            , Tw.p_6
            , Tw.mx_auto
            , Tw.bg_gray_100
            , Tw.rounded_xl
            , Tw.shadow_md
            , Tw.space_x_4
            ]
        ]
        children


viewQuestion : Question -> Html Msg
viewQuestion { categoryId, description } =
    div
        [ Attr.css
            [ Tw.flex
            , Tw.flex_col
            , Tw.space_y_6
            , Tw.p_6
            , Tw.max_w_lg
            , Tw.mx_auto
            , Tw.bg_white
            , Tw.rounded_xl
            , Tw.space_x_4
            ]
        ]
        [ p [] [ text categoryId ]
        , p [] [ text description ]
        ]


viewQuestions : List Question -> Html Msg
viewQuestions questions =
    div [ Attr.css [ Tw.flex, Tw.flex_col, Tw.space_y_6 ] ] <|
        List.map viewQuestion questions


viewQuestionsCard : List Question -> Html Msg
viewQuestionsCard questions =
    viewCard
        [ viewCardTitle "Fetch and Decode Question JSON"
        , viewButton FetchJson "Fetch JSON"
        , viewQuestions questions
        ]


viewTestCard : Html Msg
viewTestCard =
    viewCard [ TestVis.view |> Html.map TestVisualizerMsg ]


viewHomePage : { a | diceValues : Array Int, questions : List Question } -> Document Msg
viewHomePage { diceValues, questions } =
    { title = "Elm Starter | Home"
    , body =
        [ Html.toUnstyled <|
            div
                [ Attr.css [ Tw.flex, Tw.flex_wrap ] ]
                [ Css.Global.global Tw.globalStyles
                , viewTestCard
                , viewDiceCard diceValues
                , viewQuestionsCard questions
                ]
        ]
    }


viewNotFoundPage : Document Msg
viewNotFoundPage =
    { title = "Elm Starter | Not Found"
    , body =
        [ Html.toUnstyled <|
            div []
                [ Css.Global.global Tw.globalStyles
                , text "Not Found"
                ]
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

        TestVisualizerMsg tvMsg ->
            toTestVis model (TestVis.update tvMsg model.testModel)


toTestVis : Model -> ( TestVis.TvModel, Cmd TestVis.TvMsg ) -> ( Model, Cmd Msg )
toTestVis model ( tvModel, cmd ) =
    ( { model | testModel = tvModel }
    , Cmd.map TestVisualizerMsg cmd
    )


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
        , testModel = TestVis.initialModel
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
