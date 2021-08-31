module TestVisualizer exposing (TvModel, TvMsg, initialModel, update, view)

import Html.Styled as Html exposing (Html, button, div, h3, hr, text)
import Html.Styled.Events exposing (onClick)


type alias TvModel =
    { started : Bool }


type TvMsg
    = StartTests


update : TvMsg -> TvModel -> ( TvModel, Cmd msg )
update msg model =
    case msg of
        StartTests ->
            ( { model | started = True }, Cmd.none )


view : Html TvMsg
view =
    div []
        [ h3 [] [ text "Test Visualizer" ]
        , hr [] []
        , button [ onClick StartTests ] [ text "Start Tests" ]
        ]


initialModel : TvModel
initialModel =
    { started = False }
