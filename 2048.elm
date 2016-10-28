module Main exposing (..)

import Array exposing (Array)
import Html exposing (Html)
import Html.App as App
import Maybe exposing (Maybe)
import Platform.Cmd as Cmd
import Platform.Sub as Sub


type alias Board =
    Array (Maybe Int)


type alias Model =
    { board : Board
    , score : Int
    }


type Msg
    = AddCells (List ( Int, Int ))


emptyBoard : Board
emptyBoard =
    Array.repeat 16 Nothing


initModel : Model
initModel =
    { board = emptyBoard
    , score = 0
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, addInitialTwoCmd )


addInitialTwoCmd : Cmd Msg
addInitialTwoCmd =
    Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddCells newCells ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    Html.div [] []


main : Program Never
main =
    App.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
