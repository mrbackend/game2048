module Main exposing (..)

import Array exposing (Array)
import Html exposing (Html)
import Html.App as App
import Maybe exposing (Maybe)
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Return exposing (Return, return)


type alias Board =
    Array (Maybe Int)


type alias Model =
    { board : Board
    , score : Int
    }


type alias Msg =
    Int


emptyBoard : Board
emptyBoard =
    Array.repeat 16 Nothing


initModel : Model
initModel =
    { board = emptyBoard
    , score = 0
    }


init : Return Msg Model
init =
    return initModel Cmd.none


addInitialTwoCmd : Cmd Msg
addInitialTwoCmd =
    Random.generate


update : Msg -> Model -> Return Msg Model
update msg model =
    return model addInitialTwoCmd


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
