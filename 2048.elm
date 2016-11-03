module Main exposing (main)

import Array exposing (Array)
import Html exposing (Html)
import Html.App as App
import List.Extra as List
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Random exposing (Generator)
import Random.Extra as Random


type alias Model =
    { board : Board
    , score : Score
    }


type alias Board =
    Array (Maybe Value)


type alias Score =
    Int


type Msg
    = AddPieces (List NewPiece)


type alias NewPiece =
    ( Index, Value )


type alias Index =
    Int


type alias Value =
    Int


main : Program Never
main =
    App.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : ( Model, Cmd Msg )
init =
    ( initModel, addInitialTwoCmd )


initModel : Model
initModel =
    { board = emptyBoard
    , score = 0
    }


addInitialTwoCmd : Cmd Msg
addInitialTwoCmd =
    Random.generate AddPieces (newPiecesGen 2 emptyBoard)


newPiecesGen : Int -> Board -> Generator (List NewPiece)
newPiecesGen numPieces board =
    let
        indexes =
            emptyIndexes board

        indexesGen =
            chooseN numPieces indexes

        valuesGen =
            Random.list (List.length indexes) valueGen
    in
        Random.map2 List.zip indexesGen valuesGen


chooseN : Int -> List a -> Generator (List a)
chooseN num list =
    Random.map Array.toList (arrChooseN num (Array.fromList list))


arrChooseN : Int -> Array a -> Generator (Array a)
arrChooseN num arr =
    if num > 0 then
        Debug.crash ""
    else
        Random.constant Array.empty


valueGen : Random.Generator Value
valueGen =
    Random.frequency [ ( 9.0, Random.constant 2 ), ( 1.0, Random.constant 4 ) ]


emptyIndexes : Board -> List Index
emptyIndexes board =
    Debug.crash ""


emptyBoard : Board
emptyBoard =
    Array.repeat 16 Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddPieces newPieces ->
            doAddPieces model newPieces


doAddPieces : Model -> List NewPiece -> ( Model, Cmd Msg )
doAddPieces model newPieces =
    let
        newBoard =
            withPieces model.board newPieces

        newModel =
            withBoard model newBoard
    in
        ( newModel, Cmd.none )


withBoard : Model -> Board -> Model
withBoard model newBoard =
    { model | board = newBoard }


withPieces : Board -> List NewPiece -> Board
withPieces board newPieces =
    Debug.crash ""


subscriptions : Model -> Sub Msg
subscriptions model =
    Debug.crash ""


view : Model -> Html Msg
view model =
    Debug.crash ""
