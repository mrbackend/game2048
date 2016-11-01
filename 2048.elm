module Main exposing (main)

import Array exposing (Array)
import Html exposing (Html)
import Html.App as App
import List.Extra as XList
import Maybe exposing (Maybe)
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Random exposing (Generator, generate, int, list, map2, pair)
import Random.Extra as XRandom exposing (constant, frequency)


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



-- TODO


newPiecesGen : Int -> Board -> Generator (List NewPiece)
newPiecesGen numPieces board =
    let
        indexesGen : Generator (List Index)
        indexesGen =
            listChooseNGen numPieces (emptyIndexes board)

        valuesGen : Generator (List Value)
        valuesGen =
            Random.list numPieces valueGen
    in
        Random.map2 XList.zip indexesGen valuesGen



-- TODO


listChooseNGen : Int -> List a -> Generator (List a)
listChooseNGen numPieces values =
    Random.map Array.toList (arrChooseNGen numPieces (Array.fromList values))


arrChooseNGen : Int -> Array a -> Generator (Array a)
arrChooseNGen numPieces values =
    Debug.crash ""



-- TODO


valueGen : Random.Generator Value
valueGen =
    XRandom.frequency [ ( 9.0, XRandom.constant 2 ), ( 1.0, XRandom.constant 4 ) ]



-- TODO


emptyIndexes : Board -> List Index
emptyIndexes board =
    []


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



-- TODO


withPieces : Board -> List NewPiece -> Board
withPieces board newPieces =
    board



-- TODO


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- TODO


view : Model -> Html Msg
view model =
    Html.div [] []
