module Main exposing (main)

import Array exposing (Array)
import Css
    exposing
        ( Mixin
        , Stylesheet
        , (.)
        , absolute
        , backgroundColor
        , bold
        , center
        , color
        , float
        , fontFamily
        , fontSize
        , fontWeight
        , height
        , left
        , lineHeight
        , marginBottom
        , marginLeft
        , marginRight
        , marginTop
        , mixin
        , paddingBottom
        , paddingLeft
        , paddingRight
        , paddingTop
        , position
        , px
        , rgb
        , right
        , sansSerif
        , textAlign
        , transforms
        , translateX
        , translateY
        , width
        )
import Dict exposing (Dict)
import Html exposing (Attribute, Html, div, h1, h2, program, text)
import Html.Attributes as Attributes
import Keyboard
import List.Extra as List
import Maybe.Extra as Maybe
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Random exposing (Generator)
import Random.Array as Random
import Random.Extra as Random
import Tuple


type alias Model =
    { board : Board
    , score : Score
    }


type alias Board =
    Array (Maybe Value)


type alias Score =
    Int


type alias Tile =
    ( Index, Value )


type alias Index =
    Int


type alias Value =
    Int


type alias KeyCode =
    Int


type Msg
    = AddTiles (List Tile)
    | KeyDown KeyCode


main : Program Never Model Msg
main =
    program
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
    Random.generate AddTiles (newTilesGen 2 emptyBoard)


emptyBoard : Board
emptyBoard =
    Array.repeat 16 Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddTiles newTiles ->
            doAddTiles model newTiles

        KeyDown keyCode ->
            doKeyDown model keyCode


doAddTiles : Model -> List Tile -> ( Model, Cmd Msg )
doAddTiles model newTiles =
    let
        newBoard =
            withTiles model.board newTiles

        newModel =
            { model | board = newBoard }
    in
        ( newModel, Cmd.none )


withTiles : Board -> List Tile -> Board
withTiles board newTiles =
    case newTiles of
        head :: tail ->
            withTiles (Array.set (Tuple.first head) (Just (Tuple.second head)) board) tail

        [] ->
            board


doKeyDown : Model -> KeyCode -> ( Model, Cmd Msg )
doKeyDown model keyCode =
    let
        nextModel =
            if keyCode == 37 then
                slideLeft model
            else if keyCode == 38 then
                slideUp model
            else if keyCode == 39 then
                slideRight model
            else if keyCode == 40 then
                slideDown model
            else
                model

        nextCmd =
            if nextModel /= model then
                addNextOneCmd nextModel.board
            else
                Cmd.none
    in
        ( nextModel, nextCmd )


slideUp : Model -> Model
slideUp =
    slideTransformed (Array.fromList [ 0, 4, 8, 12, 1, 5, 9, 13, 2, 6, 10, 14, 3, 7, 11, 15 ])


slideRight : Model -> Model
slideRight =
    slideTransformed (Array.fromList [ 3, 2, 1, 0, 7, 6, 5, 4, 11, 10, 9, 8, 15, 14, 13, 12 ])


slideDown : Model -> Model
slideDown =
    slideTransformed (Array.fromList [ 15, 11, 7, 3, 14, 10, 6, 2, 13, 9, 5, 1, 12, 8, 4, 0 ])


slideTransformed : Array Index -> Model -> Model
slideTransformed trans model =
    let
        transformedModel =
            slideLeft { model | board = transform trans model.board }
    in
        { transformedModel | board = transform trans transformedModel.board }


slideLeft : Model -> Model
slideLeft model =
    let
        slideRes =
            Array.map slideRowLeft (groupArr 4 model.board)

        newBoard =
            concatArr (Array.map Tuple.first slideRes)

        deltaScore =
            Array.foldl (+) 0 (Array.map Tuple.second slideRes)
    in
        { board = newBoard, score = model.score + deltaScore }


slideRowLeft : Array (Maybe Value) -> ( Array (Maybe Value), Score )
slideRowLeft row =
    let
        ( tmpRow, deltaScore, prevValue ) =
            Array.foldl reduceRow ( Array.empty, 0, Nothing ) row

        newRow =
            resizelRepeat 4 Nothing tmpRow
    in
        ( newRow, deltaScore )


reduceRow : Maybe Value -> ( Array (Maybe Value), Score, Maybe Value ) -> ( Array (Maybe Value), Score, Maybe Value )
reduceRow value ( row, deltaScore, prevValue ) =
    case value of
        Just v ->
            if value == prevValue then
                ( Array.set ((Array.length row) - 1) (Just (2 * v)) row, deltaScore + 2 * v, Nothing )
            else
                ( Array.push value row, deltaScore, value )

        Nothing ->
            ( row, deltaScore, prevValue )


groupArr : Int -> Array a -> Array (Array a)
groupArr groupSize arr =
    let
        numGroups =
            ((Array.length arr) + (groupSize - 1)) // groupSize

        group groupNo =
            Array.slice (groupNo * groupSize) ((groupNo + 1) * groupSize) arr
    in
        Array.map group (Array.fromList (List.range 0 (numGroups - 1)))


concatArr : Array (Array a) -> Array a
concatArr =
    Array.foldr Array.append Array.empty


transform : Array Index -> Board -> Board
transform trans board =
    Array.indexedMap (getTransformedValue trans board) board


getTransformedValue : Array Index -> Board -> Index -> Maybe Value -> Maybe Value
getTransformedValue trans board index _ =
    Maybe.join
        (case Array.get index trans of
            Just newIndex ->
                Array.get newIndex board

            Nothing ->
                Nothing
        )


addNextOneCmd : Board -> Cmd Msg
addNextOneCmd board =
    Random.generate AddTiles (newTilesGen 1 board)


newTilesGen : Int -> Board -> Generator (List Tile)
newTilesGen numTiles board =
    let
        indexes =
            emptyIndexes board

        indexesGen =
            chooseNGen numTiles indexes

        valuesGen =
            Random.list (List.length indexes) valueGen
    in
        Random.map2 List.zip indexesGen valuesGen


emptyIndexes : Board -> List Index
emptyIndexes board =
    List.map Tuple.first (List.filter (((==) Nothing) << Tuple.second) (Array.toIndexedList board))


chooseNGen : Int -> List a -> Generator (List a)
chooseNGen num list =
    Random.map Tuple.first (chooseNGen_ num (Array.fromList list))


chooseNGen_ : Int -> Array a -> Generator ( List a, Array a )
chooseNGen_ num arr =
    if num > 0 then
        Random.andThen (chooseNGen__ (num - 1)) (Random.choose arr)
    else
        Random.constant ( [], arr )


chooseNGen__ : Int -> ( Maybe a, Array a ) -> Generator ( List a, Array a )
chooseNGen__ num headRes =
    let
        -- Create generator for the rest of the elements
        nextGen =
            chooseNGen_ num (Tuple.second headRes)
    in
        case Tuple.first headRes of
            Just a ->
                -- Cons the generated value with the rest
                Random.map (Tuple.mapFirst ((::) a)) nextGen

            Nothing ->
                nextGen


valueGen : Random.Generator Value
valueGen =
    Random.frequency [ ( 9.0, Random.constant 2 ), ( 1.0, Random.constant 4 ) ]


subscriptions : Model -> Sub Msg
subscriptions =
    \_ -> Keyboard.downs KeyDown


view : Model -> Html Msg
view model =
    div
        [ styles
            [ marginLeft (px 120)
            , width (px 460)
            ]
        ]
        [ headingView model.score
        , gameContainerView model.board
        ]


styles : List Mixin -> Attribute a
styles =
    Attributes.style << Css.asPairs


headingView : Score -> Html Msg
headingView score =
    div []
        [ titleView
        , scoreView score
        ]


titleView : Html Msg
titleView =
    h1
        [ styles
            [ fontFamily sansSerif
            , float left
            , marginTop (px 0)
            , marginBottom (px 0)
            ]
        ]
        [ text "2048" ]


scoreView : Score -> Html Msg
scoreView score =
    h2
        [ styles
            [ fontFamily sansSerif
            , float right
            , marginTop (px 9)
            , marginBottom (px 0)
            ]
        ]
        [ text ("Score: " ++ (toString score)) ]


gameContainerView : Board -> Html Msg
gameContainerView board =
    div
        [ styles
            [ float left
            , backgroundColor (rgb 187 173 160)
            , height (px 436)
            , width (px 436)
            , paddingBottom (px 12)
            , paddingLeft (px 12)
            , paddingRight (px 12)
            , paddingTop (px 12)
            ]
        ]
        [ gridContainerView
        , tileContainerView board
        ]


gridContainerView : Html Msg
gridContainerView =
    div [ styles [ position absolute ] ] (List.repeat 4 gridRowView)


gridRowView : Html Msg
gridRowView =
    div
        [ styles
            [ height (px 100)
            , marginBottom (px 12)
            ]
        ]
        (List.repeat 4 gridCellView)


gridCellView : Html Msg
gridCellView =
    div
        [ styles
            [ float left
            , backgroundColor (rgb 204 192 180)
            , height (px 100)
            , width (px 100)
            , marginRight (px 12)
            ]
        ]
        []


tileContainerView : Board -> Html Msg
tileContainerView board =
    div [] (List.map tileView (tiles board))


tileView : Tile -> Html Msg
tileView tile =
    div
        [ styles
            [ height (px 100)
            , width (px 100)
            , position absolute
            , tileTransform (Tuple.first tile)
            ]
        ]
        [ tileValueView (Tuple.second tile) ]


tileTransform : Int -> Mixin
tileTransform index =
    let
        col =
            index % 4

        row =
            index // 4
    in
        transforms
            [ translateX (px (toFloat (112 * col)))
            , translateY (px (toFloat (112 * row)))
            ]


tileValueView : Value -> Html Msg
tileValueView value =
    div
        [ styles
            [ fontFamily sansSerif
            , fontWeight bold
            , textAlign center
            , lineHeight (px 100)
            , tileMixin value
            ]
        ]
        [ text (toString value) ]


tileMixin : Value -> Mixin
tileMixin value =
    Maybe.withDefault tileSuperMixin (Dict.get value tileMixins)


tileSuperMixin : Mixin
tileSuperMixin =
    mixin
        [ backgroundColor (rgb 60 58 50)
        , color (rgb 249 246 242)
        , fontSize (px 30)
        ]


tileMixins : Dict Value Mixin
tileMixins =
    Dict.fromList
        [ ( 2
          , mixin
                [ backgroundColor (rgb 238 228 218)
                , color (rgb 119 110 101)
                , fontSize (px 55)
                ]
          )
        , ( 4
          , mixin
                [ backgroundColor (rgb 237 224 200)
                , color (rgb 119 110 101)
                , fontSize (px 55)
                ]
          )
        , ( 8
          , mixin
                [ backgroundColor (rgb 242 177 121)
                , color (rgb 249 246 242)
                , fontSize (px 55)
                ]
          )
        , ( 16
          , mixin
                [ backgroundColor (rgb 245 149 99)
                , color (rgb 249 246 242)
                , fontSize (px 55)
                ]
          )
        , ( 32
          , mixin
                [ backgroundColor (rgb 246 124 95)
                , color (rgb 249 246 242)
                , fontSize (px 55)
                ]
          )
        , ( 64
          , mixin
                [ backgroundColor (rgb 246 94 59)
                , color (rgb 249 246 242)
                , fontSize (px 55)
                ]
          )
        , ( 128
          , mixin
                [ backgroundColor (rgb 237 207 114)
                , color (rgb 249 246 242)
                , fontSize (px 45)
                ]
          )
        , ( 256
          , mixin
                [ backgroundColor (rgb 237 204 97)
                , color (rgb 249 246 242)
                , fontSize (px 45)
                ]
          )
        , ( 512
          , mixin
                [ backgroundColor (rgb 237 200 80)
                , color (rgb 249 246 242)
                , fontSize (px 45)
                ]
          )
        , ( 1024
          , mixin
                [ backgroundColor (rgb 237 197 63)
                , color (rgb 249 246 242)
                , fontSize (px 35)
                ]
          )
        , ( 2048
          , mixin
                [ backgroundColor (rgb 237 194 48)
                , color (rgb 249 246 242)
                , fontSize (px 35)
                ]
          )
        ]


tiles : Board -> List Tile
tiles board =
    List.filterMap sequenceSnd (Array.toIndexedList board)


sequenceSnd : ( a, Maybe b ) -> Maybe ( a, b )
sequenceSnd tup =
    case Tuple.second tup of
        Just b ->
            Just ( Tuple.first tup, b )

        Nothing ->
            Nothing


resizelRepeat : Int -> a -> Array a -> Array a
resizelRepeat n val xs =
    let
        l =
            Array.length xs
    in
        if l > n then
            Array.slice 0 n xs
        else if l < n then
            Array.append xs (Array.repeat (n - l) val)
        else
            xs
