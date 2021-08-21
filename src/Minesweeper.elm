module Minesweeper exposing (main)

import Browser
import Json.Decode as Json
import Html exposing (Html)
import Browser.Events as Events
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Dict exposing (Dict)
import Random
import Random.Set as Random
import Set exposing (Set)



-- GameStatus -> Playing , Lost, Won
type GameStatus
    = Run
    | Lost
    | Win

-- GameState -> Loading , Loaded
type GameState
    = Loading
    | Loaded { state : GameStatus, board : Board }

type alias DataModel =
    { game : GameState
    , controlPressed : Bool
    }


-- boardsize -> 9x9
boardSize : Int
boardSize = 9

-- flag cell
type alias Flags =
    ()

-- cell position
type alias Position =
    ( Int, Int )

-- cell type
type alias Cells cache =
    Dict Position (Cell cache)

-- board type
type alias Board =
    { size : Int
    , mines : Int
    , cells : Cells Cache
    }

-- state of cell (open, flagged close)
type CellState
    = Close { pushed : Bool }
    | Flagged
    | Open


type alias Cell cache =
    { cache
        | state : CellState
        , mined : Bool
    }


type alias Cache =
    { neighbouringMines : Int
    }

-- event of app
type UpdateEvent
    = New
    | Init { size : Int, mined : Set Position }
    | ToggleFlag Position
    | OpenCell Position
    | SetControlPressed Bool
    | SetPushed { position : Position, pushed : Bool }
    | NothingCmd

-- main
main : Program Flags DataModel UpdateEvent
main =
    Browser.document { init = init, view = view, update = update, subscriptions = attachEvent }

-- init game board
initBoard : Cmd UpdateEvent
initBoard =
    let
        toMsg =
            \mined -> Init { size = boardSize, mined = mined }
    in
    Random.generate toMsg (mineGenerator boardSize boardSize)

-- init game
init : Flags -> ( DataModel, Cmd UpdateEvent )
init _ =
    ( { game = Loading, controlPressed = False }, initBoard )


-- view whole page
view : DataModel -> { title : String, body : List (Html UpdateEvent) }
view model =
    { title = "*** Minesweeper ***"
    , body = [ viewGame model ]
    }

-- update model
update : UpdateEvent -> DataModel -> ( DataModel, Cmd UpdateEvent )
update msg model =
    case msg of
        New ->
            ( { model | game = Loading }, initBoard )

        Init { size, mined } ->
            let
                axis =
                    List.range 0 (size - 1)

                positions =
                    axis |> List.concatMap (\x -> axis |> List.map (\y -> ( x, y )))

                cells =
                    positions |> List.map (generateCell mined) |> Dict.fromList
            in
            ( { model | game = Loaded { state = Run, board = Board size (Set.size mined) (computeCaches cells) } }
            , Cmd.none
            )

        ToggleFlag pos ->
            changeBoard model (atPosition pos toggleFlag)

        OpenCell pos ->
            let
                op =
                    if model.controlPressed then
                        \b -> applyToSurrounding b pos reveal

                    else
                        \b -> Dict.get pos b.cells |> Maybe.map (reveal b pos) |> Maybe.withDefault b
            in
            changeBoard model op

        SetControlPressed controlPressed ->
            ( { model | controlPressed = controlPressed }, Cmd.none )

        SetPushed { position, pushed } ->
            let
                op =
                    if model.controlPressed then
                        \b -> applyToSurrounding b position (\board -> \p -> \_ -> atPosition p (trySetPushed pushed) board)

                    else
                        atPosition position (trySetPushed pushed)
            in
            changeBoard model op

        NothingCmd ->
            ( model, Cmd.none )


-- set mouse event
attachEvent : DataModel -> Sub UpdateEvent
attachEvent _ =
    Sub.batch
        [ Events.onKeyUp (decodeKeyPress False)
        , Events.onKeyDown (decodeKeyPress True)
        ]


-- render game
viewGame : DataModel -> Html UpdateEvent
viewGame model =
    case model.game of
        Loading ->
            Html.text "Mining…"

        Loaded { state, board } ->
            let
                gameOver =
                    case state of
                        Run ->
                            Nothing

                        Lost ->
                            Just "Hit Mine !!!"

                        Win ->
                            Just "((*** Congratulation, You Won ***))"
            in
            Html.div [ HtmlA.id "game" ]
                [
                  Html.div [ HtmlA.id "game-title" ] [ Html.text "*** Minesweeper version 1.0.0 *** " ],
                  viewBoard (gameOver /= Nothing) board
                , gameOver |> Maybe.map viewGameOver |> Maybe.withDefault (Html.text "")
                ]



-- show gameover dialog
viewGameOver : String -> Html UpdateEvent
viewGameOver message =
    Html.div [ HtmlA.id "game-over" ]
        [ Html.div [ HtmlA.class "dialog" ]
            [ Html.p [] [ Html.text message ]
            , Html.button [ HtmlE.onClick New ] [ Html.text "New Game" ]
            ]
        ]


-- show board
viewBoard : Bool -> Board -> Html UpdateEvent
viewBoard gameOver board =
    Html.div [ HtmlA.id "board" ]
        [ Html.div [ HtmlA.class "aspect" ]
            [ board.cells
                |> Dict.toList
                |> List.map (viewCell gameOver)
                |> Html.div [ HtmlA.class "cells" ]
            ]
        ]


-- show cell in board
viewCell : Bool -> ( Position, Cell Cache ) -> Html UpdateEvent
viewCell gameOver ( position, { state, mined, neighbouringMines } ) =
    let
        ( x, y ) =
            position

        minesCountClassName =
            if state == Open then
                neighbouringMines |> minesCountClass

            else
                ""

        interaction =
            if gameOver then
                []

            else
                [ onRightClick (ToggleFlag position)
                , HtmlE.onClick (OpenCell position)
                , HtmlE.onMouseDown (SetPushed { position = position, pushed = True })
                , HtmlE.onMouseLeave (SetPushed { position = position, pushed = False })
                ]
    in
    Html.button
        (List.concat
            [ interaction
            , [ HtmlA.classList
                    [ ( "cell", True )
                    , ( "flagged", state == Flagged )
                    , ( "revealed", state == Open )
                    , ( "mined", (state == Open || gameOver) && mined )
                    , ( "wrong", gameOver && not mined && state == Flagged )
                    , ( "pushed", state == Close { pushed = True } )
                    , ( minesCountClassName, state == Open && not mined )
                    ]
              , HtmlA.style "grid-area" (String.fromInt (y + 1) ++ " / " ++ String.fromInt (x + 1) ++ " / auto / auto")
              ]
            ]
        )
        []


-- show cell when open -> 1,2,3,4....9
minesCountClass : Int -> String
minesCountClass count =
    "m" ++ String.fromInt count


-- try push cell
trySetPushed : Bool -> Cell a -> Cell a
trySetPushed pushed cell =
    case cell.state of
        Close _ ->
            { cell | state = Close { pushed = pushed } }

        _ ->
            cell


-- check for control key pressed
decodeKeyPress : Bool -> Json.Decoder UpdateEvent
decodeKeyPress changeTo =
    Json.field "key" Json.string
        |> Json.map
            (\k ->
                if k == "Control" then
                    SetControlPressed changeTo

                else
                    NothingCmd
            )

-- change board
changeBoard : DataModel -> (Board -> Board) -> ( DataModel, Cmd msg )
changeBoard model change =
    case model.game of
        Loaded { board } ->
            let
                changed =
                    change board
            in
            ( { model | game = Loaded { state = changed |> boardState, board = changed } }, Cmd.none )

        _ ->
            ( model, Cmd.none )


-- boardstate
boardState : Board -> GameStatus
boardState board =
    let
        values =
            board.cells |> Dict.values
    in
    if values |> List.any (\c -> c.mined && c.state == Open) then
        Lost

    else if values |> List.all (\c -> c.state == Open && not c.mined || c.state == Flagged && c.mined) then
        Win

    else
        Run


-- mine count
minesCount : Cells a -> List Position -> Int
minesCount cells positions =
    positions
        |> List.filterMap (\pos -> Dict.get pos cells)
        |> List.map minesInCell
        |> List.sum


-- check is cell contains mine
minesInCell : Cell a -> Int
minesInCell cell =
    if cell.mined then
        1

    else
        0

-- cell neibors
surrounding : Position -> List Position
surrounding ( x, y ) =
    [ ( -1, -1 ), ( -1, 0 ), ( -1, 1 ), ( 0, -1 ), ( 0, 1 ), ( 1, -1 ), ( 1, 0 ), ( 1, 1 ) ]
        |> List.map (\( x2, y2 ) -> ( x + x2, y + y2 ))


computeCaches : Cells {} -> Cells Cache
computeCaches cells =
    cells |> Dict.map (computeCache cells)


computeCache : Cells {} -> Position -> Cell {} -> Cell Cache
computeCache cells position { state, mined } =
    { state = state, mined = mined, neighbouringMines = position |> surrounding |> minesCount cells }


atPosition : Position -> (Cell Cache -> Cell Cache) -> Board -> Board
atPosition position change board =
    { board | cells = board.cells |> Dict.update position (Maybe.map change) }

-- open cell
reveal : Board -> Position -> Cell Cache -> Board
reveal board position cell =
    let
        ( newCell, cellChanged ) =
            tryReveal cell
    in
    if cellChanged then
        let
            changedBoard =
                { board | cells = Dict.insert position newCell board.cells }
        in
        if newCell.mined then
            changedBoard

        else if cell.neighbouringMines == 0 then
            applyToSurrounding changedBoard position reveal

        else
            changedBoard

    else
        board



-- try open cell
tryReveal : Cell a -> ( Cell a, Bool )
tryReveal cell =
    let
        state =
            case cell.state of
                Close _ ->
                    Open

                Flagged ->
                    Flagged

                Open ->
                    Open
    in
    ( { cell | state = state }, cell.state /= state )


-- toggle flag on cell
toggleFlag : Cell a -> Cell a
toggleFlag cell =
    let
        state =
            case cell.state of
                Close _ ->
                    Flagged

                Flagged ->
                    Close { pushed = False }

                Open ->
                    Open
    in
    { cell | state = state }



-- generate position for cell of board
positionGenerator : Int -> Random.Generator Position
positionGenerator size =
    Random.map2 (\x -> \y -> ( x, y ))
        (Random.int 0 (size - 1))
        (Random.int 0 (size - 1))

-- mouse right click
onRightClick : msg -> Html.Attribute msg
onRightClick msg =
    { message = msg
    , stopPropagation = True
    , preventDefault = True
    }
        |> Json.succeed
        |> HtmlE.custom "contextmenu"

-- apply to all surounding cells
applyToSurrounding : Board -> Position -> (Board -> Position -> Cell Cache -> Board) -> Board
applyToSurrounding board position f =
    surrounding position
        |> List.filterMap (\pos -> Dict.get pos board.cells |> Maybe.map (\c -> ( pos, c )))
        |> List.foldl (\( pos, c ) -> \b -> f b pos c) board

-- generate cell
generateCell : Set Position -> Position -> ( Position, Cell {} )
generateCell mined position =
    ( position
    , { state = Close { pushed = False }
      , mined = mined |> Set.member position
      }
    )

-- generate mine for cell
mineGenerator : Int -> Int -> Random.Generator (Set Position)
mineGenerator size mines =
    positionGenerator size |> Random.set mines
