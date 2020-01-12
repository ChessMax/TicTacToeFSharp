module Lib.TicTacToeDomain

type Position = int * int

type Action =
    | Move of Position
    | Restart

type Player = X | O

type GameResult =
    | Win of Player
    | Draw

type Cell =
    | Empty
    | Occupied of Player

type GameState =
    | Playing
    | GameOver of GameResult

type Game = {
    State : GameState;
    Current : Player;
    Board: Cell[]
}

let fieldWidth = 3
let fieldHeight = 3

let offsets = seq [
    (-1, 0)
    (-1, -1)
    ( 0, -1)
    (-1,  1)
]

let indexToPosition index =
    let cx = index % fieldWidth
    let cy = index / fieldWidth
    (cx, cy)

let positionToIndex (cx, cy) =
    cy * fieldWidth + cx

let init() =
    {
        State = Playing;
        Current = X;
        Board = Array.create (fieldWidth * fieldHeight) Empty
    }

let isWinner currentPlayer (cells: Cell[]) (playerX, playerY) =
    let rec search px py offsetX offsetY =
        if px >= 0 && px < fieldWidth &&
           py >= 0 && py < fieldHeight then
               let pos = (px, py)

               seq {
                   match cells.[positionToIndex pos] with
                   | Occupied player when player = currentPlayer ->
                       yield pos
                       yield! (search (px + offsetX) (py + offsetY) offsetX offsetY)
                   | _ -> ()
               }
        else
            Seq.empty

    offsets
    |> Seq.map (fun (offsetX, offsetY) -> seq {
        yield! search playerX playerY offsetX offsetY
        yield! (search (playerX - offsetX) (playerY - offsetY) -offsetX -offsetY)})
    |> Seq.exists ((Seq.tryItem 2) >> Option.isSome)

let isDraw (cells: Cell[]) =
    cells
    |> Array.exists (function | Empty -> true | _ -> false)
    |> (=) false

let update game action =
    match action with
    | Restart ->
        init()
    | Move (playerX, playerY) ->
        let board = Array.mapi (fun index cell ->
            let (cx, cy) = indexToPosition index
            if cx = playerX && cy = playerY then
                Occupied(game.Current) else cell) game.Board

        let currentPlayer = game.Current
        match isWinner currentPlayer board (playerX, playerY) with
        | true -> {game with Board = board; State = GameOver (Win currentPlayer)}
        | _ -> match isDraw board with
               | true -> {game with Board = board; State = GameOver (Draw)}
               | _ -> {game with Board = board; Current = if currentPlayer = X then O else X}


