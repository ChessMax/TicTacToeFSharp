namespace Lib

open UnityEngine
open UnityEngine.UI
open Lib.TicTacToeDomain
open TMPro
open UnityEngine.Events

type ActionType =
    | Nop
    | Action of Action

[<AllowNullLiteral>]
type TicTacToeBehaviour() =
    inherit MonoBehaviour()

    [<DefaultValue>]
    [<SerializeField>]
    val mutable private field : FieldComponent

    [<DefaultValue>]
    [<SerializeField>]
    val mutable private restartButton : Button

    [<DefaultValue>]
    [<SerializeField>]
    val mutable private stateLabel : TextMeshProUGUI

    let startEvent = Event<_>()
    let updateEvent = Event<_>()

    member this.DoMoveAt pos =
        updateEvent.Trigger(Action (Move pos))

    member private this.Start() = startEvent.Trigger()
    member private this.Update() = updateEvent.Trigger(Nop)
    member private this.StartAsync() = Async.AwaitEvent startEvent.Publish
    member private this.UpdateAsync() = Async.AwaitEvent updateEvent.Publish

    member private this.Awake() =
        let gameflow = async {
            do! this.StartAsync()

            this.restartButton.onClick.AddListener(UnityAction(this.OnRestartButtonClicked))

            let initialGame = init()

            let rec gameloop currentGame = async {
                let! updateType = this.UpdateAsync()

                // update game state
                let game =
                    match updateType with
                    | Nop -> currentGame
                    | Action action -> update currentGame action

                // update view
                this.UpdateView game

                return! gameloop game
            }
            return! gameloop initialGame
        }
        gameflow |> Async.StartImmediate |> ignore

    member private this.UpdateView game =
        // update cells
        Array.iteri (fun index cell ->
            this.field.UpdateCell (indexToPosition index) cell game.State
            ) game.Board

        // update game status
        match game.State with
        | Playing -> this.stateLabel.text <- sprintf "%s's turn" (game.Current.ToString())
        | GameOver result ->
            match result with
            | Win player -> this.stateLabel.text <- sprintf "%s wins!" (player.ToString())
            | Draw -> this.stateLabel.text <- "It's a draw!"

    member private this.OnRestartButtonClicked() =
        updateEvent.Trigger (Action Restart)

and FieldComponent() =
    inherit MonoBehaviour()

    [<DefaultValue>]
    [<SerializeField>]
    val mutable private xSprite : Sprite

    [<DefaultValue>]
    [<SerializeField>]
    val mutable private oSprite : Sprite

    [<DefaultValue>]
    [<SerializeField>]
    val mutable private cells: CellComponent[]

    [<DefaultValue>]
    [<SerializeField>]
    val mutable private script: TicTacToeBehaviour

    member this.UpdateCell (px, py) cell gameState =
        let cellSprite =
            match cell with
            | Empty -> null
            | Occupied player ->
                if player = X then this.xSprite else this.oSprite
        this.cells.[positionToIndex (px, py)].UpdateCell cellSprite gameState

    member this.OnMouseDown cell =
        let index = this.cells |> Array.findIndex ((=) cell)
        let pos = indexToPosition index
        this.script.DoMoveAt(pos)

and [<RequireComponent(typedefof<Collider2D>)>]
    [<RequireComponent(typedefof<SpriteRenderer>)>]
    CellComponent() =
    inherit MonoBehaviour()

    [<DefaultValue>]
    [<SerializeField>]
    val mutable private field : FieldComponent

    [<DefaultValue>]
    [<SerializeField>]
    val mutable private collider : Collider2D

    [<DefaultValue>]
    [<SerializeField>]
    val mutable private renderer : SpriteRenderer

    member this.UpdateCell cellSprite gameState =
        this.renderer.sprite <- cellSprite
        this.collider.enabled <- isNull cellSprite && gameState = Playing

    member this.OnMouseDown() =
        this.field.OnMouseDown(this)
