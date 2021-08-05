module Tarot.Game

open Tarot.Types
open Tarot.Ext

type Player = { index: int; name: string; cards: Card[] }

type Playing = { Players: Player []; Taker: int }
with
    member this.TakerPlayer = this.Players.[this.Taker]

type GameState =
    | Start
    | Dealing
    | Betting of int
    | Playing of Playing

type Game = {
    count: int
    state: GameState
}

let playCard (state:Playing) pi i =
    let p = { state.Players.[pi] with cards =
                        state.Players.[pi].cards |>  Seq.removeIndex i |> Seq.toArray
            }
    { state with Players = state.Players
                           |> Array.map (fun pp -> if pp.index <> p.index then pp else p )
    }