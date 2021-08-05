module Tarot.Game

open Tarot.Types
open Tarot.Ext

type Player = { index: int; name: string; cards: Card[] }

type Playing = { players: Player []; taker: Player }

type GameState =
    | Start
    | Dealing
    | Betting of int
    | Playing of Playing

type Game = {
    count: int
    state: GameState
}

let playCard (state:Playing) p i =
    let p = { p with cards =
                        p.cards |>  Seq.removeIndex i |> Seq.toArray
            }
    { state with players = state.players
                           |> Array.map (fun pp -> if pp.index <> p.index then pp else p )
    }