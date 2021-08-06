module Tarot.Game

open Tarot.Types
open Tarot.Ext

type Player = { Index: int; Name: string; Cards: Card[] }

type Trick = {
    StartingPlayer: int
    PlayedCards: Card list
}
type Playing = {
    Players: Player []
    Taker: int
    Trick: Trick
//    AttackerTricks: Card list
//    DefenderTricks: Card list
}
with
    member this.PlayerCount = this.Players.Length
    member this.TakerPlayer = this.Players.[this.Taker]

type GameState =
    | Start
    | Dealing
    | Betting of int
    | Playing of Playing

type Game = {
    PlayerCount: int
    State: GameState
}

let playCard (state:Playing) pi i =
    let p = { state.Players.[pi] with Cards =
                        state.Players.[pi].Cards |>  Seq.removeIndex i |> Seq.toArray
            }
    { state with Players = state.Players
                           |> Array.map (fun pp -> if pp.Index <> p.Index then pp else p )
    }
type PlayingState =
    | WaitForCard of int
    | EndRound
    | EndGame
let getPlayingState (s:Playing) =
    match s.Trick with
    | _ when s.Trick.PlayedCards.Length = s.PlayerCount ->
        if s.Players.[0].Cards.Length = 0
        then EndGame
        else EndRound
    | _ -> let nextPlayer = s.Trick.StartingPlayer + s.Trick.PlayedCards.Length
           let nextPlayer = nextPlayer % s.Players.Length
           WaitForCard nextPlayer
