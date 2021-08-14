module Tarot.Types

open Tarot.Ext

type Suit = Heart | Spades | Diamonds | Clubs
let suits = [Heart; Diamonds; Clubs; Spades]
type Card =
    | Trump of int
    | Suit of int * Suit

    override this.ToString() =
        match this with
        | Trump i -> if i = 0 then "Excuse" else i.ToString()
        | Suit (i,s) ->
            let ii = match i with
                     | 11 -> "Valet"
                     | 12 -> "Cavalier"
                     | 13 -> "Dame"
                     | 14 -> "Roi"
                     | i -> i.ToString()
            in
                sprintf "%s %A" ii s
    static member Excuse = Trump 0

type Player = { Index: int; Name: string; Cards: Card[] }
with
    static member New index name cards = { Index = index; Name = name; Cards = cards }
type Trick = {
    StartingPlayer: int
    PlayedCards: Card list
}
let startTrick startingPlayer = { StartingPlayer = startingPlayer; PlayedCards = [] }
type Playing = {
    Players: Player []
    Taker: int
    Trick: Trick
    AttackerTricks: Card list
    DefenderTricks: Card list
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





type Model = GameState

type Msg =
    | PlayCard of Player * int
    | EndRound
    | GetSize of float * float