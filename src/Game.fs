module Tarot.Game

open Tarot.Types
open Tarot.Ext

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

let playCard (state:Playing) pi i =
    let card = state.Players.[pi].Cards.[i]
    let p = { state.Players.[pi] with
                Cards = state.Players.[pi].Cards |>  Seq.removeIndex i |> Seq.toArray
            }
    { state with Players = state.Players
                           |> Array.map (fun pp -> if pp.Index <> p.Index then pp else p )
                 Trick = { state.Trick with PlayedCards = card :: state.Trick.PlayedCards }
    }, card

let isTrump c = match c  with
                | Trump 0 -> false // excuse
                | Trump _ -> true
                | _ -> false
let isSuit c = match c  with Suit(_,_) -> true | _ -> false
let isSuitOf s c = match c  with Suit(_,cs) -> s = cs | _ -> false
let isTrumpGreaterThan value c = match c with Trump t -> t > value | _ -> false
let cardCanBePlayed (state:Playing) pi ci =
    let p = state.Players.[pi]
    let card = p.Cards.[ci]
    match state.Trick.PlayedCards with
    | [] -> true
    | [e] when e = Card.Excuse -> true
    | l ->
        let first =
            let reved = l |> List.rev
            if List.head reved = Card.Excuse then reved |> List.skip 1 |> List.head else List.head reved
        let max = Seq.map (fun c -> match c with Trump t -> t | _ -> -1) l |> Seq.max
        match first with
        | Trump _ ->
                     match card with
                     | Trump played -> played = 0 (*excuse*) || played > max || p.Cards |> Seq.forall (not << isTrumpGreaterThan max)
                     | Suit _ ->  p.Cards |> Seq.forall (not << isTrump)
        | Suit (_,s) -> match card with
                        | Trump t -> t = 0 || p.Cards |> (not << Seq.exists (isSuitOf s)) && (t > max || p.Cards |> Seq.forall (not << isTrumpGreaterThan max))
                        | Suit (_, ps) -> ps = s || p.Cards |> (not << Seq.exists (isSuitOf s))
type PlayingState =
    | WaitForCard of int
    | EndRound
    | EndGame
let getPlayingState (s:Playing): PlayingState =
    match s.Trick with
    | _ when s.Trick.PlayedCards.Length = s.PlayerCount ->
        if s.Players.[0].Cards.Length = 0
        then EndGame
        else EndRound
    | _ -> let nextPlayer = s.Trick.StartingPlayer + s.Trick.PlayedCards.Length
           let nextPlayer = nextPlayer % s.Players.Length
           WaitForCard nextPlayer
