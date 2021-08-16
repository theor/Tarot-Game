module Tarot.Game

open Tarot.Types
open Tarot.Ext

let isValid (c:Card): bool =
    match c with
    | Trump i -> i >= 0 && i <= 21
    | Suit (i,s) -> i >= 1 && i <= 14

let game: seq<Card> =
    seq {
        for s in suits do
            for i in 1..14 do
                yield Suit(i, s)
        for i in 0..21 do
            yield Trump i
    }

let pointsOf (c:Card): float32 =
    match c with
    | Trump i when i = 0 || i = 1 || i = 21 -> 4.5f
    | Trump _ -> 0.5f
    | Suit (14,_) -> 4.5f
    | Suit (13,_) -> 3.5f
    | Suit (12,_) -> 2.5f
    | Suit (11,_) -> 1.5f
    | Suit (i,_) when i <= 10 -> 0.5f
    | _ -> failwithf "Invalid Card %O" c

let dogFor (n: int): int =
    match n with
    | 3 | 4 -> 6
    | 5 -> 3
    | _ -> failwithf "Wrong number of player: %i" n
let sortCard c =
    match c with
    | Trump i -> 300 + i
    | Suit(i,Suit.Heart) -> 100 + i
    | Suit(i,Suit.Clubs) -> 200 + i
    | Suit(i,Suit.Diamonds) -> 400 + i
    | Suit(i,Suit.Spades) -> 500 + i
let deal (n: int): Card [] * Card [] [] =
    let dogCount = dogFor n in
    let shuffled = game |> Seq.shuffleSeeded 42UL |> Seq.toArray
    printfn "%A" shuffled
    let dog = shuffled |> Seq.take dogCount |> Seq.toArray
    printfn "%A" dog
    let players = shuffled |> Seq.skip dogCount |> Seq.splitInto n |> Seq.map (Seq.sortBy sortCard >> Seq.toArray) |> Seq.toArray
    (dog, players)


let playCard (state:Playing) pi i =
    let card = state.Players.[pi].Cards.[i]
    let p = { state.Players.[pi] with
                Cards = state.Players.[pi].Cards |> Seq.removeIndex i |> Seq.toArray
            }
    { state with Players = state.Players
                           |> Array.map (fun pp -> if pp.Index <> p.Index then pp else p )
                 Trick = { state.Trick with PlayedCards = card :: state.Trick.PlayedCards }
    }, card

let isTrump c = match c with
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
                        | Suit (_, ps) -> ps = s || p.Cards |>  (not << Seq.exists (fun c -> isSuitOf s c || isTrump c))
let trickWinner (state:Playing): int * Card list * Card list =
    // TODO excuse+grand chelem = winning trick
    let reved = state.Trick.PlayedCards |> List.rev
    let first =
        if List.head reved = Card.Excuse
        then reved |> List.skip 1 |> List.head
        else List.head reved
    // give a value to cards. trump = bonus 100, right suit = card value, wrong suit/excuse = 0
    let relValue startingSuit c =
        match c with
        | Trump 0 -> 0
        | Trump t -> 100 + t
        | Suit(i,s) when Some(s) = startingSuit -> i
        | _ -> 0
    let winningPlayerIndex =
        match first with
        | Trump _ -> reved |> Seq.indexed |> Seq.maxBy (snd >> relValue None) |> fst
        | Suit(_,ss) -> reved |> Seq.indexed |> Seq.maxBy (snd >> relValue (Some ss)) |> fst
    let winningPlayerIndex = (winningPlayerIndex + state.Trick.StartingPlayer) % state.Players.Length
    let winnerIsAttacker = state.TakerPlayer.Index = winningPlayerIndex
    // this assumes the excuse has been played by the loser
    let loserMustCompensateExcuse = winnerIsAttacker && reved |> List.exists ((=) Card.Excuse)
    let winnerCards = if loserMustCompensateExcuse then reved |> List.filter ((<>) Card.Excuse) else reved
    let loserCards = if loserMustCompensateExcuse then [Card.Excuse] else []
    winningPlayerIndex, winnerCards, loserCards
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

let init () =
    let dog, players = deal 4 in
    GameState.Playing {
        Players = players |> Seq.mapi (fun i x -> {Index=i;Name = "asd";Cards=x}) |> Seq.toArray
        Taker = 0
        Trick = { StartingPlayer=0; PlayedCards = [] }
        AttackerTricks = dog |> List.ofArray
        DefenderTricks = []
    }