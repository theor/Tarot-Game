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

//type Error =

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
let deal (n: int) =
    let dogCount = dogFor n in
    let shuffled = game |> Seq.shuffleSeeded 42 |> Seq.toArray
    printfn "%A" shuffled
    let dog = shuffled |> Seq.take dogCount |> Seq.toArray
    let players = shuffled |> Seq.skip dogCount |> Seq.splitInto n |> Seq.toArray
    (dog, players)




