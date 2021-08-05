module Tarot.Tests

open System
open NUnit.Framework
open NUnit.Framework.Internal
open Tarot.Game
open Tarot.Types

[<SetUp>]
let Setup () =
    ()

[<Test>]
let GameHas78Cards () =
    Assert.AreEqual(78, game |> Seq.length)
[<Test>]
let GameHas91Points () =
    game |> Seq.iter (fun x -> printfn "%A" x)
    Assert.AreEqual(91, game |> Seq.sumBy pointsOf)
[<Test>]
let Test1 () =
    [ Suit (1, Suit.Heart)
      Suit (14, Spades)
      Trump 1
      Trump 0
      Trump 21
      Trump 20 ] |> List.iter (fun (c:Card) -> printfn "%O" c)
    Assert.Pass()

[<Test>]
let RndGameHas78Points () =
//    deal 5 |> printf "%A"
    let d,p = deal 5 in
    Assert.AreEqual(78,  Seq.concat (p |> Seq.ofArray |> Seq.map Seq.ofArray) |> Seq.append d |> Seq.distinct |> Seq.length)
    ()

let testGame (cardsPerPlayer: Card seq seq): Playing =
    let players = cardsPerPlayer |> Seq.mapi (fun i cards -> {index=i; name=sprintf "Player%i" i; cards = Seq.toArray cards}) |> Seq.toArray
    { Players=players; Taker = 0 }

[<Test>]
let playCardRemovesCard () =
    let g = testGame [
            [Card.Trump 1; Card.Trump 2]
        ]
    let g2 = playCard g 0 0 in
    Assert.AreEqual(1, g2.Players.[0].cards.Length)

[<Test>]
let valid () =
    let g = testGame [
            [Card.Trump 1; Card.Trump 2]
        ]
    let g2 = playCard g 0 0 in
    Assert.AreEqual(1, g2.Players.[0].cards.Length)