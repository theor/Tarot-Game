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
    let players = cardsPerPlayer |> Seq.mapi (fun i cards -> {Index=i; Name=sprintf "Player%i" i; Cards = Seq.toArray cards}) |> Seq.toArray
    { Players=players; Taker = 0; Trick = {PlayedCards = []; StartingPlayer = 0} }

[<Test>]
let playCardRemovesCard () =
    let g = testGame [
            [Card.Trump 1; Card.Trump 2]
        ]
    let g2,_ = playCard g 0 0 in
    Assert.AreEqual(1, g2.Players.[0].Cards.Length)

[<Test>]
let valid () =
    let g = testGame [
            [Card.Trump 1; Card.Trump 2]
        ]
    let g2,_ = playCard g 0 0 in
    Assert.AreEqual(1, g2.Players.[0].Cards.Length)


let playableCardRulesCases() =
    seq {

        yield [Trump 1; Suit(1,Diamonds)], Suit(2,Diamonds), [Trump 2],false,"Suit on trump when having a trump"

        yield [Trump 1; Suit(1,Diamonds)], Suit(2,Diamonds), [Suit(3,Diamonds)],true,"Suit on trump when no trump left"
        yield [Trump 1; Suit(1,Diamonds)], Suit(2,Diamonds), [Card.Excuse],true,"Suit on trump when excuse left"
        yield [Trump 1; Suit(1,Diamonds)], Card.Excuse, [Trump 2],true,"Excuse on trump when trump left"
        yield [Trump 2], Trump 3, [Trump 1],true,"Greater trump on trump"
        yield [Trump 3], Trump 1, [Trump 2],true,"Lower trump on trump when no greater trump left"
        yield [Trump 3], Trump 1, [Trump 4],false,"Lower trump on trump when greater trump left"
        yield [Suit(1,Diamonds)], Trump 1, [Suit(2,Diamonds)],false,"Trump on suit when suit left"
        yield [Suit(1,Diamonds)], Suit(1,Heart), [Suit(2,Diamonds)],false,"Wrong suit on suit when suit left"
        yield [Suit(1,Diamonds)], Suit(1,Heart), [Suit(2,Spades)],true,"Other suit on suit when no suit left"
        yield [Suit(1,Diamonds)], Card.Excuse, [Suit(2,Diamonds)],true,"Excuse on suit when suit left"
        yield [Card.Excuse], Trump 2, [Suit(2,Diamonds)],true,"Trump on Excuse first"
        yield [Card.Excuse], Suit(2,Diamonds), [Trump 2],true,"Suit on Excuse first"
    } |> Seq.map (fun x -> TestCaseData(Microsoft.FSharp.Reflection.FSharpValue.GetTupleFields x)
                               .SetName(sprintf "%s: %s" (if x.Item4 then "Valid" else "Invalid") x.Item5))
[<Test>]
[<TestCaseSource(nameof(playableCardRulesCases))>]
let playableCardRules(playedCards:Card list, card, restOfPlayerGame, expectedValid, _) =
    let state:Playing = {
        Players= [
            yield! playedCards |> Seq.mapi (fun i x -> Player.New i (string i) [||])
            yield Player.New (playedCards.Length + 1) "p" (Seq.toArray <| card :: restOfPlayerGame)
        ]  |> Seq.toArray
        Taker = 0
        Trick= {
            PlayedCards = playedCards |> List.rev // each played card is front-appended to the list, so reverse it
            StartingPlayer = 0
        }
    }
    Assert.AreEqual(expectedValid, cardCanBePlayed state playedCards.Length 0)