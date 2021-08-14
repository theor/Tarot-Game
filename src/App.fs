module App

open Elmish
open Elmish.Debug
open Elmish.React
open Fable.Core
open Fable.Import.Animejs
open Fable.Import
open Fulma
open Tarot.Ext
open Tarot.Game
open Tarot.Types
open Tarot.View
open Elmish.HMR
open Fable.Core.JsInterop
importAll "../sass/main.sass"

// MODEL


let init (): Model * Cmd<Msg> =
    let dog, players = deal 4 in
    GameState.Playing {
        Players = players |> Seq.mapi (fun i x -> {Index=i;Name = "asd";Cards=x}) |> Seq.toArray
        Taker = 0
        Trick = { StartingPlayer=0; PlayedCards = [] }
        AttackerTricks = dog |> List.ofArray
        DefenderTricks = []
    }, Cmd.none

// UPDATE


let update (msg: Msg) (model: Model) =
    match model with
    | Playing playing ->
        match msg with
        | GetSize(w,h) ->
            Tarot.View.size := {| x=w; y=h |}
            JS.console.log("got size", !Tarot.View.size)
            model, []
        | EndRound ->
            let winnerIndex, winnerCards, loserCards = trickWinner playing
            let attackerCards,defenderCards =
                if winnerIndex = playing.TakerPlayer.Index
                then winnerCards,loserCards
                else loserCards,winnerCards
            JS.console.log("winner", winnerIndex)
            Playing <| {playing with
                            Trick = startTrick 0
                            AttackerTricks = attackerCards @ playing.AttackerTricks
                            DefenderTricks = defenderCards @ playing.DefenderTricks },[]
        | PlayCard(p,ci) ->
            let m',playedCard = playCard playing p.Index ci
            let cmd = match getPlayingState m' with
                      | PlayingState.EndRound ->
                            Cmd.OfAsync.perform (fun () -> async {
                                  do! Async.Sleep 1000
                                  return playedCard
                              }) () (fun _card -> EndRound)
                      | _ -> []
            Playing m', cmd
        | _ -> failwithf "Action %A not implemented for state %A" msg model
    | _ -> failwithf "Model %A Action %A not implemented" model msg

// VIEW (rendered with React)

open Tarot.Types

let CardBack = 0x1F0A0
let cardSymbol (c: Card) =
    let uni =
        match c with
        | Trump t -> 0x1F0E0 + t
        | Suit (t, s) ->
            t
            + match s with
              | Suit.Spades -> 0x1F0A0
              | Suit.Heart -> 0x1F0B0
              | Suit.Diamonds -> 0x1F0C0
              | Suit.Clubs -> 0x1F0D0
//        | _ -> 0x1F0BF

    Char.toUnicode (uni)



// App
Program.mkProgram init update Tarot.View.view
|> Program.withReactSynchronous "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.withConsoleTrace
|> Program.run
