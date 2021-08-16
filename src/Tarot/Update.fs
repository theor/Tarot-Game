module Tarot.Update

open Fable.Core
open Tarot.Types
open Tarot.Game

let update (msg: Msg) (model: Model) =
    match model with
    | Playing playing ->
        match msg with
        | GetSize(w,h) ->
            Tarot.View.resize w h
            JS.console.log("got size", !Tarot.View.size)
            (model), []
        | Msg.EndRound ->
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
            (Playing m'), []// cmd
        | _ -> failwithf "Action %A not implemented for state %A" msg model
    | _ -> failwithf "Model %A Action %A not implemented" model msg