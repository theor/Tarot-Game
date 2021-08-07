module App

(**
 The famous Increment/Decrement ported from Elm.
 You can find more info about Elmish architecture and samples at https://elmish.github.io/
*)

open Elmish
open Elmish.Debug
open Elmish.React
open Fable.React
open Fable.React.Props
open Fulma
open Tarot
open Tarot.Ext
open Tarot.Game
open Elmish.HMR
open Fable.Core.JsInterop
importAll "../sass/main.sass"

// MODEL

type Model = GameState

type Msg =
    | PlayCard of Player * int
    | EndRound

let init (): Model * Cmd<Msg> =
    let dog, players = Tarot.Types.deal 4 in
    GameState.Playing {
        Players = players |> Seq.mapi (fun i x -> {Index=i;Name = "asd";Cards=x}) |> Seq.toArray
        Taker = 0
        Trick = { StartingPlayer=0; PlayedCards = [] }
        AttackerTricks = []
        DefenderTricks = []
    }, Cmd.none

// UPDATE

let update (msg: Msg) (model: Model) =
    match model with
    | Playing playing ->
        match msg with
        | EndRound -> Playing <| {playing with Trick = startTrick 0 },[]
        | PlayCard(p,ci) ->
            let m',playedCard = playCard playing p.Index ci
            let cmd = match getPlayingState m' with
                      | PlayingState.EndRound -> Cmd.OfAsync.perform (fun () -> async {
                                                                                      do! Async.Sleep 1000
                                                                                      return playedCard
                                                                                  }) () (fun _card -> EndRound)
                      | _ -> []
            Playing m', cmd
        | _ -> failwithf "Action %A not implemented for state %A" msg model
    | _ -> failwithf "Model %A Action %A not implemented" model msg
//    | Increment -> model + 1
//    | Decrement -> model - 1

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
        | _ -> 0x1F0BF

    Char.toUnicode (uni)

let viewCard valid (onClick: (int -> unit) option) (cardIndex: int) (c: Card): ReactElement =
    let cl = match c with
                | Trump i -> sprintf "card bg-%i" i
                | Suit (i, Suit.Heart) -> sprintf "card bg-h%i" i
                | Suit (i, Suit.Diamonds) -> sprintf "card bg-d%i" i
                | Suit (i, Suit.Spades) -> sprintf "card bg-s%i" i
                | Suit (i, Suit.Clubs) -> sprintf "card bg-c%i" i

    div [Class "card-wrapper"] [
        div [ yield classList ["card",true;cl,true; "card-playable", Option.isSome onClick; "valid", valid && Option.isSome onClick; "invalid", not valid && Option.isSome onClick]
              match onClick with
              | Some onClick -> yield OnClick (fun _ -> onClick cardIndex)
              | None -> () ] [ ]
    ]

let viewPlayerGame dispatch (playing) (state:PlayingState) (p:Player) =
    let onClick = match state with
                  | PlayingState.WaitForCard pi when pi = p.Index -> (Some (fun cardIndex -> dispatch <| PlayCard(p, cardIndex) ))
                  | _ -> None
    div [] [
        Text.div [] [ str <| sprintf "Player %i:" p.Index ]
        div [Class "player-cards"] (p.Cards |> Seq.mapi (fun i c -> viewCard (cardCanBePlayed playing p.Index i) onClick i c))
    ]

let view (model: Model) dispatch =
    Section.section [][
        Container.container[Container.IsFullHD] [
            match model with
            | GameState.Playing p ->
                let state = getPlayingState p
                yield div [Class "player-cards"] (p.Trick.PlayedCards |> Seq.rev |> Seq.mapi (viewCard false None))
                yield Text.span [] [str <| sprintf "State: %O" state]
                yield! p.Players |> Seq.map (viewPlayerGame dispatch p state)
            | _ -> failwithf "invalid state %O" model
//            div [] (game |> Seq.map (viewCard dispatch))
        ]
    ]
//  div []
//      [ button [ OnClick (fun _ -> dispatch Increment) ] [ str "+" ]
//        div [] [ str (string model) ]
//        button [ OnClick (fun _ -> dispatch Decrement) ] [ str "-" ]
//        viewCard (Card.Suit (1, Heart)) dispatch
//        viewCard (Card.Trump 1) dispatch
//      ]

// App
Program.mkProgram init update view
|> Program.withReactSynchronous "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.withConsoleTrace
|> Program.run
