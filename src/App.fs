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
    | Increment
    | Decrement

let init (): Model =
    let dog, players = Tarot.Types.deal 4 in
    GameState.Playing {
        Players = players |> Seq.mapi (fun i x -> {Index=i;Name = "asd";Cards=x}) |> Seq.toArray
        Taker = 0
        Trick = { StartingPlayer=0; PlayedCards = [] }
    }

// UPDATE

let update (msg: Msg) (model: Model) =
    match msg with
    | _ -> model
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

let viewCard dispatch (c: Card) =
//    let isRed, isBlack, isTrump =
//        match c with
//        | Trump _ -> false, false, true
//        | Suit (_, Suit.Heart)
//        | Suit (_, Suit.Diamonds) -> true, false, false
//        | Suit (_, Suit.Spades)
//        | Suit (_, Suit.Clubs) -> false, true, false
        let cl = match c with
                    | Trump i -> sprintf "card bg-%i" i
                    | Suit (i, Suit.Heart) -> sprintf "card bg-h%i" i
                    | Suit (i, Suit.Diamonds) -> sprintf "card bg-d%i" i
                    | Suit (i, Suit.Spades) -> sprintf "card bg-s%i" i
                    | Suit (i, Suit.Clubs) -> sprintf "card bg-c%i" i

        div [ Class cl ] [ ]
//    div [ classList [ "card", true
//                      "card-red", isRed
//                      "card-black", isBlack
//                      "card-trump", isTrump ] ] [
//        str (cardSymbol c)
//    ]
//        div [] [ str (sprintf "%O" c) ]
//    ]

let viewPlayerGame dispatch (state:PlayingState) (p:Player) =

    div [Class "player-cards"] (p.Cards |> Seq.map (viewCard dispatch))
let view (model: Model) dispatch =
    Section.section [][
        Container.container[] [
            Button.button [ Button.Color Color.IsDanger ] [
                str "A button"
            ]
            match model with
            | GameState.Playing p ->
                let state = getPlayingState p
                yield Text.span [] [str <| sprintf "State: %O" state]
                yield! p.Players |> Seq.map (viewPlayerGame dispatch state)
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
Program.mkSimple init update view
|> Program.withReactSynchronous "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.withConsoleTrace
|> Program.run
