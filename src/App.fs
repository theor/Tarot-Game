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
open Tarot.Ext
open Tarot.Game

// MODEL

type Model = GameState

type Msg =
| Increment
| Decrement

let init() : Model = GameState.Playing {
    Players = [||]
    Taker = 0
}

// UPDATE

let update (msg:Msg) (model:Model) =
    match msg with
    _ -> model
//    | Increment -> model + 1
//    | Decrement -> model - 1

// VIEW (rendered with React)

open Tarot.Types
let CardBack = 0x1F0A0

let cardSymbol (c:Card) =
    let uni = match c with
        | Trump t -> 0x1F0E0 + t
        | Suit (t,s) -> t +
            match s with
            | Suit.Spades -> 0x1F0A0
            | Suit.Heart -> 0x1F0B0
            | Suit.Diamonds -> 0x1F0C0
            | Suit.Clubs -> 0x1F0D0
        | _ -> 0x1F0BF
    Char.toUnicode(uni)
let viewCard dispatch (c:Card) =
    let isRed,isBlack,isTrump =
        match c with
        | Trump _ -> false,false,true
        | Suit (_,Suit.Heart) | Suit (_,Suit.Diamonds) -> true,false,false
        | Suit (_,Suit.Spades) | Suit (_,Suit.Clubs) -> false,true,false
    div [] [
        div [ classList ["card",true; "card-red", isRed; "card-black",isBlack; "card-trump",isTrump] ] [
            str (cardSymbol c)
        ]
        div [] [ str (sprintf "%O" c) ]
    ]

let view (model:Model) dispatch =
    div []
      (game |> Seq.map (viewCard dispatch))
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
