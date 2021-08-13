module App

(**
 The famous Increment/Decrement ported from Elm.
 You can find more info about Elmish architecture and samples at https://elmish.github.io/
*)

open Elmish
open Elmish.Debug
open Elmish.React
open Fable.Core
open Fable.Import.Animejs
open Fable.Import
open Fable.React
open Fable.React.Props
open Fulma
open Tarot.Ext
open Tarot.Game
open Elmish.HMR
open Fable.Core.JsInterop
importAll "../sass/main.sass"
//let catpng:string = import "*" "../public/css_sprites.png"
//JS.console.log(catpng)

//importAll "../node_modules/pixi.js/dist/cjs/pixi.js"
 
//import

// MODEL

type Model = GameState

type Msg =
    | PlayCard of Player * int
    | EndRound

let size = ref (900., 500.)

//        anime.Invoke (jsOptions<AnimInput> (fun x ->
//            x.targets <- !!sprite
//            x.duration <- !!1000.
//            x.Item("x") <- x'
//            x.Item("y") <- y'
//            x.Item("angle") <- !!(0.,360.)
//            x.easing <- !!"easeOutQuad"
////            x.rotate <- !!jsOptions<PropertyParameters> (fun (r:PropertyParameters) ->
////                                                            r.value <- !!360.
////                                                            r.duration <- !!2000.)

let init (): Model * Cmd<Msg> =
    let dog, players = Tarot.Types.deal 4 in
    GameState.Playing {
        Players = players |> Seq.mapi (fun i x -> {Index=i;Name = "asd";Cards=x}) |> Seq.toArray
        Taker = 0
        Trick = { StartingPlayer=0; PlayedCards = [] }
        AttackerTricks = dog |> List.ofArray
        DefenderTricks = []
    }, Cmd.none

// UPDATE

let animeCmd msg (x:AnimInput): Cmd<Msg> =
    Cmd.ofSub (fun dispatch ->
        x.complete <- fun a -> dispatch msg
        anime.Invoke x |> ignore
        )

let update (msg: Msg) (model: Model) =
    match model with
    | Playing playing ->
        match msg with
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
    let onClick valid = match valid,state with
                  | true,PlayingState.WaitForCard pi when pi = p.Index -> (Some (fun cardIndex -> dispatch <| PlayCard(p, cardIndex) ))
                  | _,_ -> None
    div [] [
        Text.div [] [ str <| sprintf "Player %i:" p.Index ]
        let mapCard i c =
            let valid = (cardCanBePlayed playing p.Index i)
            viewCard valid (onClick valid) i c
        div [Class "player-cards"] (p.Cards |> Seq.mapi mapCard)
    ]
    
//let mutable ctx =
//    let c = (Browser.Dom.document.getElementById "canvas":?> HTMLCanvasElement)
//    c.getContext_2d()
//
let renderCanvas p =
    ()
//    ctx.fillStyle <- !^"rgb(200,0,0)"
//    ctx.fillRect(10., 10., 50., 50.)

let view (model: Model) dispatch =
    Section.section [][
        Container.container[Container.IsFullHD] [
            match model with
            | GameState.Playing p ->
                renderCanvas p
                let state = getPlayingState p
                yield div [Class "player-cards"] (p.Trick.PlayedCards |> Seq.rev |> Seq.mapi (viewCard false None))
                yield Text.span [] [str <| sprintf "State: %O" state]
                yield! p.Players |> Seq.map (viewPlayerGame dispatch p state)
                yield Text.p [] [str "Att"]
                yield div [Class "player-cards"] (p.AttackerTricks |> Seq.mapi (viewCard false None))
                yield Text.p [] [str "Def"]
                yield div [Class "player-cards"] (p.DefenderTricks |> Seq.mapi (viewCard false None))

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

//let rec tick dispatch time =
//    JS.console.log(time)
//    Browser.Dom.window.requestAnimationFrame(tick dispatch) |> ignore
//    ()

// App
Program.mkProgram init update view
|> Program.withReactSynchronous "elmish-app"
//|> Program.withSubscription (fun m -> Cmd.ofSub (fun dispatch ->
//    (Browser.Dom.document.getElementById "canvas").addEventListener("mousemove", fun e -> JS.console.log e)
//    Browser.Dom.window.requestAnimationFrame(tick dispatch) |> ignore))
#if DEBUG
|> Program.withDebugger
#endif
|> Program.withConsoleTrace
|> Program.run
