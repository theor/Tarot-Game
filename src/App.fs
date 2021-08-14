module App

open Browser.Types
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

// MODEL

type Model = GameState

type Msg =
    | PlayCard of Player * int
    | EndRound
    | GetSize of float * float

let size = ref {| x=Browser.Dom.window.innerWidth; y=Browser.Dom.window.innerHeight|}
JS.console.log("win size", !size)
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

let animeCmd dispatch msg (x:AnimInput) =
    x.complete <- fun a -> dispatch msg
    anime.Invoke x |> ignore

let update (msg: Msg) (model: Model) =
    match model with
    | Playing playing ->
        match msg with
        | GetSize(w,h) ->
            size := {| x=w; y=h |}
            JS.console.log("got size", !size)
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

type Dir = Horizontal | Vertical
let viewCard (getPos: int -> float*float) valid (onClick: (MouseEvent -> int -> unit) option) (dir:Dir) (cardIndex: int) (c: Card): ReactElement =
    let cl = match c with
                | Trump i -> sprintf "card bg-%i" i
                | Suit (i, Suit.Heart) -> sprintf "card bg-h%i" i
                | Suit (i, Suit.Diamonds) -> sprintf "card bg-d%i" i
                | Suit (i, Suit.Spades) -> sprintf "card bg-s%i" i
                | Suit (i, Suit.Clubs) -> sprintf "card bg-c%i" i

    let x,y = getPos cardIndex
    div [ yield Class $"card-wrapper {dir.ToString().ToLowerInvariant()}"
          yield Style [ Top y; Left x ]
          yield Key cl
          match onClick with
              | Some onClick -> yield OnClick (fun e -> onClick e cardIndex)
              | None -> ()
//          yield Ref (fun e -> ())
          
        ] [
        div [ yield classList [cl,true; "card-playable", Option.isSome onClick; "valid", valid && Option.isSome onClick; "invalid", not valid && Option.isSome onClick]
            ] [ ]
    ]

let cardSize = {|x = 148.; y=272. |}
let spaceX = 30.;
let spaceY = 30.
let viewPlayerGame dispatch (playing) (state:PlayingState) (p:Player) =
    let onClick valid =
        match valid,state with
        | true,PlayingState.WaitForCard pi when pi = p.Index ->
            Some (fun (e:MouseEvent) cardIndex ->
                JS.console.log(e)
                animeCmd dispatch (PlayCard(p, cardIndex))
                    <| jsOptions<AnimInput>(fun x ->
                                x.targets <- !!e.currentTarget
                                x.easing <- !!Easing.EaseOutQuint
//                                x.round <- !!true
                                x.duration <- !!1200.
                                x.rotate <- !!jsOptions<PropertyParameters>(fun p ->
                                                                            p.value <- !!"1turn"// !!0.
                                                                            p.easing <- !!Easing.EaseOutSine
                                                                            p.duration <- !!1000)
                                x.Item("left") <- !!(size.Value.x / 2. + float playing.Trick.PlayedCards.Length * spaceX)
                                x.Item("top") <- !!(size.Value.y / 2.)
                                )
//                dispatch <| PlayCard(p, cardIndex) ))
            )
        | _,_ -> None
    let (sx,sy), (fx,fy), dir = match p.Index with
                                | 0 -> (* bottom *) (2.*cardSize.x, (!size).y - cardSize.y),(spaceX,0.), Dir.Horizontal
                                | 1 -> (* right *) ((!size).x - cardSize.y, 0.),(0.,spaceY), Dir.Vertical
                                | 2 -> (* top *) (2.*cardSize.x,0.),(spaceX,0.), Dir.Horizontal
                                | _ -> (* left *) (cardSize.x / 2., 0.),(0.,spaceY), Dir.Vertical

    let mapCard i c =
        let valid = (cardCanBePlayed playing p.Index i)
        viewCard (fun i -> sx + fx * (float i), sy + fy * (float i)) valid (onClick valid) dir i c
    [
        yield Text.span [] [ str <| $"Player %i{p.Index}:" ]
        yield! (p.Cards |> Seq.mapi mapCard)
    ]

let view (model: Model) dispatch =
        match model with
        | GameState.Playing p ->
            let state = getPlayingState p

            div [Class "playing-area"] [
                yield Text.span [] [str <| sprintf "State: %O" state]
                yield! (p.Trick.PlayedCards |> Seq.rev |> Seq.mapi (viewCard (fun i -> ((!size).x / 2. + float i*spaceX, (!size).y / 2.)) false None Dir.Horizontal))
                yield! p.Players |> Seq.collect (viewPlayerGame dispatch p state)
                ]

//                yield div [Class "game-open attack-tricks"] [
//                    yield Text.span [] [str "Att"]
//                    yield! (p.AttackerTricks |> Seq.mapi (viewCard (fun i -> (float i * spaceX, 600.)) false None Dir.Horizontal))
//                    ]
//                yield div [Class "game-open defense-tricks"] [
//                    yield Text.span [] [str "Def"]
//                    yield! (p.DefenderTricks |> Seq.mapi (viewCard (fun i -> (float i * spaceX, 800.)) false None Dir.Horizontal))
//                    ]

        | _ -> failwithf "invalid state %O" model

// App
Program.mkProgram init update view
|> Program.withReactSynchronous "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.withConsoleTrace
|> Program.run
