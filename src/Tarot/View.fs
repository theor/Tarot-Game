module Tarot.View

open Browser.Types
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import.Animejs
open Fable.React
open Fable.React.Props
open Tarot.Game
open Tarot.Types
open Fulma

type Dir = Horizontal | Vertical
let size = ref {| x=Browser.Dom.window.innerWidth; y=Browser.Dom.window.innerHeight|}
//JS.console.log("win size", !size)
let cardSize = {|x = 148.; y=272. |}
let spaceX = 30.;
let spaceY = 30.
let trickCardPosition i =
    size.Value.x / 2. + float (i - 2) * spaceX - cardSize.x/2.,
    (size.Value.y - cardSize.y) / 2.

let animeCmd dispatch msg (x:AnimInput) =
    x.complete <- fun a -> dispatch msg
    anime.Invoke x |> ignore

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

    
let viewPlayerGame dispatch (playing) (state:PlayingState) (p:Player) =
    let onClick valid =
        match valid,state with
        | true,PlayingState.WaitForCard pi when pi = p.Index ->
            Some (fun (e:MouseEvent) cardIndex ->
                JS.console.log(e)
                let targetPos = trickCardPosition playing.Trick.PlayedCards.Length
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
                                x.Item("left") <- !!(fst targetPos)
                                x.Item("top") <- !!(snd targetPos)
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
                yield! (p.Trick.PlayedCards |> Seq.rev |> Seq.mapi (viewCard trickCardPosition false None Dir.Horizontal))
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