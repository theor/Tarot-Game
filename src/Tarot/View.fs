module Tarot.View

open Browser.Types
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Animejs
open Fable.React
open Fable.React.Props
open Tarot.Game
open Tarot.Types
open Fulma

type Dir = Horizontal | Vertical
let size = ref {| x=Browser.Dom.window.innerWidth; y=Browser.Dom.window.innerHeight|}
let ratio () = size.Value.y / size.Value.x
let normalizedWidth = 1000.
let normalizedHeight () = normalizedWidth * ratio()
let zoomFactor () = size.Value.x / normalizedWidth
//JS.console.log("win size", !size)
let cardRatio = 272. / 148.
let actualCardWidth = 100.
let cardSize = if size.Value.x < size.Value.y
               then {|x = actualCardWidth; y=actualCardWidth*cardRatio |}
               else {|x = actualCardWidth/cardRatio; y=actualCardWidth |}
let spaceX = cardSize.x * 0.25
let spaceY = spaceX
let trickCardPosition i =
    normalizedWidth / 2. + float (i - 2) * spaceX - cardSize.x/2.,
    normalizedHeight()

let animeCmd (dispatch: Msg -> unit) (msg:Msg) (x:AnimInput) =
    x.complete <- fun a -> JS.console.log("Done, dispatching " + msg.ToString(), a); dispatch msg
    anime.Invoke x |> ignore
    
    
[<RequireQualifiedAccess>]
type CardClass = Table | Player | Trick 

let viewCard (cardClass:CardClass) (getPos: float*float) valid (onClick: (MouseEvent -> unit) option) (dir:Dir) (c: Card): ReactElement =
    let cl = match c with
                | Trump i -> sprintf "card card%i" i
                | Suit (i, Suit.Heart) -> sprintf "card cardh%i" i
                | Suit (i, Suit.Diamonds) -> sprintf "card cardd%i" i
                | Suit (i, Suit.Spades) -> sprintf "card cards%i" i
                | Suit (i, Suit.Clubs) -> sprintf "card cardc%i" i

    let x,y = getPos
    div [ yield Class $"card-wrapper {dir.ToString().ToLowerInvariant()} {cardClass.ToString().ToLowerInvariant()}"
          yield Style [ Top y; Left x ]
          yield Key cl
          match onClick with
              | Some onClick -> yield OnClick (fun e -> onClick e)
              | None -> ()
          
        ] [
        div [ yield classList [ cl,true
                                "card-playable", Option.isSome onClick
                                "valid", valid && Option.isSome onClick
                                "invalid", not valid && Option.isSome onClick ]
              yield Style [ //BackgroundSize $"7500px"
                            Width cardSize.x
                            Height cardSize.y ] 
        ] [ ]
    ]

    
let viewPlayerGame dispatch (playing) (state:PlayingState) (p:Player) =
    let onClick cardIndex valid =
        match valid,state with
        | true,PlayingState.WaitForCard pi when pi = p.Index ->
            Some (fun (e:MouseEvent) ->
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
            )
        | _,_ -> None
    let (sx,sy), (fx,fy), dir = match p.Index with
                                | 0 -> (* bottom *) (2.*cardSize.x, normalizedHeight() - cardSize.y),(spaceX,0.), Dir.Horizontal
                                | 1 -> (* right *) (normalizedWidth - cardSize.y,0.),(0.,spaceY), Dir.Vertical
                                | 2 -> (* top *) (2.*cardSize.x,0.),(spaceX,0.), Dir.Horizontal
                                | _ -> (* left *) (cardSize.x / 2., 0.),(0.,spaceY), Dir.Vertical

    let mapCard i c =
        let valid = (cardCanBePlayed playing p.Index i)
        viewCard CardClass.Player (sx + fx * (float i), sy + fy * (float i)) valid (onClick i valid) dir c
    [
        yield Text.span [] [ str <| $"Player %i{p.Index}:" ]
        yield! (p.Cards |> Seq.mapi mapCard)
    ]

let refHook dispatch elt =
    // called twice, first time with null, see https://github.com/facebook/react/issues/9328
    if isNullOrUndefined elt then ()
    else
        JS.console.log("ref START ANIM")
        animeCmd dispatch EndRound
        <| jsOptions<Animejs.AnimInput>(fun x ->
                x.targets <- !!".table" 
                x.duration <- !!1000.
                x.Item("left") <- !!(-cardSize.x)
                )
let view (model: Model) dispatch =
        match model with
        | GameState.Playing p ->
            let state = getPlayingState p
            let refCb = match getPlayingState p with
                        | PlayingState.EndRound ->
                              JS.console.log("ref HOOK ANIM")
                              Some <| refHook dispatch
                        | _ -> None
                
            div [ yield Class "playing-area"
                  if Option.isSome refCb then yield refCb |> Option.get |> Ref
                  yield Style [ Width <| !!(normalizedWidth - 20.)
                                Height <| !!(normalizedHeight())
                                Zoom (zoomFactor())  ]
            ] [
                yield Text.span [] [str <| sprintf "State: %O" state]
                yield! p.Trick.PlayedCards
                       |> Seq.rev
                       |> Seq.mapi (fun i x ->
                                                viewCard CardClass.Table (trickCardPosition i) false None Dir.Horizontal x)
                yield! p.Players |> Seq.collect (viewPlayerGame dispatch p state)
            ]

//                yield div [Class "game-open attack-tricks"] [
//                    yield Text.span [] [str "Att"]
//                    yield! (p.AttackerTricks |> Seq.mapi (viewCard CardClass.Trick (fun i -> (float i * spaceX, 600.)) false None Dir.Horizontal))
//                    ]
//                yield div [Class "game-open defense-tricks"] [
//                    yield Text.span [] [str "Def"]
//                    yield! (p.DefenderTricks |> Seq.mapi (viewCard CardClass.Trick (fun i -> (float i * spaceX, 800.)) false None Dir.Horizontal))
//                    ]

        | _ -> failwithf "invalid state %O" model