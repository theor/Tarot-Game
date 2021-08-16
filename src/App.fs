module App

open Elmish
open Elmish.Debug
open Elmish.React
open Fable.Core
open Fable.React
open Fable.React.Props
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

[<ImportMember("./Firebase.js")>]
let loginFlow () = jsNative

type User = {id:string}
type AppModel<'m> = {
    user: User option
    model: 'm
}

module TestApp =
    type TestModel = int
    type TestMsg = Increment | Decrement
    let testInit()= 0
    let testUpdate msg m =
        match msg with
        | Increment -> m+1 
        | Decrement -> m-1 
    let testView m dispatch =
        div [] [
            div [] [str (string m)]
            button [ OnClick (fun e -> dispatch Increment) ] [str "+"]
            button [ OnClick (fun e -> dispatch Decrement) ] [str "-"]
        ]

// MODEL
type Model = Tarot.Types.Model * int

let init (): Model * Cmd<Msg> =
    
    (Tarot.Game.init (), 0), Cmd.none

// UPDATE


let update (msg: Msg) (m: Model) =
    let model,i = m
    let model', cmd = Tarot.Update.update msg model
    ((model', i), cmd)
   

// VIEW (rendered with React)

let view (m,i) dispatch =
    div [] [
        div [ Id "firebase-auth" ] []
        Fulma.Button.button [ Button.Option.OnClick (fun _ -> loginFlow()) ] [ str "login" ]
        Tarot.View.view m dispatch
    ]
// App
Program.mkProgram init update view
|> Program.withReactSynchronous "elmish-app"
|> Program.withSubscription (fun m -> Cmd.ofSub (fun dispatch ->
    Browser.Dom.window.onresize <- fun e -> dispatch (GetSize(Browser.Dom.window.innerWidth, Browser.Dom.window.innerHeight)))
    )
#if DEBUG
|> Program.withDebugger
#endif
|> Program.withConsoleTrace
|> Program.run
