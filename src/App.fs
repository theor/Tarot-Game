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
[<ImportMember("./Firebase.js")>]
let logOut () = jsNative
[<ImportMember("./Firebase.js")>]
let onAuthStateChanged (cb: obj -> unit) = jsNative

module Auth =
    type User = {id:string
                 email: string
                 name: string}
    type Model = User option
    type Msg =
        | LogOut
        | LogIn
        | LoggedIn of User option
    type AppModel<'m> = {
        user: User option
        UserModel: 'm
    }
    let createModel = None


    let sub dispatch =
        onAuthStateChanged(fun u -> dispatch(LoggedIn <| if isNullOrUndefined u then None else Some (u :?> User)))
    let update msg (model:Model) =
        match msg with
        | LogOut ->
            logOut()
            None, []
        | LogIn ->
            loginFlow()
            model, []
        | LoggedIn u -> u, []
    let view  (model:Model) dispatch =
        div [] [
            match model with
            | None -> div [ Id "firebase-auth" ] [ button [ OnClick (fun _ -> loginFlow()) ] [ str "Login" ] ]
            | Some u -> button [ OnClick (fun _ -> dispatch LogOut) ] [str $"Logout {u.id}"]
        ]

//    program |> Program.map mapInit mapUpdate mapView mapSetState mapSubscribe
    

module TestApp =
    type TestModel = int * Auth.Model
    type TestMsg = Increment | Decrement | AuthMsg of Auth.Msg
    let init()= (0, None), Cmd.none
    let update msg (m,auth) =
        match msg with
        | Increment -> (m+1,auth) , Cmd.none
        | Decrement -> (m-1,auth) , Cmd.none
        | AuthMsg msg ->
            let auth',cmdAuth = Auth.update msg auth
            (m,auth'), Cmd.map AuthMsg cmdAuth
    let view m dispatch =
        div [] [
            div [] [str (string m)]
            button [ OnClick (fun e -> dispatch Increment) ] [str "+"]
            button [ OnClick (fun e -> dispatch Decrement) ] [str "-"]
            Auth.view (snd m) (dispatch << AuthMsg)
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
//Program.mkProgram init update view
Program.mkProgram TestApp.init TestApp.update TestApp.view
|> Program.withReactSynchronous "elmish-app"
|> Program.withSubscription (fun m -> Cmd.ofSub (fun dispatch -> Auth.sub (dispatch << TestApp.AuthMsg)))
//|> Program.withSubscription (fun m -> Cmd.ofSub (fun dispatch ->
//    Browser.Dom.window.onresize <- fun e -> dispatch (GetSize(Browser.Dom.window.innerWidth, Browser.Dom.window.innerHeight)))
//    )
#if DEBUG
|> Program.withDebugger
#endif
|> Program.withConsoleTrace
|> Program.run
