module FableHero.App

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Elmish

open FableHero.Types
open Fable.PowerPack

open Fable.Core.JsInterop
open Fable.Helpers.React.Props
open Fable.Helpers.React.ReactiveComponents
open Fable
module R = Fable.Helpers.React    


let userView (user : User) =
    R.div [] [
        R.str <| sprintf "欢迎 %s, " user.Name
        R.str "余额"
        R.str (user.Balance.ToString())
    ]

let horseView (horse : Horse, multiplier : Multiplier) = 
    R.div [ ClassName "card text-center" ] [
            R.div [ ClassName "card-body" ] [
                    R.h3 [ ClassName "card-title" ] [ R.str horse.Name ]
                    R.div [] [ R.str "赔率" ]
                    R.div [] [ R.str (sprintf "1:%i" multiplier) ]
                    R.button [] [ R.str "下注" ]
                ]
        ]

let navView dispatch model =
    R.nav [ ClassName "navbar navbar-light bg-light" ]
        <| match model.Player with 
            | LoggedIn user -> 
                [ 
                    R.str "赛马"
                    userView user
                    R.a [ ClassName "nav-link"; OnClick (fun _ -> dispatch SignOut) ] [ R.str "退出登录" ]
                ]
            | Guest _ -> [ R.str "赛马" ]

let mainView dispatch model =
    R.div [] [
        R.div [ ClassName "row" ] [
            R.h5 [] [ R.str (sprintf "第 %i 期" model.Round) ]
        ]
        R.div [ ClassName "card-deck" ] (model.Odds |> List.map horseView)
        R.div [ ClassName "row" ] [
            R.button [ OnClick (fun _ -> dispatch Race) ] [ R.str "开跑" ]
        ]
    ]

let loginView dispatch model =
    R.div [] [
        R.form [] [
            R.input [ 
                ClassName "form-control"
                Placeholder "请输入用户名"
                DefaultValue (match model.Player with | LoggedIn x -> x.Name | Guest x -> x.Name)
                OnChange (fun ev -> UpdateUserName (!!ev.target?value) |> dispatch) ]
            R.button [ OnClick (fun _ -> dispatch SignIn ) ] [ R.str "登录" ]
        ]
    ]

let view dispatch model = 
    R.div [ ClassName "container" ] [
        navView dispatch model
        (match model.Page with
         | Login -> loginView dispatch model
         | Main -> mainView dispatch model)
    ]

open Elmish.React
open Elmish.Browser.Navigation
open Elmish.Browser.UrlParser

let route : Parser<Page -> Page, Page> =
    oneOf [
        map Login (s "/")
        map Main (s "main")
    ]

let urlUpdate (result : Option<Page>) model =
    match result with
    | Some Login -> 
        model, []
    | Some Main -> 
        model, []
    | None -> 
        model, Navigation.modifyUrl "#"

let init result =
    urlUpdate result (zero ())
    
let update (model : GameState) msg = 
    match msg with
    | Race -> 
        { model with Round = model.Round + 1
                     Odds = odds horses
                     Result = race () }, []
    | Bet (horseID, wager) -> 
        model, []
    | UpdateUserName username ->
        let player = match model.Player with
                     | LoggedIn user -> LoggedIn { user with Name = username }
                     | Guest user -> Guest { user with Name = username }
        { model with Player = player }, []
    | SignIn ->
        let player = match model.Player with
                     | LoggedIn user -> LoggedIn user
                     | Guest user -> LoggedIn { Name = user.Name; Balance = 1000m }
        { model with Page = Main; Player = player }, []
    | SignOut ->
        { model with Page = Login; Player = Guest { Name = "" } }, []

Program.mkProgram init update view
|> Program.toNavigable (parseHash route) urlUpdate
|> Program.withConsoleTrace
|> Program.withReact "fable-hero"
|> Program.run

