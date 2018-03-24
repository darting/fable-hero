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

let horseBetView dispatch (horse : Horse, multiplier : Multiplier) = 
    R.div [ ClassName "card text-center" ] [
            R.div [ ClassName "card-body" ] [
                    R.h3 [ ClassName "card-title" ] [ R.str horse.Name ]
                    R.div [] [ R.str "赔率" ]
                    R.div [] [ R.str (sprintf "1:%i" multiplier) ]
                    R.button [ OnClick (fun _ -> (horse.ID, 1m) |> Bet |> dispatch) ] [ R.str "下注" ]
                ]
        ]

let gameHistoryView (game : Game) = 
    let findWin horse = 
        maybe {
           let! (_, wager, win) = List.tryFind (fun (x, _, _) -> x = horse.ID) game.Bets
           return wager, win 
        }

    let cells = game.Result
                |> List.map 
                    (fun horse -> 
                        horse, 
                        game.Odds |> List.find (fun (x, _) -> x = horse) |> snd,
                        findWin horse)

    let horseView (horse : Horse, odds : Multiplier, bets : (decimal * decimal) option) = 
        R.td [] [ 
            R.str horse.Name
            R.span [ ClassName "badge badge-light" ] [ R.str (sprintf "1:%i" odds) ]
            
            (match bets with
            | Some (wager, win) -> 
                let style = ClassName <| 
                                if win > 0m then "badge badge-danger" 
                                else "badge badge-secondary"
                R.div [] [
                    R.span [ style ] 
                           [ R.str <| sprintf " 下注:%M, 赢:%M" wager win ]
                ]
            | None -> R.div [] [])
        ]

    let round = R.td [] [ R.str (sprintf "第 %i 期" game.RoundID) ]
    let cols = round :: (List.map horseView cells)
    R.tr [] cols

let historyView (games : List<Game>) = 
    R.table [ ClassName "table table-bordered table-sm" ] [ 
        R.thead [  ] [ 
            R.th [ ] [ R.str "期数" ] 
            R.td [] [ R.str "第一名" ]
            R.td [] [ R.str "第二名" ]
            R.td [] [ R.str "第三名" ]
            R.td [] [ R.str "第四名" ]
            R.td [] [ R.str "第五名" ]
            R.td [] [ R.str "第六名" ]
            R.td [] [ R.str "第七名" ]
            R.td [] [ R.str "第八名" ]
        ]
        games 
        |> List.map gameHistoryView
        |> R.tbody [] ]

let betSummaryView (game : Game) =
    let cells = game.Bets 
                |> List.map (fun (h, b, _) ->
                                let horse = horses |> List.find (fun x -> x.ID = h)
                                R.td [] [ R.str (sprintf "%s : 下注 %M" horse.Name b) ])
    
    R.table [ ClassName "table table-bordered table-sm" ] [ 
        R.thead [  ] [
            R.th [ ColSpan (float cells.Length) ] [ R.str "下注明细" ]
        ]
        R.tbody [] [ R.tr [] cells ] ]
    

let navView dispatch model =
    R.nav [ ClassName "navbar navbar-light bg-light" ]
        <| match model.Player with 
            | LoggedIn user -> 
                [ 
                    R.str "赛马"
                    userView user
                    R.button [ ClassName "btn btn-sm btn-outline-secondary"; 
                               OnClick (fun _ -> dispatch SignOut) ] 
                             [ R.str "退出登录" ]
                ]
            | Guest user -> 
                [ 
                    R.str "赛马" 
                    userView { user with Name = "来宾" }
                    R.button [ ClassName "btn btn-sm btn-outline-secondary"; 
                               OnClick (fun _ -> dispatch SignOut) ] 
                             [ R.str "登录" ]
                ]

let mainView dispatch model =
    R.div [] [
        R.div [ ClassName "row" ] [
            R.h5 [] [ R.str (sprintf "第 %i 期" model.Current.RoundID) ]
        ]
        R.div [ ClassName "card-deck" ] (model.Current.Odds |> List.map (horseBetView dispatch))
        R.div [ ClassName "row" ] [
            R.button [ OnClick (fun _ -> dispatch Race) ] [ R.str "开跑" ]
        ]
        R.hr []
        betSummaryView model.Current
        historyView model.History
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
open Elmish.Browser

let route : Parser<Page -> Page, Page> =
    oneOf [
        map Login (s "/")
        map Main (s "main")
    ]

let urlUpdate (result : Option<Page>) model =
    match result with
    | Some Login -> 
        { model with Page = Login }, []
    | Some Main -> 
        { model with Page = Main }, []
    | None -> 
        model, Navigation.modifyUrl "#"

let init result =
    urlUpdate result (zero ())
    
let update (model : State) msg = 
    match msg with
    | Race -> 
        let horses = race ()
        let winner = horses |> List.head
        let multiplier = model.Current.Odds |> List.find (fun (h, _) -> h = winner) |> snd
        let bets = model.Current.Bets 
                   |> List.map (fun (h, b, _) -> 
                                    h, b, if h = winner.ID then b * (decimal multiplier) else 0m)
        let winAmount = bets |> List.sumBy (fun (_,_,w) -> w)
        let player = match model.Player with
                     | LoggedIn user -> LoggedIn { user with Balance = user.Balance + winAmount }
                     | Guest user -> Guest { user with Balance = user.Balance + winAmount } 
        let current = { model.Current with Result = horses }
        let newGame = newGame ()
        let history = { current with Bets = bets } :: model.History 
        { model with Current = newGame; History = history; Player = player }, []
    | Bet (horseID, wager) -> 
        let user = match model.Player with | LoggedIn user | Guest user -> user
        if user.Balance < wager then
            model, []
        else
            let player = match model.Player with 
                         | LoggedIn user -> 
                            LoggedIn { user with Balance = user.Balance - wager }
                         | Guest user -> 
                            Guest { user with Balance = user.Balance - wager }
            let bet = 
                maybe {
                    let! (id, b, w) = model.Current.Bets |> List.tryFind ((fun (id, _, _) -> id = horseID))
                    return id, b + wager, w
                }
                |> Option.defaultValue (horseID, wager, 0m)
            let bets = bet :: (model.Current.Bets |> List.filter ((fun (id, _, _) -> id <> horseID)))
            let game = { model.Current with Bets = bets }
            { model with Current = game; Player = player }, []
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
        { model with Page = Login; Player = Guest { Name = ""; Balance = 0m } }, []

Program.mkProgram init update view
|> Program.toNavigable (parseHash route) urlUpdate
|> Program.withConsoleTrace
|> Program.withReact "fable-hero"
|> Program.run

