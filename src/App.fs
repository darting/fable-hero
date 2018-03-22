module FableHero

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Elmish

open FableHero.Types
open Fable.PowerPack


let update (model : Model) = function
    | Race -> 
        { model with Round = model.Round + 1
                     Odds = odds horses
                     Result = race () }
    | Bet (horseID, wager) -> model


open Fable.Core.JsInterop
open Fable.Helpers.React.Props
module R = Fable.Helpers.React    


let horseView (horse : Horse, multiplier : Multiplier) = 
    R.div [ ClassName "card" ]
        [
            R.div [ ClassName "card-body" ]
                [
                    R.h3 [] [ R.str horse.Name ]
                    R.div [] [ R.str (sprintf "赔率 1:%i" multiplier) ]
                    R.button [] [ R.str "下注" ]
                ]
        ]

let view dispatch model = 
    R.div [ ClassName "container" ]
        [
            R.div [ ClassName "row" ] [
                R.span [] [ R.str (sprintf "第 %i 期" model.Round) ]
            ]            
            R.div [ ClassName "row" ] (model.Odds |> List.map horseView)
            R.div [ ClassName "row" ] [
                R.button [ OnClick (fun _ -> dispatch Race) ] [ R.str "开跑" ]
            ]
        ]

open Elmish.React

Program.mkSimple init update view
|> Program.withConsoleTrace
|> Program.withReact "fable-hero"
|> Program.run

