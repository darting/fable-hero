module FableHero.Types


type HorseID = int 

type Horse = {
    ID : HorseID
    Name : string
}

type Multiplier = int

type Odds = (Horse * Multiplier) list

type Model = {
    Round : int    
    Result : Horse list
    Odds : Odds
}

type Msg = 
    | Race
    | Bet of HorseID * decimal


let horses =  [
        { ID = 0; Name = "绝地" }
        { ID = 1; Name = "翻羽" }
        { ID = 2; Name = "越影" }
        { ID = 3; Name = "奔霄" }
        { ID = 4; Name = "逾辉" }
        { ID = 5; Name = "超光" }
        { ID = 6; Name = "腾雾" }
        { ID = 7; Name = "挟翼" }
    ]    

let pays = [ 10; 20; 30; 80; 125; 250; 500; 1000 ]

let rnd = System.Random()

let shuffle (items : List<'T>) = List.sortBy (fun _ -> rnd.NextDouble()) items

let odds horses : Odds =
    List.zip horses (shuffle pays)

let init () : Model = { Round = 1; Result = horses; Odds = odds horses }

let race () = shuffle horses


