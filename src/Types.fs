module FableHero.Types


type MaybeBuilder () = 
    member __.Zero () = None
    member __.Bind (m, f) = Option.bind f m
    member __.Return v = Some v
    member __.ReturnFrom m = m

let maybe = MaybeBuilder ()

type User = {
    Name : string
    Balance : decimal
}

type Player = 
    | LoggedIn of User
    | Guest of User

type HorseID = int 

type Horse = {
    ID : HorseID
    Name : string
}

type Multiplier = int

type Odds = (Horse * Multiplier) list

type Page =
    | Login
    | Main

type Game = 
    { RoundID : int
      Result : Horse list
      Bets : (HorseID * decimal * decimal) list // horse * wager * win
      Odds : Odds }

type State = {
    Player : Player
    Current : Game
    History : Game list
    Page : Page
}

type Msg = 
    | UpdateUserName of string
    | SignOut
    | SignIn
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

let newGame = 
    let mutable round = 0
    fun () ->
        round <- round + 1
        { RoundID = round
          Result = []
          Bets = []
          Odds = horses |> shuffle |> odds |> List.sortBy (fun (h,_) -> h.ID) }

let zero () : State = { 
    Player = Guest { Name = ""; Balance = 1000m }
    Current = newGame ()
    History = []
    Page = Login }

let race () = shuffle horses


