module Day2
open System.Text.RegularExpressions
open System.IO

type Turn = {
    red: int;
    green: int;
    blue: int;
}

type Game = {
    id: int;
    turns: Turn list
}

let maxRed = 12
let maxGreen = 13
let maxBlue = 14

let gameRegex = Regex(@"Game (\d+)");
let colorRegex color = Regex($@"(\d+) {color}")
let redRegex = Regex(@"(\d+) red");
let greenRegex = Regex(@"(\d+) green")
let blueRegex = Regex(@"(\d+) blue");

let getGameList () = 
    File.ReadAllLines "inputs/day2.txt" 

let getGameId (s) =
    gameRegex.Match(s).Groups[1].Value |> int

let part1 () =
    let isValidTurn (turn: Turn) = turn.blue <= maxBlue && turn.red <= maxRed && turn.green <= maxGreen
    let isValidGame (game: Game) =
        game.turns |> List.forall isValidTurn
    let countColor (color:string,  s:string) = 
        let colorMatch = colorRegex(color).Match s
        if colorMatch.Success then colorMatch.Groups[1].Value |> int else 0
    let grabs (s: string) =
        s.Split(";") |> Array.map (fun s -> {
            red = countColor("red", s)
            green = countColor("green", s)
            blue = countColor("blue", s)
        }) |> Array.toList

    let gameList = getGameList()

    let games = 
        gameList      
        |> Seq.map (fun line -> line.Split(':'))
        |> Seq.map (fun parts -> {
            id = getGameId parts[0]
            turns = grabs(parts[1])
        })
        |> Seq.toList

    let validGames = games |> Seq.filter isValidGame
    let validGamesIds = validGames |> Seq.map _.id
    let idsSum = validGamesIds |> Seq.sum

    idsSum |> printfn "%d"