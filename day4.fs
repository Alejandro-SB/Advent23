module day4
open System.IO
open System.Text.RegularExpressions
open System

type Card = {
    id: int;
    winningNumbers: int list;
    playingNumbers: int list;
}

let run () = 
    let cardNumber s = 
        Regex("Card\s+(\d+)").Match(s).Groups[1].Value
        |> int
    let numbers (s:string)  = 
        s.Split(' ')
        |> Array.map _.Trim()
        |> Array.filter (fun x -> x <> String.Empty)
        |> Array.map int
        |> Array.toList

    let cardPartsToCard (parts: string array) = 
        let numberParts = parts[1].Split('|')
        let cardId = cardNumber parts[0]
        let winningNumbers = numbers numberParts[0]
        let playingNumbers = numbers numberParts[1]

        {
            id = cardId; 
            winningNumbers = winningNumbers; 
            playingNumbers = playingNumbers
        }
        
    let cards = 
        File.ReadAllLines "inputs/day4.txt" 
        |> Array.map _.Split(':')
        |> Array.map cardPartsToCard            

    let cardScore card = 
        let winners = 
            card.playingNumbers 
            |> List.filter (fun n -> List.contains n card.winningNumbers)
            |> List.length
        if winners > 0 then pown(2) (winners - 1) else 0
    
    cards 
    |> Array.map cardScore
    |> Array.sum
    |> printfn "%d"
