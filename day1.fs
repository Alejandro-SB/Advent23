module Day1

open System
open System.IO

let handle () = 
    let inline charToInt c = int c - int '0'
    let numbersInLine s =
        Seq.filter Char.IsDigit s |> Seq.map (fun c -> charToInt c)
    let pairFromLine (d: int list)  =
        match d with
        | [] -> (0,0)
        | [x] -> (x,x)
        | head:: tail -> (head, List.last(tail))
    let lines = File.ReadLines("inputs/day1.txt")
    let items = lines |> Seq.map numbersInLine |> Seq.map Seq.toList |> Seq.map pairFromLine |> Seq.toList

    items |> List.map (fun (a,b) -> a*10+b) |> List.sum |> printf "%d"

