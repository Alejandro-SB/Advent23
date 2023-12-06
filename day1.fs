module Day1

open System
open System.IO
open System.Text.RegularExpressions

let lines () = File.ReadLines "inputs/day1.txt"

let part1 () = 
    let inline charToInt c = int c - int '0'
    let numbersInLine s =
        Seq.filter Char.IsDigit s |> Seq.map (fun c -> charToInt c)
    let pairFromLine (d: int list)  =
        match d with
        | [] -> (0,0)
        | [x] -> (x,x)
        | head:: tail -> (head, List.last(tail))
    let items = lines() |> Seq.map numbersInLine |> Seq.map Seq.toList |> Seq.map pairFromLine |> Seq.toList

    items |> List.map (fun (a,b) -> a*10+b) |> List.sum |> printf "%d"

let part2  () = 
    let regex = Regex(@"(?=(\d|one|two|three|four|five|six|seven|eight|nine))")
    let matchToNumber (s:string) = 
        if s.Length = 1 then 
            int(s)
        else
            match s with
            | "one" -> 1
            | "two" -> 2
            | "three" -> 3
            | "four" -> 4
            | "five" -> 5
            | "six" -> 6
            | "seven" -> 7
            | "eight" -> 8
            | "nine" -> 9
            | "" -> 0
            | _ -> invalidOp("FAIL")
    let lineToNumbers line =
        let matches = regex.Matches(line) |> List.ofSeq 
        let groupValues = matches |> List.map (fun m -> matchToNumber m.Groups[1].Value)

        match groupValues with
        |[] -> (0,0)
        | [x] -> (x,x)
        | head ::tail -> (head, List.last(tail))

    let lineNumbers = lines() |> Seq.toList |> List.map lineToNumbers
    let lineSum = lineNumbers |> List.map (fun (a,b) -> a*10 + b) |> List.sum
    lineSum |>printfn "%d"
        