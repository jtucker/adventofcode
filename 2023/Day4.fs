module AOC2023.Day4

open System
open Common

let calculatePoints (values: int list) =
    Math.Pow(2.0, float (values.Length - 1)) |> int


let parse (card: string) = 
    let intersect (listOfList: int list list) =
        listOfList
        |> List.map Set.ofList
        |> List.reduce Set.intersect
        |> Set.toList

    let values = 
        String.split ':' card
        |> Array.last
        |> String.split '|'
        |> List.ofArray
        |> List.map (fun part -> 
                part.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)
                |> Array.toList
                |> List.map int)

    values |> intersect

(*
    Card 1 -> 4 matches -> 
*)

let test = """
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
"""

let run () =
    let input = readInput "Day4.txt"
    let part1 = input |> Array.map parse |> Array.map calculatePoints |> Array.sum
    $"Day 4 Part 1 { part1 }"