module AOC2023.Day1

open System
open Common

let textInts = [
    ("1", 1)
    ("2", 2)
    ("3", 3)
    ("4", 4)
    ("5", 5)
    ("6", 6)
    ("7", 7)
    ("8", 8)
    ("9", 9)
    ("one", 1)
    ("two", 2)
    ("three", 3)
    ("four", 4)
    ("five", 5)
    ("six", 6)
    ("seven", 7)
    ("eight", 8)
    ("nine", 9)
]

let getWordsAndInts (input: string) =
   let first = (textInts |> List.choose (FirstLastIndexBy.indexBy input) |> List.minBy fst) |> snd
   let last = (textInts |> List.choose (FirstLastIndexBy.lastIndexBy input) |> List.maxBy fst) |> snd
   $"{first}{last}" |> int
   
let getInts (input : string) =
    let ints = input
            |> Seq.map (string >> Int32.TryParse)
            |> Seq.filter fst
            |> Seq.map snd
            |> Seq.toList
    $"{List.item 0 ints}{List.last ints}" |> int

let part1 (inputs : string[]) =
    inputs
    |> Array.map getInts
    |> Array.sum
    
let part2 (inputs : string[]) =
    inputs
    |> Array.map getWordsAndInts
    |> Array.sum
    
let run () =
    let inputs = readInput "day1.txt"
    $"Day 1, Part 1: {part1 inputs}; Day 1, Part 2: {part2 inputs}"
