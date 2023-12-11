module AOC2023.Day5

open System
open Common

let parse line = 
    line

let run () = 
    let input = readInput "Day5.txt"
    let part1 = input |> Array.map parse
    $"Day 5 Part 1 { part1 }"