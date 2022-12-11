module codingforbeer.Aoc2022.Day3

open System.IO
open System

let testData = 
    File.ReadAllLines("./2022/data/day3-test.txt")

let getPriority c = 
    let priorities = List.concat [['a'..'z']; ['A'..'Z']]
    List.findIndex (fun x -> x = c) priorities |> (+) 1

let calculateRucksack (s: string) =
    let middle = s.Length / 2
    let left = s[0..middle - 1] |> Seq.toList
    let right = s[middle..] |> Seq.toList
    seq {
        left
        right
    }

let calculate f group =
    group    
    |> Array.map f
    |> Seq.collect id
    |> Seq.map Set.ofList
    |> Set.intersectMany
    |> Set.map getPriority
    |> Set.toSeq 
    |> Seq.head

let data =
    File.ReadAllLines("./2022/data/day3.txt")

let part1 data =
    data
    |> Array.splitInto data.Length
    |> Array.map (calculate calculateRucksack)
    |> Array.sum
    |> printfn "Day 3, Part 1: %d"

let part2 data =
    data
    |> Array.splitInto (data.Length / 3)
    |> Array.map (calculate (fun x -> x |> Array.map Set.ofArray |> Set.intersectMany |> Set.toSeq))

data 
|> Array.splitInto (data.Length / 3)
|> Array.map (calculate calculateRucksack) 
|> Array.sum