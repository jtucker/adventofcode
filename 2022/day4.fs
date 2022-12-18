module codingforbeer.Aoc2022.Day4

open System
open System.IO

let testData = [|
    "2-4,6-8"; "2-3,4-5"; "5-7,7-9"; "2-8,3-7"; "6-6,4-6"; "2-6,4-8"
|]

let input = File.ReadAllLines "./2022/data/day4.txt"

let splitOn (on: string) (item: string) =
    item.Split (on, StringSplitOptions.RemoveEmptyEntries)

let overlap (pair: int array array) =
    let firstRange = [pair[0][0]..pair[0][1]] |> Set.ofList
    let scondRange = [pair[1][0]..pair[1][1]] |> Set.ofList
    Set.intersect firstRange scondRange |> Set.isEmpty |> not
     
let fullyContain (pair: int array array) =
    pair[0][0] <= pair[1][0] && pair[0][1] >= pair[1][1] 

let getPairs pair =
    splitOn "-" pair |> Array.map int

let part1 data = 
    splitOn "," data
    |> Array.map getPairs
    |> Array.sortBy (fun x -> x[0] - x[1])
    |> fullyContain 

let part2 data =
    splitOn "," data
    |> Array.map getPairs
    |> Array.sortBy (fun x -> x[0] - x[1])
    |> overlap

input
|> Array.map (part1)
|> Array.filter (fun x -> x)
|> Array.length
|> printfn "Part 1: %d"

input
|> Array.map (part2)
|> Array.filter (fun x -> x)
|> Array.length
|> printfn "Part 2: %d"
