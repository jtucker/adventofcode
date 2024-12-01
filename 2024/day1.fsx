open System
open System.IO

let folder (state: (int array * int array)) (pair: string) = 
    let splitPair = pair.Split("   ") |> Array.map int
    (
        Array.append (fst state) [| splitPair[0]|],
        Array.append (snd state) [| splitPair[1]|]
    )

let getDistance (leftArray: int array, rightArray: int array) = 
    Array.zip (leftArray |> Array.sort) (rightArray |> Array.sort)
    |> Array.map (fun (first, second) -> Math.Abs(first - second))

let getCounts (arrayToFind: int array, arrayToMatch: int array) = 
    let counter (item: int) = 
        let timesFound = (Array.filter (fun n -> n = item) arrayToMatch) |> Array.length
        item * timesFound

    arrayToFind 
    |> Array.map counter

let inputData = 
    File.ReadAllLines(("./inputs/day1.txt"))
    |> Array.fold folder (Array.empty, Array.empty)

let runner data (func: (int array * int array) -> int array) =
    data
    |> func
    |> Array.sum

let runnerWithData = runner inputData
let part1 = runnerWithData getDistance
let part2 = runnerWithData getCounts

printfn $"Part 1: {part1}"
printfn $"Part 2: {part2}"