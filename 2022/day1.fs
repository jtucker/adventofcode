module codingforbeer.Aoc2022.Day1

open System.IO
open System

let openAocTestData day =
    File.ReadAllText $"./2022/data/{day}-test.txt"

let openAocData day =
    File.ReadAllText $"./2022/data/{day}.txt"

let splitString (splitter: string) (stringToSplit: string) =
    stringToSplit.Split(splitter) |> Array.toList

let partitionFor f listOfItems =
    let folder accum item =
        match accum with 
        | [] -> [[item]]
        | current::rest when f item -> [item]::accum
        | current::rest -> (item::current)::rest
    List.fold folder [] listOfItems |> List.map List.rev |> List.rev

let calculateCalories listOfCalories =
    listOfCalories 
    |> splitString Environment.NewLine // ['a', 'b', '', 'c']
    |> partitionFor (fun x -> String.IsNullOrEmpty(x))
    |> List.map (fun x -> x |> List.filter (fun y -> y.Length <> 0) |> List.map int |> List.sum) 
    |> List.mapi (fun idx sum -> (idx + 1, sum))

let mostCalories listOfCalories = 
    calculateCalories listOfCalories |> List.maxBy (fun (i, s) -> s)

let top3Calories listOfCalories =
    calculateCalories listOfCalories
    |> List.sortByDescending (fun (i, s) -> s)
    |> List.take 3
    |> List.sumBy (fun (i, s) -> s)
 
sprintf "Most Calories: %A" (mostCalories (openAocData "day1"))
sprintf "Top 3 sum calories: %A" (top3Calories (openAocData "day1"))
