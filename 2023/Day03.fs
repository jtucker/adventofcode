module AOC2023.Day3

open System
open System.Text.RegularExpressions

type FoundValue = { Value: string; X: int; Y: int }
type NumberPosition = { StartX: int; EndX: int; Y: int }
type SymbolPosition = { X: int; Y: int }
type NumberValue = { Value: int; Position: NumberPosition }
type SymbolValue = { Value: char; Position: SymbolPosition }

let (|Number|_|) (s: string) =
    match Int32.TryParse s with
    | true, number -> Some number
    | false, _ -> None

let (|Char|_|) (s: string) =
    match s.Length with
    | 1 -> Some s[0]
    | _ -> None

let partitionMap partitioner =
    let rec loop (acc1, acc2) =
        function
        | [] -> List.rev acc1, List.rev acc2
        | x :: xs ->
            match partitioner x with
            | Choice1Of2 y -> loop (y :: acc1, acc2) xs
            | Choice2Of2 y -> loop (acc1, y :: acc2) xs

    loop ([], [])

let partitioner (row: FoundValue) =
    match row.Value with
    | Number n ->
        Choice1Of2({ 
            NumberValue.Value = n
            Position = {
                StartX = row.X
                EndX = row.X + row.Value.Length - 1
                Y = row.Y
            }
         })
    | Char c ->
        Choice2Of2(
            { Value = c
              Position = { X = row.X; Y = row.Y } })
    | _ -> failwith "Invalid input"

let getSymbolLocations (input: string array) =
    input
    |> Array.indexed
    |> Array.collect (fun (idx, line) ->
        Regex.Matches(line, "\d+|[^.]")
        |> Seq.map (fun m ->
            { X = m.Index
              Y = idx
              Value = m.Value })
        |> Seq.toArray)
    |> Array.toList
    |> partitionMap partitioner

let isAdjacentTo n s = 
    let left = abs (n.StartX - s.X) <= 1
    let right = abs (n.EndX - s.X) <= 1
    let Y = abs (n.Y - s.Y) <= 1

    (left || right) && Y

let getPartNumbers (numbers : NumberValue list, symbols) =
    numbers
    |> List.choose(fun num -> 
        let isPartNumber =
            symbols |> List.exists ( fun s -> s.Position |> isAdjacentTo num.Position)
        if isPartNumber then Some num else None)
    |> List.sumBy (_.Value)

let run () =
    let input = readInput "day3.txt"
    let sum = input 
            |> getSymbolLocations
            |> getPartNumbers

    $"Day3 part 1: {sum}"
