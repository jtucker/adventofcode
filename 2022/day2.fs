module codingforbeer.Aoc2022.Day2

open System.IO
open System

type Win =
    | Player
    | Opponent
    | Draw
    static member GetWin letter =
        match letter with
        | "X" -> Opponent
        | "Y" -> Draw
        | "Z" -> Player
        | _ -> failwith "unknown letter"

type Shape =
    |Rock
    |Paper
    |Scissors
    static member GetShape letter =
        match letter with
        | "A" | "X" -> Rock
        | "B" | "Y" -> Paper
        | "C" | "Z" -> Scissors
        | _ -> failwith "unknown letter"

let getShapeValue shape =
    match shape with
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

let getWinValue win =
    match win with 
    | Player -> 6
    | Opponent -> 0
    | Draw -> 3

let findWinner player opp =
        match (player, opp) with
        | Rock, Paper -> Opponent
        | Rock, Scissors -> Player
        | Paper, Rock -> Player
        | Paper, Scissors -> Opponent
        | Scissors, Paper -> Player
        | Scissors, Rock -> Opponent
        | _, _ -> Draw

let calculateScore player win =
    let shapeScore = getShapeValue player
    let winValue = getWinValue win
    shapeScore + winValue

let calculateTotalScore (data: string array) =
    data
    |> Array.map (fun x -> x.Split(" ", StringSplitOptions.RemoveEmptyEntries))
    |> Array.map(fun x -> (Shape.GetShape x[0], Shape.GetShape x[1]))
    |> Array.map(fun (opponent, player) -> (player, findWinner player opponent))
    |> Array.sumBy (fun (player, win) -> calculateScore player win)

open Xunit
open Swensen.Unquote

let [<Fact>] ``test data should equal 15`` () =
    let testData = File.ReadAllLines ("./2022/data/day2-test.txt") |> calculateTotalScore

let actualScore = File.ReadAllLines ("./2022/data/day2.txt") |> calculateTotalScore

let part2calculate (data: string array) =
    data
    |> Array.map (fun x -> x.Split(" ", StringSplitOptions.RemoveEmptyEntries))
    |> Array.map (fun x -> (Shape.GetShape x[0], Win.GetWin x[1]))