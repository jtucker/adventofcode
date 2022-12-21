module codingforbeer.AoC2022.Day5

open System
open System.IO

open FParsec

let data = File.ReadAllLines "./2022/data/day5.txt"

type Step = int * int * int

let moves = 
    let str s = pstring s .>> spaces
    let betweenStrings s1 s2 p = str s1 >>. p .>> str s2
    let move = str "move" >>. (pint32 .>> spaces)
    let from = str "from" >>. (pint32 .>> spaces)
    let goTo = str "to" >>. (pint32 .>> spaces)

    let moveCommand = pipe5 move spaces from spaces goTo (fun m  _  f  _ t -> Step (m, f - 1, t - 1))

    data
    |> Array.skipWhile(fun x -> not (x.StartsWith("move")))
    |> Array.map (fun x -> run moveCommand x)
    |> Array.map (function | Success(x, _, _) -> x | _ -> failwith "error")

let stacks = 
    let generateStacks lineItem =
        let trimString (s: string) = s.Trim()
        lineItem
        |> Seq.chunkBySize 4 
        |> Seq.map (Seq.item 1)
        |> Seq.toList

    data 
    |> Array.takeWhile (fun x -> not (x.StartsWith(" 1")))
    |> Array.map generateStacks
    |> List.transpose
    |> List.map (List.filter Char.IsLetter)

let mover orderer stacks moves =
    let move (stacks: char list list) (step: Step) =
        let (count, from, dest) = step
        let (h, t) = List.splitAt count stacks.[from]
        stacks |> List.updateAt dest ((h |> orderer) @ stacks[dest]) |> List.updateAt from t

    Array.fold move stacks moves
    |> List.map List.head
    |> Array.ofList 
    |> String


let part1 = mover List.rev stacks moves
let part2 = mover id stacks moves