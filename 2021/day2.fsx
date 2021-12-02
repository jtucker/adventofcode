#r "nuget: Unquote"

open System
open Swensen.Unquote


let testCommands =
    [| "forward 5"; "down 5"; "forward 8"; "up 3"; "down 8"; "forward 2"; |]

let collect (horizontal, vertical) (cmd: string) =
    match cmd.Split([|' '|], StringSplitOptions.RemoveEmptyEntries) with
    | [|cmdname; value;|] when cmdname = "forward" -> (horizontal + int value, vertical)
    | [|cmdname; value;|] when cmdname = "down" -> (horizontal, vertical + int value)
    | [|cmdname; value;|] when cmdname = "up" -> (horizontal, vertical - int value)
    | _ -> failwith "bad command"
let multiplyTuple (x, y) = x * y

test <@ Array.fold collect (0,0) testCommands |> multiplyTuple = 150 @>

open System.IO

let fileCommands = File.ReadAllLines("./data/day2.txt")
printfn $"{ Array.fold collect (0, 0) fileCommands |> multiplyTuple}"