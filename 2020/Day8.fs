module codingforbeer.AdventOfCode.Day8

open System
open FParsec

let demo = "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6"

type State = {
    Acc: int
    CurrIndex: int
    Visited: int list
}
let state = { Acc = 0; CurrIndex = 0; Visited = []; }

let check state cmd = 
    let action, v = cmd
    match action with
    | "acc" -> { state with Acc = (state.Acc + (int32 v)); Visited = (state.Visited @ [state.CurrIndex]) }
    | "jmp" -> { state with CurrIndex = (int32 v); Visited = (state.Visited @ [state.CurrIndex])}
    | _ -> state

let parseCmd cmd =
    let actionParser = choice [
        pstring "acc"
        pstring "jmp"
        pstring "nop"
    ]
    let parser = sepBy (actionParser .>> spaces1 .>>. pint32) newline
    match run parser cmd with
    | Success (v, _, _) -> v |> List.mapi ( fun i (action, value) -> (i, action, value))
    | Failure _ -> failwith "parsing error"

demo.Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries) 
|> Array.map parseCmd