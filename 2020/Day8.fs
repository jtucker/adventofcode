module codingforbeer.AdventOfCode.Day8

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

type Action = 
    | Accumulator
    | Jump
    | NoOperation

type Execution =
    | Duplicate of int
    | InfiniteLoop
    | Exited of int

type State = {
    Acc: int
    PreviousIndex: int
    CurrIndex: int
    Visited: int list
}

let state = { Acc = 0; PreviousIndex = 0; CurrIndex = 0; Visited = List.empty; }

let parseCmd cmd =
    let actionParser = choice [
        stringReturn "acc" Accumulator
        stringReturn "jmp" Jump
        stringReturn "nop" NoOperation
    ]
    let parser = sepBy (actionParser .>> spaces1 .>>. pint32) newline
    match run parser cmd with
    | Success (v, _, _) -> v |> List.mapi (fun i (action, value) -> (i, action, value))
    | Failure _ -> failwith "parsing error"

let rec getAccumulator (state: State) (actions: (int * Action * int) list) =
    if state.CurrIndex = actions.Length then Exited state.Acc 
    else
        let nextIndex = state.CurrIndex + 1
        let idx, action, value = actions.[state.CurrIndex]
        
        match action with
        | _ when List.contains state.CurrIndex state.Visited -> Duplicate state.Acc
        | Jump when state.CurrIndex = (state.CurrIndex + value) -> InfiniteLoop
        | NoOperation -> getAccumulator { state with Visited = state.Visited @ [idx]; CurrIndex = nextIndex; PreviousIndex = state.CurrIndex } actions 
        | Accumulator -> getAccumulator { state with Acc = state.Acc + value; CurrIndex = nextIndex; Visited = state.Visited @ [idx]; PreviousIndex = state.CurrIndex } actions 
        | Jump -> getAccumulator { state with CurrIndex = state.CurrIndex + value; Visited = state.Visited @ [idx]; PreviousIndex = state.CurrIndex } actions

let getExecution (actions: (int * Action * int) list) =
    let listOfJmpNop (actions: (int * Action * int) list) =
        let filter item = 
            let _, action, _ = item
            match action with
            | NoOperation | Jump -> true
            | _ -> false    
        actions |> List.filter filter

    let updateList (actions: (int * Action * int) list) (idx, action, value) =
        let flipAction = function
            | NoOperation -> Jump
            | Jump -> NoOperation
            | Accumulator -> Accumulator
        actions.[0..idx - 1] @ [(idx, flipAction action, value)] @ actions.[idx + 1..]

    listOfJmpNop actions
    |> List.map ((updateList actions) >> (getAccumulator state)) 
    |> List.filter(function | Exited _ -> true | _ -> false);;