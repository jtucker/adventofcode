open System.IO

let mutable inputData = 
    File.ReadAllText("inputdata\\day2.txt").Split(',')
    |> Array.map int

type ProcessedData = 
    {
        data : int array        
    }

let initial = 
    {
        data = inputData
    }

let updateState state operation x y p =
    state.data.[p] <- operation state.data.[x] state.data.[y]
    state

let folder state opcode =
    match opcode |> Array.toList with
    | x :: xs when x = 1 && xs.Length = 3 -> updateState state (+) xs.[0] xs.[1] xs.[2]
    | x :: xs when x = 2 && xs.Length = 3 -> updateState state (*) xs.[0] xs.[1] xs.[2]
    | _ -> state

let processed = inputData
                |> Array.chunkBySize 4
                |> Array.fold folder initial

printfn "value at position 0: %i" processed.data.[0]