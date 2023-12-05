module AOC2023.Common

open System.IO

let readInput path =
    File.ReadAllLines $"Inputs/{path}"
    
module FirstLastIndexBy =
    let private toOption =
        function
        | x when x >= 0 -> Some x
        | x -> None
    
    let indexBy (line: string) (search: string, value) =
        line.IndexOf(search) |> toOption |> Option.map (fun x -> (x, value))
        
    let lastIndexBy (line: string) (search: string, value) =
        line.LastIndexOf(search) |> toOption |> Option.map (fun x -> (x, value))
        