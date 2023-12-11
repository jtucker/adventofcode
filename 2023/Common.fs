[<AutoOpen>]
module AOC2023.Common

open System
open System.IO

let readInput path =
    File.ReadAllLines $"Inputs/{path}"

let readInputAsString path =
    File.ReadAllText path = $"Inputs/{path}"

module FirstLastIndexBy =
    let private toOption =
        function
        | x when x >= 0 -> Some x
        | x -> None
    
    let indexBy (line: string) (search: string, value) =
        line.IndexOf(search) |> toOption |> Option.map (fun x -> (x, value))
        
    let lastIndexBy (line: string) (search: string, value) =
        line.LastIndexOf(search) |> toOption |> Option.map (fun x -> (x, value))
        
module String =
    let splitRows (s: string) = 
        s.Split([| "\n"; Environment.NewLine |], StringSplitOptions.RemoveEmptyEntries)
    let split (c: char) (s: string) = 
        s.Split([| c |], StringSplitOptions.RemoveEmptyEntries)
    let trim (c: char) (s: string) = 
        s.Trim([| c |])
    let toInt (s: string) = 
        Int32.Parse(s)