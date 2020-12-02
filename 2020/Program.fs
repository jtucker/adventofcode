
module codingforbeer.AdventOfCode.Main

open System
open Argu

let colorizer = function
    | ErrorCode.HelpText -> None
    | _ -> Some ConsoleColor.Red

[<EntryPoint>]
let main argv =
    let errorHandler = ProcessExiter (colorizer)
    let parser = ArgumentParser.Create<AdventArguments> (programName = "advent", errorHandler = errorHandler)
    
    let results = parser.ParseCommandLine argv

    printfn "Got results %A" <| results.GetAllResults ()
    0