[<AutoOpen>]
module codingforbeer.AdventOfCode.AssetLoader

open System.IO

let fileText path =
    try 
        File.ReadAllText path
    with
        | :? FileNotFoundException -> printf "File path (%s) not found." path; ""

let fileLines path =
    try
        seq {
            let reader = new StreamReader(File.OpenRead path)
            while( not reader.EndOfStream ) do 
                yield reader.ReadLine()
        }
    with 
        | :? FileNotFoundException -> sprintf "File path (%s) not found." path |> ignore; Seq.empty