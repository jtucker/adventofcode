[<AutoOpen>]
module codingforbeer.AdventOfCode.AssetLoader

open System.IO

let fileContents path =
    try
        seq {
            let reader = new StreamReader(File.OpenRead path)        
            while( not reader.EndOfStream ) do 
                yield reader.ReadLine()
        }
    with 
        | :? FileNotFoundException -> sprintf "File path (%s) not found." path |> ignore; Seq.empty