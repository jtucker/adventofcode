namespace codingforbeer

open System

[<AutoOpen>]
module common =

    let splitOn (sep: string) (s: string) =
        s.Split([|sep|], StringSplitOptions.RemoveEmptyEntries)

    let loadData (path: string) =
        System.IO.File.ReadAllText(path)