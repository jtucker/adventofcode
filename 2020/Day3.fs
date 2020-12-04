module codingforbeer.AdventOfCode.Day3

type Position = {
    X: int
    Y: int
}

let assetContent = fileContents @"2020\assets\day3.txt" |> List.ofSeq
let startPosition = { X = 0; Y = 0 }

let movePosition position =
    { position with X = position.X + 3; Y = position.Y + 1 }

let isTree =
    function | '#' -> 1
             | _ -> 0

let folder (p, t) (l: string) =
    System.Console.WriteLine (sprintf "%A" p)
    (movePosition p, t + (isTree l.[p.X % 31]))

let _, treeCount = assetContent |> List.fold (folder) (startPosition, 0)
System.Console.WriteLine ( sprintf "You would hit %i trees" treeCount )
