module codingforbeer.AdventOfCode.Day3

type Position = {
    First: int
    Second: int
}

let arrContent = fileContents @"2020\assets\day3.txt" |> Array.ofSeq |> array2D
let slopes = [(1, 1); (3, 1); (5, 1); (7, 1); (1, 2)]
let startState = ({ First = 0; Second = 0 }, 0)

let movePosition slope position maxLength =
    let (right, down) = slope
    match position with
    | p when p.First + down > maxLength -> None
    | _ -> Some ({ position with First = position.First + down; Second = position.Second + right })

let isTree =
    function | '#' -> 1
             | _ -> 0

let rec arrFold slope state (array: char [,]) =
    let (position: Position), (trees:int) = state
    match movePosition slope position (Array2D.length1 array) with
    | None -> state
    | Some (p) -> arrFold slope (p, trees + (isTree array.[position.First, position.Second % 31])) array

let calcTrees =
    let treeMapper = (fun s -> arrFold s startState arrContent) >> (fun (p, trees) -> int64 trees)
    slopes |> List.map (treeMapper) |> List.reduce ((*))
