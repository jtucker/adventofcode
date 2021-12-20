#r "nuget: Unquote"
open Swensen.Unquote

let testData = "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2"

let splitOn (c: string) (s: string) = s.Split(Seq.toArray c, System.StringSplitOptions.RemoveEmptyEntries)
let toTuple (pair: string array) = (pair[0], pair[1])    
let getMatch (x1, y1, x2, y2)=
    let forwardReverse a b = if a < b then 1 else -1
    if x1 = x2 then 
        [| for i in y1..(forwardReverse y1 y2)..y2 do x1, i |]
    else if y1 = y2 then
        [| for i in x1..(forwardReverse x1 x2)..x2 do i, y1 |]
    else
        Array.zip [| x1..(forwardReverse x1 x2)..x2 |] [|y1..(forwardReverse y1 y2)..y2|]

let getOverlappingPoints filter data =
    data 
        |> splitOn System.Environment.NewLine
        |> Array.map ((splitOn ", ->") >> Array.map (int) >> (fun i -> i[0], i[1], i[2], i[3]))
        |> Array.filter filter
        |> Array.collect getMatch
        |> Array.groupBy id
        |> Array.filter (fun (_, l) -> Array.length l > 1)
        |> Array.length 

let part1Filter (x1, y1, x2, y2) = (x1 = x2 && y1 <> y2) || (x1 <> x2 && y1 = y2)

test <@ getOverlappingPoints part1Filter testData = 5 @>
test <@ getOverlappingPoints (fun _ -> true) testData = 12 @>

open System.IO
let input = File.ReadAllText "./data/day5.txt"

getOverlappingPoints part1Filter input
getOverlappingPoints (fun _ -> true) input