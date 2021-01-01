module codingforbeer.AdventOfCode.Day9
open System

let demo = "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576"

let numbers = fileText @"/workspaces/adventofcode/2020/assets/day9.txt"
let hasSumOfTwo input windowCount =
    let windowList (input: string) count = 
        input.Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries) 
        |> Array.toList
        |> List.map (float)
        |> List.windowed count
    
    let combinationOf (l: float list) =
        let removeFromListAtIndex idx = l.[..idx-1] @ l.[idx+1..]
        let combineWithList item list = List.map(fun z -> (item, z)) list
        l 
        |> List.mapi (fun idx item -> (removeFromListAtIndex idx) |> combineWithList item) 
        |> List.collect (id)
        
    let mapToCombos l = (List.last l, (combinationOf l))
    let valueFound (checkFor, comboList) =
        match List.tryFind (fun (x, y) -> x + y = checkFor) comboList with
        | Some _ -> false
        | None -> true

    windowList input windowCount 
        |> List.tail 
        |> List.map mapToCombos
        |> List.filter valueFound
        |> List.head
        |> fst
