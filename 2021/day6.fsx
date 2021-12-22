open System


let rec getLanternCountForDay days (state: int array) =
    if days = 0 then
        Array.length state
    else 
        let minusOneOrReset a = Array.map (fun x -> if x = 0 then 6 else x - 1) a
        let getZeroCount a = a |> Array.filter (fun f -> f = 0) |> Array.length, a
        let appendNewLanterns = 
            function 
                | (0, arr) -> arr
                | (zeros, arr) -> Array.concat [| arr ; (Array.create zeros 8) |]
        
        getLanternCountForDay (days-1) (state 
                                |> getZeroCount
                                |> (fun (zeros, arr) -> zeros, minusOneOrReset arr)
                                |> appendNewLanterns)

let groupFish input =
    input |> List.groupBy id |> List.map (fun (day, c) -> day, List.length c |> uint64)

let rec getLanternGroupByDay days state = 
    if days = 0 then
        state
    else
        let minusOneOrReset x = if x = 0 then 6 else x - 1
        let zeroCount = 
            match state |> List.tryFind (fst >> (=) 0) with 
            | None -> 0UL
            | Some (_, c) -> c

        let updatedDays = state |> List.map (fun (d, c) -> (minusOneOrReset d, c))
        let combine = List.groupBy fst >> List.map (fun (d, c) -> (d, List.sumBy snd c))

        getLanternGroupByDay (days - 1) <| combine updatedDays @ [(8, zeroCount)]


open System.IO

let input = File.ReadAllText "./data/day6.txt"
let splitComma (s:string) = s.Split(",", StringSplitOptions.RemoveEmptyEntries)
let splitAndConvertToIntList (a: string) = (splitComma >> Array.map int >> List.ofArray) a
let inputInts = input |> splitAndConvertToIntList

getLanternCountForDay 80 (input |> (splitComma >> Array.map int))
getLanternGroupByDay 256 (groupFish inputInts) |> List.sumBy snd