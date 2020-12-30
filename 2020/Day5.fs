module codingforbeer.AdventOfCode.Day5

let inputLines = fileLines @"2020\assets\day5.txt"

let getSeatList input = 
    let split x = Seq.toList x |> List.splitAt 7
    let positionFolder (range: int list) half =
        match half with
        | 'F' | 'L' ->  range.[.. range.Length / 2 - 1]
        | 'B' | 'R' -> range.[range.Length / 2 ..]
        | _ -> range

    let seatNumber (row, col) = 
        match row with
        | [x] -> match col with
                 | [y] -> x * 8 + y
                 | _ -> 0
        | _ -> 0

    let getPosition (rows, cols) =
        (rows |> List.fold positionFolder [0..127], cols |> List.fold positionFolder [0..7])
    
    input |> Seq.map (split >> getPosition >> seatNumber)
    
let seatList = getSeatList inputLines
let getHighestSeat =  seatList |> Seq.max
let getLowestSeat = seatList |> Seq.min
let seatRange = [getLowestSeat .. getHighestSeat]
let missing = seatRange |> List.except seatList

printfn "Highest Seat: %i" getHighestSeat
printfn "Missing Seat: %i" (List.head missing)