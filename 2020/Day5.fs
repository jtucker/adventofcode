module codingforbeer.AdventOfCode.Day5

let inputLines = fileLines @"2020\assets\day5.txt"

let getHighestSeat input = 
    let split x = Seq.toList x |> List.splitAt 7
    let getPosition (range: int list) half =
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
    
    (Seq.map (split >> ((fun (r, c) -> ((List.fold getPosition [0..127] r), (List.fold getPosition [0..7] c))) >> (seatNumber))) input)
    |> Seq.max

printfn "Highest Seat: %i" (getHighestSeat inputLines)