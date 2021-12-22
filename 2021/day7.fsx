open System

let testdata = "16,1,2,0,4,2,7,1,2,14"

let splitComma (s:string) = s.Split(",", StringSplitOptions.RemoveEmptyEntries)
       
let calculateFuel (list: int list) =
   
    let calculateCost x y = if x < y then y - x else x - y
    let sortedInput = list |> List.sort
    let getIndex = if sortedInput.Length % 2 = 0 then sortedInput.Length / 2 else (sortedInput.Length + 1) / 2
    let position = sortedInput[getIndex]

    sortedInput |> List.sumBy (calculateCost position)

let calculateFuel2 (list: float list) = 
    let median = Seq.ofList list |> Seq.average
    let topIndex = Math.Ceiling median 
    let bottomIndex = Math.Floor median

    let calculateCost x y = 
        let diff = if x < y then y - x else x - y
        List.sum [1.0 .. diff]

    let firstFuel = List.sumBy (calculateCost topIndex) list
    let sndFuel = List.sumBy (calculateCost bottomIndex) list

    if firstFuel < sndFuel then firstFuel else sndFuel


open System.IO

let inputdata = File.ReadAllText "./data/day7.txt"
let inputlist = inputdata |> splitComma |> List.ofArray
calculateFuel ( inputlist |> List.map int)

calculateFuel2 ( inputlist |> List.map float)
