open System.IO

let inputData = 
    File.ReadAllText("adventofcode2019\\inputdata\\day2.txt").Split(',')
    |> Array.map int

type OpCodes = 
    | Addition = 1
    | Multiplication = 2
    | EndProgram = 99

Array.splitInto