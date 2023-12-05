module AOC2023.Day2
open Common
open FParsec

type CubeType = 
    | Red
    | Green
    | Blue
    
type Cube = {
    Count: int
    Type: CubeType
}

type ElfGame = {
    Number : int
    Cubes: Cube list
}

let parseGame =
    let gameId = skipStringCI "Game" >>. spaces >>. pint32 .>> pstring ":" .>> spaces
    let cubeType = choice [
        (skipStringCI "blue" >>% Blue)
        (skipStringCI "red" >>% Red)
        (skipStringCI "green" >>% Green)
    ]
    let cube = pint32 .>> spaces1 .>>. cubeType
    let splitCube = sepBy1 cube (pstring "; " <|> pstring ", ")
                    |>> fun cubes -> cubes |> List.groupBy snd |> List.map (fun (cubeType, cubes) -> cubes |> List.sumBy fst |> fun count -> {Count = count; Type = cubeType})
    pipe2 gameId splitCube (fun id cubes -> {Number = id; Cubes = cubes})
    
let part1 (inputs: string array) : string =
    inputs
    |> Array.map (run parseGame)

    
let run() =
    let inputs = readInput "day2.txt"
    $"Day 2 Part 1: {part1 inputs}"