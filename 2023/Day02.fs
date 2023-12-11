module AOC2023.Day2

open Common
open FParsec

type CubeType =
    | Red
    | Green
    | Blue

type Cube = { Count: int; Type: CubeType }
type ElfGame = { Number: int; Cubes: Cube list }

let parseGame =
    let gameId = skipStringCI "Game" >>. spaces >>. pint32 .>> pstring ":" .>> spaces

    let cubeType =
        choice
            [ (skipStringCI "blue" >>% Blue)
              (skipStringCI "red" >>% Red)
              (skipStringCI "green" >>% Green) ]

    let cube = pint32 .>> spaces1 .>>. cubeType
    let splitCube = sepBy1 cube (pstring "; " <|> pstring ", ")

    let convertToGame id cubes =
        let cubeList =
            List.map (fun (num, cubeType) -> { Count = num; Type = cubeType }) <| cubes

        { Number = id; Cubes = cubeList }

    pipe2 gameId splitCube convertToGame

let getGame parser gameToParse =
    let parsed = run parseGame gameToParse

    match parsed with
    | Success(game, _, _) -> game
    | Failure(err, _, _) -> failwith err

let possible (game: ElfGame) =
    let filterCubes color count cubes =
        cubes
        |> List.filter (fun cube -> cube.Type = color)
        |> List.filter (fun cube -> cube.Count > count)
        |> List.isEmpty // if there ar any left then it's possible

    let red = game.Cubes |> filterCubes Red 12
    let green = game.Cubes |> filterCubes Green 13
    let blue = game.Cubes |> filterCubes Blue 14

    red && green && blue

let powerPossible game =
    let maxColorCubeCount color cubes =
        cubes 
        |> List.filter (fun cube -> cube.Type = color) 
        |> List.maxBy (fun cube -> cube.Count)

    let redCount = maxColorCubeCount Red game.Cubes
    let blueCount = maxColorCubeCount Blue game.Cubes
    let greenCount = maxColorCubeCount Green game.Cubes

    redCount.Count * blueCount.Count * greenCount.Count

let part1 (games) : int =
    games
    |> Array.filter possible
    |> Array.sumBy (fun game -> game.Number)

let part2 (games) : int =
    games
    |> Array.map powerPossible
    |> Array.sum

let run () =
    let inputs = readInput "day2.txt"
    let games = inputs |> Array.map (getGame parseGame)

    $"Day 2 Part 1: {part1 games}; Day 2 Part 2: {part2 games}"