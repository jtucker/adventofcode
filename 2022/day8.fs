module codingforbeer.Aoc2022.Day7

open System
open codingforbeer.common

let testData = "30373
25512
65332
33549
35390"

type Coord = Coord of int * int

let sideViews (m: 'a [,]) (Coord (c, r)) =
    [ m[0 .. r - 1, c] |> Array.rev   // above (in reverse order to mimic walking away)
      m[r, c + 1 ..]                  // right
      m[r + 1 .., c]                  // below
      m[r, 0 .. c - 1] |> Array.rev ] // left (in reverse order to mimic walking away)

let treeVisible arr pos height =
    not <| List.forall (Array.exists ((<=) height)) (sideViews arr pos)

let createGrid f arr = 
    seq {
        for row in 0 .. Array2D.length1 arr - 1 do
            for col in 0 .. Array2D.length2 arr - 1 -> f arr (Coord(col, row)) arr[row, col]
    }

let parseInput f (input: string) =
    input
    |> splitOn Environment.NewLine
    |> (array2D >> Array2D.map (string >> int))
    |> createGrid f

let calculateScore arr pos height=
    sideViews arr pos
    |> List.map (fun x -> x |> Seq.tryFindIndex ((<=) height) |> Option.map ((+)1) |> Option.defaultValue (Array.length x))
    |> List.reduce (*)
    
    
let part1 = loadData "2022/data/day8.txt" 
                    |> parseInput treeVisible
                    |> Seq.filter id
                    |> Seq.length

let part2 = loadData "2022/data/day8.txt" 
                    |> ((parseInput calculateScore) >> Seq.max)

