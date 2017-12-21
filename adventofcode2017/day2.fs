module AdventOfCode.Day2
open Xunit
open System.IO

let calculatePair (l, s) curr =
   let x = match l with 
            | _ when curr > l -> curr
            | _ -> l
   let y = match s with 
            | _ when curr < s -> curr
            | _ -> s
   (x, y)

let distance (x, y) = x - y
let convertToInt i = i |> int
let min = System.Int32.MinValue
let max = System.Int32.MaxValue

let getLineDistance (line:string) =
    line.Split() |> 
    Seq.map convertToInt |> 
    Seq.fold calculatePair (min, max) |> 
    distance 

let solveChecksum filename = 
    Seq.sumBy getLineDistance (File.ReadAllLines ("C:\\code\\adventofcode2017\\adventofcode2017\\assets\\" + filename))

[<Theory>]
[<InlineData("day2_theory.txt", 18)>]
let ``should solve checksum`` (input, output) =
    let actual = solveChecksum input
    Assert.Equal<int>(output, actual)

