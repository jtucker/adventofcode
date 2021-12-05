#r "nuget: Unquote"
open Swensen.Unquote
open System

let testData = 
    "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010".Split(System.Environment.NewLine, StringSplitOptions.RemoveEmptyEntries)

let invertcalc (calc:string) =
    let switch = function
        | '0' -> '1'
        | '1' -> '0'
        | x -> x
    Array.fold (fun s i -> s + string (switch i)) "" (calc.ToCharArray ())

let rec getreportbys state index (reports: string array) =    
    if index = reports[0].Length then
        state
    else
        let max = reports |> Array.groupBy (fun i -> i[index]) |> Array.maxBy snd |> fst
        getreportbys (state + (string max)) (index+1) reports
 
let reportResult reports = getreportbys "" 0 reports
let calculateGamma result = Convert.ToInt64(result, 2)
let calculateEpsilon result = invertcalc result |> (fun i -> Convert.ToInt64(i, 2))

test <@ reportResult testData = "10110" @>
test <@ calculateGamma "10110" = 22 @>
test <@ calculateEpsilon "10110" = 9 @>

open System.IO

let fileReports = File.ReadAllLines("./2021/data/day3.txt")
let filereportsresult = reportResult fileReports

let fileGamma = calculateGamma filereportsresult
let fileEpsilon = calculateEpsilon filereportsresult

printfn $"{fileGamma * fileEpsilon}"

(* Part 2 *)
let reportcalculator calc reports = 
    let rec getreportby calc index (reports: string array) =
        match reports with        
        | [|x|] -> [|x|]
        | _ ->  let filtered = reports |> (Array.groupBy (fun i -> i.[index]) >> calc snd >> snd)
                getreportby calc (index + 1) filtered

    let convertToInt i = Convert.ToInt64(i, 2)
    getreportby calc 0 reports |> Array.head |> convertToInt

let calculateO2Gen (reports: string array) : int64 = 
    reportcalculator (Array.maxBy) reports

let calculateCO2Scrub reports : int64 =
    reportcalculator (Array.minBy) reports

test <@ calculateO2Gen testData = 23 @>
test <@ calculateCO2Scrub testData = 10 @>

printfn $"{(calculateO2Gen fileReports) * (calculateCO2Scrub fileReports)}"
