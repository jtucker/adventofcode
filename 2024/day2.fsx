let testInput = """7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9"""

let inOrder (report: int array) =
    let ascendingReport = Array.sort report
    let descendingReport = Array.sortDescending report
    (ascendingReport = report) || (descendingReport = report)

let inRange (report: int array) =
    report
    |> Array.pairwise
    |> Array.map (fun (first, second) -> abs(first - second))
    |> Array.forall (fun diff -> (diff > 0) && (diff <= 3))  

let isSafeReport (report: int array) =
    (inOrder report) && (inRange report)

let reportGenerator (report: int array) =
    seq {
        yield report
        let length = (Array.length report) - 1
        for i in [0 .. length] -> Array.removeAt i report
    }

let isSafeWithTolerance (report: int array) =
    let generatedReports = reportGenerator report
    generatedReports |> Seq.exists isSafeReport

open System.IO
let inputData =
    File.ReadAllLines("./inputs/day2.txt")
    
let runner (processor: int array -> bool) (data: string array) =
    data
    |> Array.map (fun x -> x.Split(" ") |> Array.map int)
    |> Array.filter processor
    |> Array.length

printfn $"part 1: {runner isSafeReport inputData}"
printfn $"part 2: {runner isSafeWithTolerance inputData}"    
