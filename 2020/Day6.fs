module codingforbeer.AdventOfCode.Day6
open System

let responses = fileText @"2020\assets\day6.txt"

let yesAnswerCounts (group: string) =
    let isLetter = function
        | '\013' | '\010' -> false
        | _ -> true

    group.Split(Environment.NewLine + Environment.NewLine, StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
    |> Array.map(fun s -> s |> Seq.toList |> List.distinct |> List.filter isLetter |> List.length)

printfn "Sum total of groups yes answers: %i" (yesAnswerCounts responses |> Array.sum)