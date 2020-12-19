module codingforbeer.AdventOfCode.Day6
open System

let responses = fileText @"2020\assets\day6.txt"
let blankLineSeperator = Environment.NewLine + Environment.NewLine

let yesAnswerCounts (group: string) =
    let isLetter = function
        | '\013' | '\010' -> false
        | _ -> true
    let getYesAnswerCountForGroup g =
        g |> Seq.toList |> List.distinct |> List.filter isLetter |> List.length

    group.Split(blankLineSeperator, StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
    |> Array.map getYesAnswerCountForGroup

printfn "Sum total of groups yes answers: %i" (yesAnswerCounts responses |> Array.sum)

let getEveryoneYesAnswers (group: string) =
    let getAllSameAnswerForGroup (g: string) =
        g.Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries) 
        |> Array.toList
        |> List.map Set.ofSeq
        |> List.reduce (Set.intersect)
        |> Set.count
        

    group.Split(blankLineSeperator, StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries) 
    |> Array.toList
    |> List.map (getAllSameAnswerForGroup)

printfn "Sum total of groups same answers: %i" (getEveryoneYesAnswers responses |> List.sum)