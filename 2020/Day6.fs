module codingforbeer.AdventOfCode.Day6

open System

let responses = fileText @"2020\assets\day6.txt"

let getAllYesAnswers items =
    items |> (Array.collect (Seq.toArray) >> Array.distinct >> Array.length)

let getAnswerCounts counter group = 
    let splitter (splitOn: string) (str: string) = str.Split(splitOn, StringSplitOptions.RemoveEmptyEntries);
    let singleSplitter = splitter Environment.NewLine
    let doubleSplitter = splitter (Environment.NewLine + Environment.NewLine)

    Array.map (singleSplitter >> counter) (group |> doubleSplitter)

printfn "Sum total of groups yes answers: %i" (getAnswerCounts getAllYesAnswers responses |> Array.sum)

let getEveryoneYesAnswers items =
    items |> (Array.map Set.ofSeq >> Array.reduce Set.intersect >> Set.count)

printfn "Sum total of groups same answers: %i" (getAnswerCounts getEveryoneYesAnswers responses |> Array.sum)