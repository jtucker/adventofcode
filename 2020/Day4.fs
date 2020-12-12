module codingforbeer.AdventOfCode.Day4

open System.Text.RegularExpressions

let splitTheLines (str: string) = 
    str.Split("\n\n", System.StringSplitOptions.RemoveEmptyEntries)

let getMatchValue (mtch: Match) =
    mtch.Groups.[1].Value

let regexMatch str =
    Regex.Matches(str, "(\w+):")
    |> Seq.cast<Match>
    |> Seq.map(getMatchValue) 
    |> Set.ofSeq

let validPassport p =
    ["byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid";] |> Seq.forall ( fun x -> Set.contains x p)

let c = fileText @"2020\assets\day4.txt" 
        |> splitTheLines

let processPassports =
    c 
    |> Array.map(regexMatch)
    |> Seq.filter validPassport
    |> Seq.length

