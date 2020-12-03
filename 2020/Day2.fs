module codingforbeer.AdventOfCode.Day2

open System
open System.Text.RegularExpressions

type PasswordLine = {
    Positions : (int * int)
    Letter : Char
    Password: String
}

module PasswordRule =
    let isInRange range (str: String) =
        match range with
        | (lower, higher) when str.Length >= lower && str.Length <= higher -> true
        | _ -> false
    
    let isInPositions positions letter (str: String) =
        match positions with 
        | (first, second) when str.[first - 1] = letter && str.[second - 1] = letter -> false
        | (first, second) when str.[first - 1] = letter || str.[second - 1] = letter -> true
        | _ -> false

    let parse line =
        let matches = Regex.Match(line, "(?<lower>\d+)-(?<higher>\d+) (?<letter>[a-z]): (?<password>[a-z]+)")
        {
            Positions = (int(matches.Groups.["lower"].Value), int(matches.Groups.["higher"].Value))
            Letter = char(matches.Groups.["letter"].Value)
            Password = matches.Groups.["password"].Value
        }

    let tobogganValidate rule =
        rule.Password |> isInPositions rule.Positions rule.Letter

    let validate rule = 
        rule.Password |> String.filter(fun c -> c = rule.Letter) |> isInRange rule.Positions

open System.IO

let fileContents path =
    try
        seq {
            let reader = new StreamReader(File.OpenRead path)        
            while( not reader.EndOfStream ) do 
                yield reader.ReadLine()
        }
    with 
        | :? FileNotFoundException -> sprintf "File path (%s) not found." path |> ignore; Seq.empty

let validSledPasswordRules = 
    (Seq.map (PasswordRule.parse >> PasswordRule.validate) (fileContents @"2020\assets\day2.txt"))
    |> Seq.filter (id)
    |> Seq.length

let validTobogganPasswordRules = 
    (Seq.map (PasswordRule.parse >> PasswordRule.tobogganValidate) (fileContents @"2020\assets\day2.txt"))
    |> Seq.filter (id)
    |> Seq.length
