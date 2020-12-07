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

let validatePasswords validator = 
    (Seq.map (PasswordRule.parse >> validator) (fileContents @"2020\assets\day2.txt"))
    |> Seq.filter (id)
    |> Seq.length

Console.WriteLine(sprintf "The number of valid sled passwords is %i" (validatePasswords PasswordRule.validate))
Console.WriteLine(sprintf "The number of valid toboggan passwords is %i" (validatePasswords PasswordRule.tobogganValidate))