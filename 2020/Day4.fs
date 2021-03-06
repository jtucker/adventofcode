module codingforbeer.AdventOfCode.Day4

open System.Text.RegularExpressions
open System.Collections.Generic

type Height =
    | Centimeters of int
    | Inches of int
    | NoHeight

type Passport =
    { PassportId: string
      EyeColor: string
      Height: Height
      HairColor: string
      ExpirationYear: int
      IssueYear: int
      BirthYear: int }

let splitTheLines (splitBy: string array) (str: string) =
    str.Split(splitBy, System.StringSplitOptions.RemoveEmptyEntries)

let passports =
    fileText @"2020\assets\day4.txt"
    |> splitTheLines [| "\n\n" |]

let processPassports passportList mapper filter =
    let regexMatch str =
        let getMatchValue (groupName: string) (mtch: Match) = mtch.Groups.[groupName].Value
        Regex.Matches(str, "(?<Field>\w+):(?<Value>\S+)")
        |> Seq.cast<Match>
        |> Seq.map (fun m -> ((getMatchValue "Field" m), (getMatchValue "Value" m)))

    passportList
    |> Array.map (regexMatch)
    |> Seq.map mapper
    |> Seq.filter filter
    |> Seq.length

let validPassport p =
    [ "byr"
      "iyr"
      "eyr"
      "hgt"
      "hcl"
      "ecl"
      "pid" ]
    |> Seq.forall (fun x -> Set.contains x p)

let requiredFieldsMapper p = Set.ofSeq p |> Set.map fst
let validPassportsRequiredFields = processPassports passports requiredFieldsMapper validPassport
printfn "Answer Part 1: %i passports with all required fields" validPassportsRequiredFields

let validRulesMapper keyValues =
    let items = keyValues |> Seq.distinctBy fst |> dict
    let getValueOrDefault key defaultValue (items: IDictionary<string, string>) =
        match items.TryGetValue(key) with
        | (true, x) -> x
        | (false, _) -> defaultValue
    let height s =
        let mtch = Regex.Match(s, "(?<value>\d+)(?<measurement>(in|cm))")
        match mtch.Success with
        | true ->
            match mtch.Groups.["measurement"].Value with
            | "in" -> Inches(mtch.Groups.["value"].Value |> int)
            | "cm" -> Centimeters(mtch.Groups.["value"].Value |> int)
            | _ -> NoHeight
        | false -> NoHeight

    { PassportId = getValueOrDefault "pid" "" items
      EyeColor = getValueOrDefault "ecl" "" items
      Height = getValueOrDefault "hgt" "" items |> height
      HairColor = getValueOrDefault "hcl" "" items
      ExpirationYear = getValueOrDefault "eyr" "0" items |> int
      IssueYear = getValueOrDefault "iyr" "0" items |> int
      BirthYear = getValueOrDefault "byr" "0" items |> int }

let validRulesFilter (p: Passport) = 
    let str r s = Regex.IsMatch(s, r)

    let isBetween f l v =
        v >= f && v <= l

    let validHeight = function
        | Inches i when i |> isBetween 59 76 -> true
        | Centimeters c when c |> isBetween 150 193 -> true
        | _ -> false
        
    let eye  = function
        | "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" -> true
        | _ -> false

    (p.PassportId |> str "^\d{9}$") &&
    (p.EyeColor |> eye) &&
    (p.HairColor |> str "^#([0-9a-fA-F]){6}$") &&
    (p.ExpirationYear |> isBetween 2020 2030) &&
    (p.IssueYear |> isBetween 2010 2020) &&
    (p.BirthYear |> isBetween 1920 2002) &&
    (p.Height |> validHeight)

let validPassportsByRules = processPassports passports validRulesMapper validRulesFilter
printfn "Answer Part 2: %i passports with based on rules" validPassportsByRules

