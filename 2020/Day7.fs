module codingforbeer.AdventOfCode.Day7

open FParsec
type UserState = unit // doesn't have to be unit, of course
type Parser<'t> = Parser<'t, UserState>

let bagParser: Parser<_> = manyCharsTill anyChar (skipString " bag" .>> optional (skipChar 's'))
let skipContainParser: Parser<_> = spaces1 >>. skipString "contain" .>> spaces1
let containerParser = pfloat .>> spaces1 .>>. bagParser
let containerChooserParser = ((stringReturn "no other bags" List.empty)<|> sepBy containerParser (skipString ", "))

let parser =   
    sepEndBy (bagParser .>> skipContainParser .>>. containerChooserParser .>> pchar '.') skipNewline

let rules = fileText @"2020\assets\day7.txt" 

let getSuccessRules parser line =    
    match run parser line with
    | Failure _ -> failwith "Parsing error"
    | Success (v, _, _) -> 
        v |> List.collect(fun (bagColor, contents) -> contents |> List.map(fun (_, c) -> (c, bagColor)))

let getContainersForKey (key, keyContainer) =
    let c = keyContainer |> List.map snd
    (key, c)

let dictFormat = getSuccessRules parser rules
                    |> List.groupBy fst
                    |> List.map getContainersForKey
                    |> dict

let hasContents c =
    match dictFormat.TryGetValue(c) with
    | (true, contents) -> contents
    | (false, _) -> List.empty

let rec getContainersForBag containers bags =
    bags
    |> List.collect hasContents
    |> function 
        | [] -> containers
        | matches -> 
            let updatedContainers = Set.union (Set.ofList matches) (Set.ofList containers) |> Set.toList
            getContainersForBag updatedContainers matches

getContainersForBag List.empty ["shiny gold"]
|> List.length
|> printfn "Number of bags: %i"