module codingforbeer.AdventOfCode.Day7

open FParsec

let rules = fileText @"2020\assets\day7.txt" 

let bagParser = manyCharsTill anyChar (skipString " bag" .>> optional (skipChar 's'))
let skipContainParser = spaces1 >>. skipString "contain" .>> spaces1
let containerParser = pint32 .>> spaces1 .>>. bagParser
let containerChooserParser = ((stringReturn "no other bags" List.empty)<|> sepBy containerParser (skipString ", "))

let parser =   
    sepEndBy (bagParser .>> skipContainParser .>>. containerChooserParser .>> pchar '.') skipNewline

let getSuccessRules parser line =    
    match run parser line with
    | Failure _ -> failwith "Parsing error"
    | Success (v, _, _) -> v
        
let rec getContainersForBag containers bags =
    let getContainersForKey (key, keyContainer) =
        let c = keyContainer |> List.map snd
        (key, c)

    let rulesFlipper rules = 
        rules 
        |> List.collect(fun (bagColor, contents) -> contents |> List.map(fun (_, c) -> (c, bagColor)))

    let rulesDictionary = getSuccessRules parser rules
                        |> rulesFlipper
                        |> List.groupBy fst
                        |> List.map getContainersForKey
                        |> dict    

    let hasContents c =
        match rulesDictionary.TryGetValue(c) with
        | (true, contents) -> contents
        | (false, _) -> List.empty

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

let rld = getSuccessRules parser rules 
            |> dict

let rec getCountOfBags bagColor = 
    match rld.TryGetValue bagColor with   
    | (true, []) -> 0
    | (true, containers) -> containers |> List.sumBy (fun (c, bgc) -> c + (c * getCountOfBags bgc))
    | (false, _) -> 0

printfn "Count of bags: %i" (getCountOfBags "shiny gold")