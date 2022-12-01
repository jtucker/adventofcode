#load "common.fsx"
#r "nuget: FSharp.Data"
open FSharp.Collections
open System
open Common

let testData = "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
let valuesToLook = [|2; 4; 3; 7;|]

let parsedOutput = splitOn "|" >> Array.rev >> Array.head >> (fun s -> s.Trim()) >> splitOn " "
let calcOutputCount data =
    data 
    |> splitOn System.Environment.NewLine
    |> Array.collect parsedOutput
    |> Array.groupBy (fun s -> s.Length)
    |> Array.filter (fun g -> Array.contains (fst g) valuesToLook)
    |> Array.sumBy (snd >> Array.length)

open System.IO

let fileData = File.ReadAllText "./data/day8.txt"
calcOutputCount fileData


// remove existing letters
// remaining letters mapped to display
(*
  0:      1:      2:      3:      4:
 aaaa    ....    aaaa    aaaa    ....
b    c  .    c  .    c  .    c  b    c
b    c  .    c  .    c  .    c  b    c
 ....    ....    dddd    dddd    dddd
e    f  .    f  e    .  .    f  .    f
e    f  .    f  e    .  .    f  .    f
 gggg    ....    gggg    gggg    ....

  5:      6:      7:      8:      9:
 aaaa    aaaa    aaaa    aaaa    aaaa
b    .  b    .  .    c  b    c  b    c
b    .  b    .  .    c  b    c  b    c
 dddd    dddd    ....    dddd    dddd
.    f  e    f  .    f  e    f  .    f
.    f  e    f  .    f  e    f  .    f e
 gggg    gggg    ....    gggg    gggg

fcgedb

DisplayMap {
    a: g
    b: c
    c: b
    d: a
    e: f
    f: e
    g: d
}

(0: abcefg)
(1: cf)
(2: acdeg)
(3: acdfg)
(4: bcdf)
(5: abdfg)
(6: abdefg)
(7: acf)
(8: abcdefg)
(9: abcdfg)

*)

let map display (data: string) =
    let existingMap = display |> List.map (fun (x, _) -> x)
    let filtered = List.except existingMap (data.ToCharArray() |> List.ofArray) |> List.sort

    match data.Length with
    | 2 -> display @ [(filtered[0], 'c'); (filtered[1], 'f')] // a , b
    | 3 -> display @ [(filtered[0], 'a')] // d
    | 4 -> display @ [(filtered[0], 'b'); (filtered[1], 'd')] // e, f
    | 7 -> display @ [(filtered[0], 'g'); (filtered[1], 'e')] // c, g
    | _ -> failwith "should not be here"

let generateMapping input = 
    input
    |> splitOn " " 
    |> Array.sortBy (String.length)
    |> Array.filter (fun i -> Array.contains i.Length valuesToLook)
    |> List.ofArray
    |> List.fold map []

let getFromMap (mappings: (char * char) list) (toMap: char) =
    List.filter (fun (s, t) -> s = toMap) mappings |> List.head |> (snd >> string)

let convertFromSignal mapping (signal: string) =
    let signalMaps = dict [
        ("abcefg", "0")
        ("cf", "1")
        ("acdeg", "2")
        ("acdfg", "3")
        ("bcdf", "4")
        ("abdfg", "5")
        ("abdefg", "6")
        ("acf", "7")
        ("abcdefg", "8")
        ("abcdfg", "9")]
    let mapSignalToInt signal = signalMaps[signal]

    printfn $"{signal}"
    signal.ToCharArray () 
    |> List.ofArray
    |> List.map (getFromMap mapping)
    |> List.sort
    |> String.concat ""
    |> mapSignalToInt

let toTuple = splitOn "|" >> Array.map (fun s -> s.Trim()) >> (function [|x; y|] -> x,y | _ -> failwith "nope") 

let convertToInt (input, decodeList) = 
    let mapping = generateMapping input
    let printstuff z = printfn $"{z}"
    let dump z = 
        printstuff z
        
    decodeList 
    |> splitOn " "
    |> Array.head
    |> convertFromSignal mapping 
    

testData
|> splitOn System.Environment.NewLine
|> Array.map toTuple
|> Array.map convertToInt
 
// create mapping -> parse signals -> merge numbers -> conver to int

let contains (str: string) (c: char) = str.Contains c

let findDigits signal (digits: seq<string>) =
    let digitFromLength len map signal =
        signal
        |> Seq.filter (Seq.length >> (=) len)
        |> Seq.head

    let find3 (map: Map<int, string>) =
        Seq.find (fun s ->
            Seq.length s = 5
            && Seq.forall (contains s) map.[1])

    let find9 (map: Map<int, string>) signal =
        map.[3] + map.[4] |> Seq.distinct |> String.Concat

    let find0 (map: Map<int, string>) =
        Seq.find (fun s ->
            Seq.length s = 6
            && Seq.forall (contains s) map.[1]
            && not <| Seq.forall (contains s) map.[9])

    let find6 (map: Map<int, string>) =
        Seq.find (fun s ->
            Seq.length s = 6
            && not <| Seq.forall (contains s) map.[0]
            && not <| Seq.forall (contains s) map.[9])

    let find5 (map: Map<int, string>) =
        Seq.find (fun s ->
            Seq.length s = 5
            && Seq.forall (contains map.[6]) s)

    let find2 (map: Map<int, string>) =
        Seq.find (fun s -> Seq.length s = 5 && s <> map.[3] && s <> map.[5])

    let map =
        Seq.fold (fun (map: Map<int, string>) (d, f) -> map.Change(d, (fun _ -> Some <| f map signal))) Map.empty
        <| seq [
            (1, digitFromLength 2)
            (4, digitFromLength 4)
            (7, digitFromLength 3)
            (8, digitFromLength 7)
            (3, find3)]
            (9, find9)
            (0, find0)
            (6, find6)
            (5, find5)
            (2, find2)]

    let lookup =
        map
        |> Map.toSeq
        |> Seq.map (fun (k, v) -> (v |> Seq.sort |> String.Concat, k))
        |> Map.ofSeq

    digits
    |> Seq.map (Seq.sort >> String.Concat)
    |> Seq.map (fun s -> lookup.[s])
    |> Seq.reduce (fun acc d -> acc * 10 + d)
