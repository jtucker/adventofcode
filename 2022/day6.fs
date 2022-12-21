module codingforbeer.AoC2022.Day6

open System
open FsUnit.Xunit
open System.IO

let getIndex size (s: string) (marker: string)  =
    s.IndexOf(marker) + size

let findSeq size s =
    s
    |> Seq.windowed size
    |> Seq.mapi (fun i x -> (i, x |> Seq.distinct))
    |> Seq.skipWhile(fun (i, x) -> (Seq.length x) <> size)
    |> Seq.take 1
    |> Seq.collect (fun (i, x) -> x)
    |> Seq.toArray
    |> String
    |> getIndex size s

let data = File.ReadAllText "2022/data/day6.txt"

let part1 = findSeq 4 data
let part2 = findSeq 14 data

let ``Calculates the correct index items of 5; 6; 10; 11`` =
    [
        "bvwbjplbgvbhsrlpgdmjqwftvncz";
        "nppdvjthqldpwncqszvftbrmjlhg";
        "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg";
        "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw";
    ]
    |> List.map (findSeq 4) 
    |> should equal [5; 6; 10; 11]

let ``Calculates the correct index items of 23; 23; 29; 26`` =
    [
        "bvwbjplbgvbhsrlpgdmjqwftvncz";
        "nppdvjthqldpwncqszvftbrmjlhg";
        "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg";
        "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw";
    ]
    |> List.map (findSeq 14)
    |> should equal [23; 23; 29; 26]

