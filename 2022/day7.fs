module codingforbeer.Aoc2022.Day7

open System
open System.IO
open FParsec

let testData = "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k" 

type FileSystemNode = Directory of string | File of int * string
type Command = CD of string | LS of FileSystemNode list
 
let runParser parser input =
    match run parser input with
    | Success (x, _, _) -> x
    | _ -> failwith "Invalid input"

let fsCommands =
    let cdCmd = pstring "$ cd " >>. restOfLine true |>> CD
    let file = (pint32 .>> pchar ' ') .>>. restOfLine true |>> File
    let directory = pstring "dir " >>. restOfLine true |>> Directory
    let fsNode = directory <|> file
    let lsCmd = pstring "$ ls" >>. skipNewline >>. many fsNode |>> LS
    let cmds = cdCmd <|> lsCmd
    
    let parser = many cmds
    runParser parser
    
let getCmd = function
    | (_, "/") -> []
    | (_ :: rest, "..") -> rest
    | ([], "..") -> failwith "Invalid path"
    | (path, dir) -> dir :: path

let buildFs (commands: Command list) =
    let rec fs path =
        function 
        | (CD dir) :: rest -> fs (getCmd (path, dir)) rest
        | (LS items) :: t -> (path, items) :: fs path t
        | [] -> []
        
    commands |> fs [] |> Map.ofList

let rec getDirectorySize fs path =
    fs
    |> Map.find path
    |> List.sumBy(function 
        | Directory dir -> getDirectorySize fs <| getCmd (path, dir) 
        | File (size, _) -> size)

let solvePart1 =
    let fs = File.ReadAllText "./2022/data/day7.txt"
                    |> fsCommands
                    |> buildFs

    fs 
    |> Map.keys
    |> Seq.map (getDirectorySize fs)
    |> Seq.filter (fun x -> x < 100_000)
    |> Seq.sum

let solvePart2 =
    let fs = File.ReadAllText "./2022/data/day7.txt"
                    |> fsCommands
                    |> buildFs

    let getRequiredSpace dirs = 
        let totalSize = dirs |> Seq.max
        let difference = 70_000_000 - totalSize
        let spaceToClear = 30_000_000 - difference

        dirs |> Seq.filter (fun x -> x >= spaceToClear) |> Seq.min

    fs
    |> Map.keys
    |> Seq.map (getDirectorySize fs)
    |> getRequiredSpace
