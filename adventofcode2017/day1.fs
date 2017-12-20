module AdventOfCode.Day1

open Xunit

let converToInt (c:char) = System.Convert.ToInt32(c |> string)
let areEqual (pair:int[]) = pair.[0] = pair.[1]

let solveCaptcha (captcha:string) offset =
    let appended = captcha + (captcha |> Seq.take 1 |> System.String.Concat)
    appended |> 
        Seq.map converToInt |> 
        Seq.windowed 2 |> 
        Seq.filter areEqual |> 
        Seq.map (fun x ->  x.[0]) |> 
        Seq.sum


[<Theory>]
[<InlineData("1122", 3)>]
[<InlineData("1111", 4)>]
[<InlineData("1234", 0)>]
[<InlineData("91212129", 9)>]
let ``should solve captcha [problem 1]`` (input, output) =
    let actual = solveCaptcha input
    Assert.Equal<int>(output, actual)

[<Theory>]
[<InlineData("1212", 6)>]
let ``should solve captcha with circular list [problem 2]`` (input, output) =
    let actual = solveCaptcha input
    Assert.Equal<int>(output, actual)