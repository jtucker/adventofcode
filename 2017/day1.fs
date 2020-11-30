module AdventOfCode.Day1

open Xunit


let charToInt (c:char) = System.Convert.ToInt32(c |> string)
let stringToInt (c:string) = c |> Seq.map charToInt
let areEqual (pair:int[]) = pair.[0] = pair.[1]

let solveCaptcha (captcha:string) =
    let appended = captcha + (captcha |> Seq.take 1 |> System.String.Concat)
    appended |> 
        Seq.map charToInt |> 
        Seq.windowed 2 |> 
        Seq.filter areEqual |> 
        Seq.map (fun x ->  x.[0]) |> 
        Seq.sum

let solveCaptcha2 (captcha:string) =
    let offset = captcha.Length / 2
    let offsetCaptcha = 
        seq { yield captcha |> 
                    stringToInt |> 
                    Seq.skip offset |> 
                    Seq.take offset; 
              yield captcha |> 
                    Seq.map charToInt |> 
                    Seq.take offset }
        |> Seq.concat

    captcha |> 
        stringToInt |>
        Seq.zip offsetCaptcha |> 
        Seq.filter (fun (x, y) -> x = y) |>
        Seq.map  (fun (x, _) -> x) |>
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
[<InlineData("1221", 0)>]
[<InlineData("123425", 4)>]
[<InlineData("123123", 12)>]
[<InlineData("12131415", 4)>]
let ``should solve captcha with circular list [problem 2]`` (input, output) =
    let actual = solveCaptcha2 input
    Assert.Equal<int>(output, actual)