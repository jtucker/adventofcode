module AdventOfCode.Day2
open Xunit

[<Theory>]
[<InlineData("day2_theory.txt", 18)>]
let ``should solve checksum`` (input, output) =
    let actual = input |> int
    Assert.Equal<int>(output, actual)
