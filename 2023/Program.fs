open System.Threading.Tasks
open AOC2023

let days = [
    Day1.run
    Day2.run
]
let result = Day1.run()
printfn $"Day 1: %A{result}"
let result2 = Day2.run()
printfn $"Day 2: %A{result2}"


