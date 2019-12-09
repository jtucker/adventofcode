open System.IO

let inputData = File.ReadAllLines "adventofcode2019\\inputdata\\day1.txt"
let calculateMass mass = 
    System.Math.Max(0, ((mass / 3) - 2))

let rec calculateMassWithFuel total mass = 
    if mass <= 0 then
        total
    else        
        let newMass = calculateMass mass
        calculateMassWithFuel (total + newMass) newMass

let day1result =
    inputData |> Array.sumBy (int >> calculateMass)

let day1resultWithFuel = 
    inputData |> Array.sumBy (int >> calculateMassWithFuel 0)

printfn "Total Mass: %i" day1result
printfn "Total Mass with fuel: %i" day1resultWithFuel