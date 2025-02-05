[<AutoOpen>]
module Day02

open AoC.Util

let parseInput (input:string list) =
    input 
    |> List.map (fun line -> 
        line.Split(" ") 
        |> Array.map (fun s -> int s) 
        |> Array.toList)

let allPositive trend: bool =
    let filtered =
        trend |> List.filter (fun a -> a > 0 && a <= 3)
    filtered.Length = trend.Length

let allNegative trend: bool =
    let filtered =
        trend |> List.filter (fun a -> a < 0 && a >= -3)
    filtered.Length = trend.Length

let solvePartOne (reports: int list list) =
    let trends =
        reports
        |> List.map (fun rpt -> 
            List.windowed 2 rpt 
            |> List.map (fun [a;b] -> b - a ))
    let ascending = 
        trends |> List.filter allPositive
    let descending =
        trends |> List.filter allNegative
    ascending.Length + descending.Length

let solvePartTwo input =
    2

let solveDay02 isTest: Unit =
    let day = 2
    let puzzleName = "Red-Nosed Reports"
    printfn $"Day {day}: {puzzleName}"
    let inputName = inputFileName day isTest
    let input = readInput inputName true

    let reports = parseInput input

    let solution1 = solvePartOne reports
    printfn $"The number of safe reports is {solution1}"

    let solution2 = solvePartTwo input

    printfn "All done."
