[<AutoOpen>]
module Day02

open AoC.Util

let parseInput (input:string list) =
    input 
    |> List.map (fun line -> 
        line.Split(" ") 
        |> Array.map (fun s -> int s) 
        |> Array.toList)

let positivePredicate a = a > 0 && a <= 3
let negativePredicate a = a < 0 && a >= -3

let allPositive trend: bool =
    trend |> List.forall positivePredicate

let allNegative trend: bool =
    trend |> List.forall negativePredicate

let isSafe trend: bool =
    (allPositive trend) || (allNegative trend)

let findUnsafeValueIndex (trend: int list) =
    if trend.Head > 0 then
        List.findIndex (positivePredicate >> not) trend
    else
        List.findIndex (negativePredicate >> not) trend

let getTrend report = 
    report
    |> List.windowed 2
    |> List.map (fun [a;b] -> b - a )

let getTrends reports =
    reports
    |> List.map getTrend

let mkReport trend =
    trend |> List.scan (fun acc t -> acc + t) 0 

let solvePartOne (reports: int list list) =
    let trends = getTrends reports
    let safeTrends = trends |> List.filter isSafe
    safeTrends.Length

let solvePartTwo (reports: int list list) pt1SafeCount =
    let trends = getTrends reports
    let unsafeTrends = trends |> List.filter (isSafe >> not)

    let mutable countUnsafeMadeSafe = 0
    for trend in unsafeTrends do
        let mutable canBeMadeSafe = false
        let badIndex = findUnsafeValueIndex trend
        //printfn $"The bad index in {trend} is {badIndex}"
        let report = mkReport trend
        let imin = max 0 (badIndex-1)
        let imax = badIndex+1
        for i in imin .. imax do
            let before = List.take i report
            let after = List.skip (i+1) report
            let modifiedReport = before @ after
            let modifiedTrend = getTrend modifiedReport
            if isSafe modifiedTrend then
                //printfn $"Removing number at {i} made {modifiedReport} safe."
                canBeMadeSafe <- true
        if canBeMadeSafe then
            countUnsafeMadeSafe <- countUnsafeMadeSafe + 1

    pt1SafeCount + countUnsafeMadeSafe

let solveDay02 isTest: Unit =
    let day = 2
    let puzzleName = "Red-Nosed Reports"
    printfn $"Day {day}: {puzzleName}"
    let inputName = inputFileName day isTest
    let input = readInput inputName true

    let reports = parseInput input

    let solution1 = solvePartOne reports
    printfn $"The number of safe reports is {solution1}"

    let solution2 = solvePartTwo reports solution1
    printfn $"The number of safe reports with dampening is {solution2}"

    printfn "All done."
