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

// Turns a trend into a report starting at 0
// which doesn't matter because all that's important is increases and decreases
let mkReport trend =
    trend |> List.scan (fun acc t -> acc + t) 0 

let canBeMadeSafe trend: bool =
    let badIndex = findUnsafeValueIndex trend
    let report = mkReport trend
    let imin = max 0 (badIndex-1)
    let imax = badIndex+1
    [imin .. imax]
    |> List.map (fun i ->
        let before = List.take i report
        let after = List.skip (i+1) report
        let modifiedReport = before @ after
        getTrend modifiedReport)
    |> List.map isSafe
    |> List.contains true


let solvePartOne (reports: int list list) =
    let trends = getTrends reports
    let safeTrends = trends |> List.filter isSafe
    safeTrends.Length

let solvePartTwo (reports: int list list) pt1SafeCount =
    let trends = getTrends reports
    let unsafeTrends = trends |> List.filter (isSafe >> not)
    let unsafeMadeSafe = List.filter canBeMadeSafe unsafeTrends
    let pt2SafeCount = unsafeMadeSafe.Length
    pt1SafeCount + pt2SafeCount


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