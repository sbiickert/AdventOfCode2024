[<AutoOpen>]
module Day01

open AoC.Util
open System.Text.RegularExpressions

let parseInput input =
    let rx = Regex(@"(\d+)\s+(\d+)")
    let check e = 
        let m = rx.Match(e)
        [int m.Groups[1].Value; int m.Groups[2].Value]
    
    input |> List.map check

let solvePartOne input =
    let sorted = input |> List.map List.sort
    
    let totalDistance =
        List.zip sorted[0] sorted[1]
        |> List.map (fun (a,b) -> abs(a - b))
        |> List.sum
    totalDistance

let solvePartTwo (input: int list list) = 
    let col2Counts = frequencyMap input[1]
    let totalSimilarity =
        input[0]
        |> List.map (fun v ->
            if col2Counts.ContainsKey v then
                v * col2Counts.Item v
            else
                0
        )
        |> List.sum
    totalSimilarity

let solveDay01 isTest: Unit =
    let day = 1
    let puzzleName = "Historian Hysteria"
    printfn $"Day {day}: {puzzleName}"
    let inputName = inputFileName day isTest
    let input = readInput inputName true
    
    let pairs = parseInput input
    let pivot = AoC.Util.pivotMatrix pairs
    
    let solution1 = solvePartOne pivot
    printfn $"The total distance is {solution1}"

    let solution2 = solvePartTwo pivot
    printfn $"The total similarity is {solution2}"

    printfn "All done."
