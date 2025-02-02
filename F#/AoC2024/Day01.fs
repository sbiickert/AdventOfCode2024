[<AutoOpen>]
module Day01

open AoC.Util

let solveDay01 isTest: Unit =
    let day = 1
    let puzzleName = "Historian Hysteria"
    printfn $"Day {day}: {puzzleName}"
    let inputName = inputFileName day isTest
    let input = readInput inputName true

    //let solution1 = solvePartOne input
    //let solution1 = solvePartTwo input

    printfn "All done."
