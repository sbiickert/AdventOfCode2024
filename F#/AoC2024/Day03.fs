[<AutoOpen>]
module Day03

open AoC.Util
open System.Text.RegularExpressions

let mul (m:Match) =
    int m.Groups[1].Value * int m.Groups[2].Value

let sumForLine line =
    let rx = Regex(@"mul\((\d+)\,(\d+)\)")
    rx.Matches(line)
    |> Seq.cast
    |> Seq.map mul
    |> Seq.sum

let solvePartOne input =
    sumForLine input

let solvePartTwo (input:string) =
    input.Split "do()"
    |> Array.map (fun (s:string) -> s.Split("don't()")[0])
    |> String.concat ""
    |> sumForLine

let solveDay03 isTest: Unit =
    let day = 3
    let puzzleName = "Mull It Over"
    printfn $"Day {day}: {puzzleName}"
    let inputName = inputFileName day isTest
    let input = readGroupedInput inputName

    let joined1 = String.concat "" input[0]
    let joined2 = String.concat "" input[0]

    let solution1 = solvePartOne joined1
    printfn $"Part One: the sum is {solution1}"
    let solution2 = solvePartTwo joined2
    printfn $"Part Two: the sum is {solution2}"

