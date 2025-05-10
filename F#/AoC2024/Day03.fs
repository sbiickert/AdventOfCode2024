[<AutoOpen>]
module Day03

open AoC.Util
open System.Text.RegularExpressions


let sumForLine line =
    let rx = Regex(@"mul\((\d+)\,(\d+)\)")
    let matches = rx.Matches(line)
    let mSeq = Seq.cast matches

    Seq.map (fun (a:Match) -> int a.Groups[1].Value * int a.Groups[2].Value) mSeq
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

