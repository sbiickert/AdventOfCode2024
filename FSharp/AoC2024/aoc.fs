// For more information see https://aka.ms/fsharp-console-apps
printfn "Advent of Code 2024"

open System.IO
let lines = File.ReadLines("../../Input/day00_test.txt")

lines |> Seq.iter(fun x -> printfn  "%s" x) 
