#load "AoCUtil.fs"

let assertEqual a b =
    if a <> b then failwith $"Value '{a}' does not equal '{b}'."

let testReadInput() = 
    let inFileNameTest = AoC.Util.inputFileName 0 true
    assertEqual inFileNameTest "/Users/sjb/Developer/Advent of Code/2024/AdventOfCode2024/Input/day00_test.txt"
    let inFileNameChall = AoC.Util.inputFileName 0 false
    assertEqual inFileNameChall "/Users/sjb/Developer/Advent of Code/2024/AdventOfCode2024/Input/day00_challenge.txt"
    let inputWithEmpty = AoC.Util.readInput inFileNameTest false
    assertEqual 10 inputWithEmpty.Length
    assertEqual "G0, L0" inputWithEmpty.Head
    assertEqual "" inputWithEmpty[3]
    let inputWithoutEmpty = AoC.Util.readInput inFileNameTest true
    assertEqual 8 inputWithoutEmpty.Length
    assertEqual "G1, L0" inputWithoutEmpty[3]

testReadInput ()
   
let testReadGroupedInput() =
    let inFileNameTest = AoC.Util.inputFileName 0 true
    let input = AoC.Util.readGroupedInput inFileNameTest
    assertEqual 3 input.Length
    let lengths = List.map (fun (x:list<string>) -> x.Length) input
    assertEqual [3;2;3] lengths
    let g2 = input[2]
    assertEqual "G2, L2" g2[2]

testReadGroupedInput ()

let testGCD() = 
    assertEqual 2L (AoC.Util.gcd 2 4)
    assertEqual 5L (AoC.Util.gcd 15 20)
    assertEqual 1L (AoC.Util.gcd 13 20)

testGCD ()

let testLCM() =
    assertEqual 12L (AoC.Util.lcm [2;3;4])
    assertEqual 156L (AoC.Util.lcm [3;4;13])

testLCM ()

printfn $"**** All tests passed. ****"