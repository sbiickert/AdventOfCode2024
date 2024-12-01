//
//  main.swift
//  AoC2024
//
//  Created by Simon Biickert on 2024-09-30.
//

import Foundation

let s = Day01()
let i = AoCInput.inputsFor(solution: s)
var rTest = s.solve(i[0])
print(rTest)
let rChallenge = s.solve(i[0])
print(rChallenge)
