#load "AoCUtil.fs"
#load "AoCGeometry.fs"
#load "AoCGrid.fs"

open AoC.Geometry

let pt = {x = 10; y = 20}

let neighbors = adjacentCoords pt AdjacencyRule.Queen

open AoC.Util
let filePath = inputFileName 0 true

readInput filePath true

readGroupedInput filePath

gcd 2 4
gcd 15 20
gcd 13 20


lcm [2;3;4]
lcm [3;4;13]
