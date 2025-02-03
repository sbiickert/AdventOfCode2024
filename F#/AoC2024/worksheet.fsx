#load "AoCUtil.fs"
#load "AoCGeometry.fs"
#load "AoCGrid.fs"

open AoC.Geometry

let pt = {x = 10; y = 20}

let neighbors = Coord.adjacentCoords pt AdjacencyRule.Queen

open AoC.Util
let filePath = inputFileName 0 true

readInput filePath true

readGroupedInput filePath

gcd 2 4
gcd 15 20
gcd 13 20


lcm [2;3;4]
lcm [3;4;13]

let pos = mkPos Coord.origin Direction.N
Position.turn pos RotationDirection.CW

Position.moveForward pos 16L

let dOpt = Direction.fromString "w"
match dOpt with
| Some(d) -> Some(mkPos Coord.origin d)
| None -> None

let aList = [1;2;3;4]
let bList = ["a"; "b"; "c"]
let cp = AoC.Util.cartesian aList bList

let ext =mkExtI 1 2 13 14
let ins = Extent.inset ext 2