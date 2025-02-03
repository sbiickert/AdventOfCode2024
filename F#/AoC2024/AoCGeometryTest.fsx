#load "AoCUtil.fs"
#load "AoCGeometry.fs"

let assertEqual a b =
    if a <> b then failwith $"Value '{a}' does not equal '{b}'."

let assertTrue a =
    if a <> true then failwith $"Assertion was not true."
let assertFalse a =
    if a <> false then failwith $"Assertion was not false."

open AoC.Geometry

// Test Direction here

let testCoordCreate () =
    let c = mkCoord 10 30
    assertEqual 10L c.x
    assertEqual 30L c.y
    let copied = {c with y = -40}
    assertEqual 10L copied.x
    assertEqual -40L copied.y

testCoordCreate ()

let testCoordAddSub () =
    let c1 = mkCoord 10 30
    let c2 = mkCoord 5 20
    let sum = Coord.add c1 c2
    assertEqual 15L sum.x
    assertEqual 50L sum.y
    let delta = Coord.delta c1 c2
    assertEqual -5L delta.x
    assertEqual -10L delta.y
    
testCoordAddSub ()

let testCoordDistance () = 
    let c1 = mkCoord 10 30
    let c2 = mkCoord 5 20
    let md = Coord.manhattanDistance c1 c2
    assertEqual 15L md
    let d = Coord.distance c1 c2
    assertTrue (AoC.Util.approxEqual 0.01 d 11.18)

testCoordDistance ()

let testCoordOffsets () = 
    assertEqual {x = 0; y = -1} (Coord.offset Direction.N 1L Coord.origin)
    assertEqual {x = -1; y = 1} (Coord.offset Direction.SW 1L Coord.origin)

testCoordOffsets ()

let testCoordAdjacency () =
    let cHorz = Direction.offset Direction.E
    assertTrue (Coord.areAdjacent cHorz Coord.origin AdjacencyRule.Rook)
    assertFalse (Coord.areAdjacent cHorz Coord.origin AdjacencyRule.Bishop)
    assertTrue (Coord.areAdjacent cHorz Coord.origin AdjacencyRule.Queen)
    let cDiag = Direction.offset Direction.SE
    assertFalse (Coord.areAdjacent cDiag Coord.origin AdjacencyRule.Rook)
    assertTrue (Coord.areAdjacent cDiag Coord.origin AdjacencyRule.Bishop)
    assertTrue (Coord.areAdjacent cDiag Coord.origin AdjacencyRule.Queen)
    let cFar = {x = 2; y = 0}
    assertFalse (Coord.areAdjacent cFar Coord.origin AdjacencyRule.Queen)

    let rookAdj = Coord.adjacentCoords Coord.origin AdjacencyRule.Rook
    assertEqual 4 rookAdj.Length
    let bishopAdj = Coord.adjacentCoords Coord.origin AdjacencyRule.Bishop
    assertEqual 4 bishopAdj.Length
    let queenAdj = Coord.adjacentCoords Coord.origin AdjacencyRule.Queen
    assertEqual 8 queenAdj.Length

testCoordAdjacency ()

printfn $"**** All tests passed. ****"