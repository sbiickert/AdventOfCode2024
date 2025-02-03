#load "AoCUtil.fs"
#load "AoCGeometry.fs"

let assertEqual a b =
    if a <> b then failwith $"Value '{a}' does not equal '{b}'."

let assertTrue a =
    if a <> true then failwith $"Assertion was not true."

open AoC.Geometry

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

printfn $"**** All tests passed. ****"