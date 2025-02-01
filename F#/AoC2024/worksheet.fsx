#load "AoCUtil.fs"
#load "AoCGeometry.fs"
#load "AoCGrid.fs"

open AoC.Geometry

let pt = {x = 10; y = 20}

let neighbors = adjacentCoords pt AdjacencyRule.Queen