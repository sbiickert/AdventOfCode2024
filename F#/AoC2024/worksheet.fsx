#load "AoCUtil.fs"
#load "AoCGeometry.fs"
#load "AoCGrid.fs"

open AoC.Geometry

let l = [[5;3];[10;7]]
List.map (fun [a;b] -> a - b) l