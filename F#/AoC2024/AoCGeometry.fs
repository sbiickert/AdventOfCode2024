namespace AoC

module Geometry =

    type Coord =
        {
            x: int64
            y: int64
        }

    type AdjacencyRule =
        | Rook
        | Bishop
        | Queen

    type Direction =
        | N
        | NE
        | E
        | SE
        | S
        | SW
        | W
        | NW

    type RotationDirection =
        | CW
        | CCW

    type Position = 
        {
            coord: Coord
            dir: Direction
        }

    type Segment =
        {
            coords: Coord * Coord
        }

    type Extent =
        {
            min: Coord
            max: Coord
        }

    let mkCoord x y =
        {x = x; y = y}

    let origin = mkCoord 0 0 

    let add a b =
        {x = a.x + b.x; y = a.y + b.y}
    
    let delta a b =
        {x = b.x - a.x; y = b.y - a.y}

    let manhattanDistance a b =
        abs(a.x - b.x) + abs(a.y - b.y)

    let distance a b =
        let del = delta a b
        sqrt (double del.x ** 2 + double del.y ** 2)

    let directionOffset (d:Direction) =
        match d with
        | Direction.N  -> {x =  0; y = -1}
        | Direction.NE -> {x =  1; y = -1}
        | Direction.E  -> {x =  1; y =  0}
        | Direction.SE -> {x =  1; y =  1}
        | Direction.S  -> {x =  0; y =  1}
        | Direction.SW -> {x = -1; y =  1}
        | Direction.W  -> {x = -1; y =  0}
        | Direction.NW -> {x = -1; y = -1}

    let directionsFor rule = 
        if rule = AdjacencyRule.Rook then [Direction.N; Direction.E; Direction.S; Direction.W]
        elif rule = AdjacencyRule.Bishop then [Direction.NE; Direction.SE; Direction.SW; Direction.NW]
        else
            [Direction.N; Direction.NE; Direction.E; Direction.SE; 
             Direction.S; Direction.SW; Direction.W; Direction.NW]

    let offset a dir (size: int64) =
        let off = directionOffset dir
        if size = 0 then a
        elif size = 1 then add a off
        else
            let bigOffset = {x = off.x * size; y = off.y * size}
            add a bigOffset
    
    let areAdjacent a b (rule:AdjacencyRule) =
        match rule with
        | AdjacencyRule.Rook -> manhattanDistance a b = 1
        | AdjacencyRule.Bishop -> abs(a.x - b.x) = 1 && abs(a.y - b.y) = 1
        | AdjacencyRule.Queen -> manhattanDistance a b = 1 || (abs(a.x - b.x) = 1 && abs(a.y - b.y) = 1)

    let adjacentCoords c (rule:AdjacencyRule) =
        let adjacent =
            directionsFor rule
            |> List.map (fun dir -> directionOffset dir)
            |> List.map (fun off -> add c off)
        adjacent
