Program AOC_2024_Day06;
{$mode objfpc} // directive to be used for defining classes
{$m+}		   // directive to be used for using constructor
{$H+}		   // directive to be used for ANSI strings
{$J-}		   // directive to be used for turning off writeable const

Uses SysUtils, StrUtils, AoCUtils, AoCGeometry, AoCGrid, Classes, ContNrs;

Const
	DAY = 6;

Function CoordInFrontOf(pos: Pos2D): Coord2D;
Begin
	result := pos.Location.Offset(pos.Direction);
End;


Function DoesPathLoop(grid: Grid2D; ext: Extent2D; startCoord:Coord2D; obstacleLocation: Coord2D): Boolean;
Var
	path: TFPHashList; // Much faster than AoCStringMap
	pos: Pos2D;
	val: Integer;
	ptr: Pointer;
	key: String;
Begin
	grid.SetString('#', obstacleLocation);
	
	path := TFPHashList.Create;
	pos := Pos2D.Create(startCoord, Dir2D.NORTH);
	val := 1;
	ptr := @val;
	result := False;
	
	while ext.Contains(pos.Location) do
	begin
		key := pos.ToStr;
		if path.FindIndexOf(key) <> -1 then
		begin
			result := true;
			break;
		end;
		
		while grid.GetString(CoordInFrontOf(pos)) = '#' do
		begin
			path.Add(key, ptr); // Only note the turns: save 6 seconds
			pos.Turn(Rot2D.CW);
		end;
			
		pos.MoveForward(1);
	end;
	
	path.Free;
	grid.SetString('.', obstacleLocation);
End;


Function SolvePart1(grid: Grid2D): Coord2DArray;
Var
	coords: Coord2DArray;
	startCoord: Coord2D;
	pos: Pos2D;
	ext: Extent2D;
Begin
	WriteLn('Part 1: How many distinct positions will the guard visit?');
	
	coords := grid.GetCoords('^');
	startCoord := coords[0];
	pos := Pos2D.Create(startCoord, Dir2D.NORTH);
	ext := grid.GetExtent;
	
	coords := [];
	while ext.Contains(pos.Location) do
	begin
		// Tracking this way to get list of potential places for obstacles in part 2
		if grid.GetString(pos.Location) = '.' then
			PushCoord(pos.Location, coords);
			
		grid.SetString('X', pos.Location);
		if grid.GetString(CoordInFrontOf(pos)) = '#' then
			pos.Turn(Rot2D.CW);
		pos.MoveForward(1);
	end;
	
	result := coords;
		
	WriteLn(Format('Part One Solution: %d', [Length(coords)+1]));
End;


Procedure SolvePart2(grid: Grid2D; potentialObstacleLocations: Coord2DArray);
Var
	obstacleLocation: Coord2D;
	startCoord: Coord2D;
	ext: Extent2D;
	isLoop: Boolean;
	loopCount: Integer;
Begin
	WriteLn('Part 2: How many places could an obstruction be placed to create a loop?');
	
	startCoord := grid.GetCoords('^')[0];
	ext := grid.GetExtent;
	loopCount := 0;
	
	for obstacleLocation in potentialObstacleLocations do
	begin
		isLoop := DoesPathLoop(grid, ext, startCoord, obstacleLocation);
		
		if isLoop then
		begin
			Inc(loopCount);
			if loopCount mod 100 = 0 then WriteLn(loopCount);
		end;
		
	end;
	
	WriteLn(Format('Part Two Solution: %d', [loopCount]));
End;

Var
	iFileName: String;
	input: TStringList;
	grid: Grid2D;
	coords: Coord2DArray;
Begin
	WriteLn('AoC 2015 Day 6: Guard Gallivant');
	iFileName := InputFileName(DAY, False);
	input := ReadInput(iFileName);
	
	grid := Grid2D.Create('.', Adjacency.ROOK);
	grid.Load(input);
	coords := SolvePart1(grid);
	
	grid := Grid2D.Create('.', Adjacency.ROOK);
	grid.Load(input);
	SolvePart2(grid, coords);
End.
