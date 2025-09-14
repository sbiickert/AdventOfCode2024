Program AOC_2024_Day06;
{$mode objfpc} // directive to be used for defining classes
{$m+}		   // directive to be used for using constructor
{$H+}		   // directive to be used for ANSI strings
{$J-}		   // directive to be used for turning off writeable const

Uses SysUtils, StrUtils, AoCUtils, AoCGeometry, AoCGrid, Classes;

Const
	DAY = 6;

Function CoordInFrontOf(pos: Pos2D): Coord2D;
Begin
	result := pos.Location.Offset(pos.Direction);
End;

Procedure SolvePart1(grid: Grid2D);
Var
	coords: Coord2DArray;
	pos: Pos2D;
	ext: Extent2D;
Begin
	WriteLn('Part 1: How many distinct positions will the guard visit?');
	
	coords := grid.GetCoords('^');
	pos := Pos2D.Create(coords[0], Dir2D.NORTH);
	ext := grid.GetExtent;
	
	while ext.Contains(pos.Location) do
	begin
		grid.SetString('X', pos.Location);
		if grid.GetString(CoordInFrontOf(pos)) = '#' then
			pos.Turn(Rot2D.CW);
		pos.MoveForward(1);
	end;
	
// 	grid.Print;
	coords := grid.GetCoords('X');
	
	WriteLn(Format('Part One Solution: %d', [Length(coords)]));
End;

Procedure SolvePart2(values: TStringList);
Var
	a, b, c: Integer;
Begin
	WriteLn('Part 2: DESCRIPTION');
	WriteLn(Format('Part Two Solution: %d', [13]));
End;

Var
	iFileName: String;
	input: TStringList;
	grid: Grid2D;
Begin
	WriteLn('AoC 2015 Day 6: Guard Gallivant');
	iFileName := InputFileName(DAY, False);
	input := ReadInput(iFileName);
	grid := Grid2D.Create('.', Adjacency.ROOK);
	grid.Load(input);
// 	grid.Print;
	SolvePart1(grid);
	SolvePart2(input);
End.
