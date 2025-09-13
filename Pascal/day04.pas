Program AOC_2024_Day04;
{$mode objfpc} // directive to be used for defining classes
{$m+}		   // directive to be used for using constructor
{$H+}		   // directive to be used for ANSI strings
{$J-}		   // directive to be used for turning off writeable const

Uses SysUtils, StrUtils, AoCUtils, AoCGeometry, AoCGrid, Classes;

Const
	DAY = 4;

Function GetStringAtCoordWithOffset(grid: Grid2D; c: Coord2D; off: Coord2D; length: Integer): String;
Var
	ptr: Coord2D;
	i: Integer;
Begin
	ptr := c;
	result := '';
	for i := 1 to length do
	begin
		result := result + grid.GetString(ptr);
		ptr := ptr.Add(off);
	end;
End;

Procedure SolvePart1(grid: Grid2D);
Var
	count: Integer;
	offsets:Coord2DArray;
	ptr,offset: Coord2D;
	str: String;
Begin
	WriteLn('Part 1: How many times does "XMAS" appear?');
	
	count := 0;
	offsets := grid.GetNeighbourOffsets;
	
	for ptr in grid.GetExtent.AllCoords do
		if grid.GetString(ptr) = 'X' then
			for offset in offsets do
			begin
				str := GetStringAtCoordWithOffset(grid, ptr, offset, 4);
				if str = 'XMAS' then Inc(count);
			end;
	
	WriteLn(Format('Part One Solution: %d', [count]));
End;

Procedure SolvePart2(grid: Grid2D);
Var
	count,i,j: Integer;
	ptr: Coord2D;
	dirs: array of Dir2D;
	dir1,dir2: Dir2D;
	str1,str2: String;
Begin
	WriteLn('Part 2: How many times does an X-MAS appear?');
	
	count := 0;
	dirs := [NW,NE,SE,SW];
	
	for ptr in grid.GetExtent.AllCoords do
		if grid.GetString(ptr) = 'A' then
			for i := 0 to 3 do
			begin
				j := (i + 1) MOD 4;
				
				dir1 := dirs[i];
				dir2 := dirs[j];
				str1 := GetStringAtCoordWithOffset(grid,
												   ptr.Add(Coord2D.XOffset(dir1)),
												   Coord2D.XOffset(OppositeDir(dir1)),
												   3);
				str2 := GetStringAtCoordWithOffset(grid,
												   ptr.Add(Coord2D.XOffset(dir2)),
												   Coord2D.XOffset(OppositeDir(dir2)),
												   3);
				if (str1 = 'MAS') and (str2 = 'MAS') then Inc(count);
			end;
	
	WriteLn(Format('Part Two Solution: %d', [count]));
End;

Var
	iFileName: String;
	input: TStringList;
	grid: Grid2D;
Begin
	WriteLn('AoC 2015 Day 04: Ceres Search');
	iFileName := InputFileName(DAY, False);
	input := ReadInput(iFileName);
	grid := Grid2D.Create('.', Adjacency.QUEEN);
	grid.Load(input);
	SolvePart1(grid);
	SolvePart2(grid);
End.
