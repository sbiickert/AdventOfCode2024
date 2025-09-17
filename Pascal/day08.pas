Program AOC_2024_Day08;
{$mode objfpc} // directive to be used for defining classes
{$m+}		   // directive to be used for using constructor
{$H+}		   // directive to be used for ANSI strings
{$J-}		   // directive to be used for turning off writeable const

Uses SysUtils, StrUtils, AoCUtils, AoCGeometry, AoCGrid, Classes;

Const
	DAY = 8;


Function GetAntinode(a1,a2: Coord2D; resonance: Integer = 1): Coord2D;
Var
	delta: Coord2D;
	r: Integer;
Begin
	delta := a1.DeltaTo(a2);
	result := a2;
	for r := 1 to resonance do
		result := result.Add(delta);
// 	WriteLn('The antinode of ' + a1.AsKey + ' and ' + a2.AsKey + ' is ' + result.AsKey + ' when r=' + IntToStr(resonance));
End;


Function GetAntinodes(antennae: Coord2DArray; ext: Extent2D; includeHarmonics: Boolean): Coord2DArray;
Var
	i,j,r,lo,hi: Integer;
	antinode: Coord2D;
Begin
	result := [];
	
	if includeHarmonics then
	begin
		lo := 0;
		hi := 10000000; // Just a big number
	end
	else
	begin
		lo := 1;
		hi := 1;
	end;
	
	for i := 0 to High(antennae)-1 do
		for j := i+1 to High(antennae) do
		begin
			for r := lo to hi do
			begin
				antinode := GetAntinode(antennae[i], antennae[j], r);
				if ext.Contains(antinode) then
					PushCoord(antinode, result)
				else
					break;
			end;
			for r := lo to hi do
			begin
				antinode := GetAntinode(antennae[j], antennae[i], r);
				if ext.Contains(antinode) then
					PushCoord(antinode, result)
				else
					break;
			end;
		end;
End;


Function SolvePart(grid: Grid2D; includeHarmonics: Boolean): Integer;
Var
	hist, unique: AoCIntegerMap;
	letter: String;
	i,letterCount: Integer;
	antennae, antinodes: Coord2DArray;
	ext: Extent2D;
	antinode: Coord2D;
Begin
	hist := grid.GetHistogram;
	ext := grid.GetExtent;
	unique := AoCIntegerMap.Create;
	
	for i := 0 to hist.Count-1 do
	begin
		letter := hist.Keys[i];
		letterCount := hist.Data[i];
		if (letterCount > 1) and (letter <> '.') then
		begin
			antennae := grid.GetCoords(letter);
			antinodes := GetAntinodes(antennae, ext, includeHarmonics);
			for antinode in antinodes do
				unique[antinode.AsKey] := 1;
		end;
	end;
	result := unique.Count;
End;

Procedure SolvePart1(grid: Grid2D);
Var
	count: Integer;
Begin
	WriteLn('Part 1: How many unique locations contain an antinode?');
	
	count := SolvePart(grid, false);
	
	WriteLn(Format('Part One Solution: %d', [count]));
End;

Procedure SolvePart2(grid: Grid2D);
Var
	count: Integer;
Begin
	WriteLn('Part 2: How many when resonance is included?');

	count := SolvePart(grid, true);
		
	WriteLn(Format('Part Two Solution: %d', [count]));
End;

Var
	iFileName: String;
	input: TStringList;
	grid: Grid2D;
Begin
	WriteLn('AoC 2015 Day 8: Resonant Collinearity');
	iFileName := InputFileName(DAY, False);
	input := ReadGroupedInput(iFileName, 0);
	grid := Grid2D.Create('-', Adjacency.QUEEN);
	grid.Load(input);
// 	grid.Print;
	SolvePart1(grid);
	SolvePart2(grid);
End.
