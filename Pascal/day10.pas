Program AOC_2024_Day10;
{$mode objfpc} // directive to be used for defining classes
{$m+}		   // directive to be used for using constructor
{$H+}		   // directive to be used for ANSI strings
{$J-}		   // directive to be used for turning off writeable const

Uses SysUtils, StrUtils, AoCUtils, AoCGeometry, AoCGrid, Classes;

Const
	DAY = 10;

type
	ScoreAndCount = Record
		score: Integer;
		count: Integer;
	end;



Function RateTrailheadBFS(th: Coord2D; map: Grid2D): ScoreAndCount;
Var
	coords,nextCoords,neighbours: Coord2DArray;
	elev,pathCount: Integer;
	coord, n: Coord2D;
	uniq: AoCStringMap;
Begin
	pathCount := 0;
	coords := [th];
	uniq := AoCStringMap.Create;
	
	while Length(coords) > 0 do
	begin
		nextCoords := [];
		
		for coord in coords do
		begin
			elev := map.GetInteger(coord);
			
			if elev = 9 then
			begin
				Inc(pathCount);
				uniq[coord.AsKey] := '';
			end
			else
			begin
				neighbours := map.GetNeighbourCoords(coord);
				for n in neighbours do
				begin
					if map.GetString(n) = '.' then continue;
					if map.GetInteger(n) = elev + 1 then PushCoord(n, nextCoords);
				end;
			end;
		end;
		
		coords := nextCoords;
	end;
	
	result.count := pathCount;
	result.score := uniq.Count;
End;


Procedure SolveParts(map: Grid2D);
Var
	score,count: Integer;
	thInfo: ScoreAndCount;
	trailheads: Coord2DArray;
	trailhead: Coord2D;
Begin
	score := 0;
	count := 0;
	trailheads := map.GetCoords('0');
	
	for trailhead in trailheads do
	begin
		thInfo := RateTrailheadBFS(trailhead, map);
		score := score + thInfo.score;
		count := count + thInfo.count;
	end;
	
	WriteLn('Part 1: Sum of all the scores of trailheads?');
	WriteLn(Format('Part One Solution: %d', [score]));
	WriteLn('Part 2: Count of all unique trails?');
	WriteLn(Format('Part Two Solution: %d', [count]));
End;


Var
	iFileName: String;
	input: TStringList;
	map: Grid2D;
Begin
	WriteLn('AoC 2015 Day 10: Hoof It');
	iFileName := InputFileName(DAY, False);
	input := ReadInput(iFileName);
	map := Grid2D.Create('.', Adjacency.ROOK);
	map.Load(input);
	//map.Print;
	SolveParts(map);
End.
