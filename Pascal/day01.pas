Program AOC_2024_Day01;
{$mode objfpc} // directive to be used for defining classes
{$m+}		   // directive to be used for using constructor
{$H+}		   // directive to be used for ANSI strings
{$J-}		   // directive to be used for turning off writeable const

Uses SysUtils, StrUtils, AoCUtils, Classes;

type
	IArr2D = array of array of Integer;

Const
	DAY = 1;

Procedure SolvePart1(values: IArr2D);
Var
	idx,sumDistance: Integer;
Begin
	WriteLn('Part 1: What is the sum of the distances between pairs of numbers?');
	
	sumDistance := 0;
	for idx := 0 to High(values[0]) do
	begin
		sumDistance := sumDistance + Abs(values[0,idx] - values[1,idx]);
	end;
	
	WriteLn(Format('Part One Solution: %d', [sumDistance]));
End;

Procedure SolvePart2(values: IArr2D);
Var
	i,j,count,similarityScore: Integer;
Begin
	WriteLn('Part 2: What is the similarity score between the columns?');
	
	similarityScore := 0;
	for i := 0 to High(values[0]) do
	begin
		count := 0;
		for j := 0 to High(values[1]) do
		begin
			if values[0,i] = values[1,j] then
				Inc(count);
		end;
		similarityScore := similarityScore + (count * values[0,i]);
	end;
	WriteLn(Format('Part Two Solution: %d', [similarityScore]));
End;

Function ParseColumns(input: TStringList): IArr2D;
Var
	idx: Integer;
	dynArray: IArr2D;
	splitResult: TStringArray;
Begin
	SetLength(dynArray, 2, input.Count);
	for idx := 0 to input.Count - 1 do
	Begin
		splitResult := input[idx].Split([' '], TStringSplitOptions.ExcludeEmpty);
		dynArray[0,idx] := StrToInt(splitResult[0]);
		dynArray[1,idx] := StrToInt(splitResult[1]);
	End;
	SortIntArray(dynArray[0], True);
	SortIntArray(dynArray[1], True);
	result := dynArray;
End;

Var
	iFileName: String;
	input: TStringList;
	data: IArr2D;
Begin
	WriteLn('AoC 2015 Day 01: Historian Hysteria');
	iFileName := InputFileName(DAY, False);
	input := ReadInput(iFileName);
	data := ParseColumns(input);
	SolvePart1(data);
	SolvePart2(data);
End.
