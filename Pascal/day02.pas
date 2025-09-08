Program AOC_2024_Day02;
{$mode objfpc} // directive to be used for defining classes
{$m+}		   // directive to be used for using constructor
{$H+}		   // directive to be used for ANSI strings
{$J-}		   // directive to be used for turning off writeable const

Uses SysUtils, StrUtils, AoCUtils, Classes;

Type
	IArr2D = array of array of Integer;

Const
	DAY = 2;

Function ReportsToTrends(reports: IArr2D): IArr2D;
Var
	i,j,diff: Integer;
Begin
	SetLength(result, Length(reports), 7);
	for i := 0 to Length(reports)-1 do
	begin
		SetLength(result[i], Length(reports[i])-1);
		for j := 0 to High(reports[i])-1 do
		begin
			diff := reports[i,j+1] - reports[i,j];
			// Write( Format('%d ', [diff]) );
			result[i,j] := diff;
		end;
		// WriteLn;
	end;
End;

Procedure SolvePart1(reports: IArr2D);
Var
	i,j,safeCount,posCount,negCount: Integer;
	trends: IArr2D;
	isSafe: Boolean;
Begin
	WriteLn('Part 1: How many of the reports are safe?');
	
	safeCount := 0;
	trends := ReportsToTrends(reports);
	for i := 0 to High(trends) do
	begin
		isSafe := True;
		posCount := 0;
		negCount := 0;
		for j := 0 to High(trends[i]) do
		begin
			if Abs(trends[i,j]) > 3 then isSafe := False
			else if trends[i,j] > 0 then Inc(posCount)
			else if trends[i,j] < 0 then Inc(negCount)
			else isSafe := False; // zero
		end;
		if (posCount > 0) and (negCount > 0) then isSafe := False;
		if isSafe then Inc(safeCount);
	end;
	
	WriteLn(Format('Part One Solution: %d', [safeCount]));
End;

Procedure SolvePart2(values: IArr2D);
Var
	a, b, c: Integer;
Begin
	WriteLn('Part 2: DESCRIPTION');
	WriteLn(Format('Part Two Solution: %d', [13]));
End;

Function ParseReports(input: TStringList): IArr2D;
Var
	splitResult: TStringArray;
	i,j: Integer;
Begin
	SetLength(result, input.Count, 8);
	for i := 0 to input.Count-1 do
	begin
		splitResult := input[i].Split([' ']);
		SetLength(result[i], Length(splitResult));
		for j := 0 to High(splitResult) do
			result[i,j] := StrToInt(splitResult[j])
	end;
End;


Var
	iFileName: String;
	input: TStringList;
	reports: IArr2D;
Begin
	WriteLn('AoC 2015 Day 02: Red-Nosed Reports');
	iFileName := InputFileName(DAY, False);
	input := ReadInput(iFileName);
	reports := ParseReports(input);
// 	AssertIntEqual(reports[0,0], 7, '');
// 	AssertIntEqual(reports[0,4], 1, '');
// 	AssertIntEqual(reports[5,0], 1, '');
// 	AssertIntEqual(reports[5,4], 9, '');
// 	AssertIntEqual(High(reports[5]), 4, '');
	SolvePart1(reports);
	SolvePart2(reports);
End.
