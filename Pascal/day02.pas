Program AOC_2024_Day02;
{$mode objfpc} // directive to be used for defining classes
{$m+}		   // directive to be used for using constructor
{$H+}		   // directive to be used for ANSI strings
{$J-}		   // directive to be used for turning off writeable const

Uses SysUtils, StrUtils, AoCUtils, Classes;

Type
	IArr2D = array of array of Integer;
	IArr1D = array of Integer;

Const
	DAY = 2;

Function ReportToTrend(report: IArr1D; ignoringIndex: Integer = -1): IArr1D;
Var
	i,j,k,diff: Integer;
Begin
	if ignoringIndex >= 0 then 
		SetLength(result, Length(report)-2)
	else
		SetLength(result, Length(report)-1);
		
	i := 0; j := 0; k := 0;
	while j <= High(report) do
	begin
		if i = ignoringIndex then Inc(i);
		j := i + 1;
		if j = ignoringIndex then Inc(j);
		diff := report[j] - report[i];
// 		Write( Format('%d ', [diff]) );
		result[k] := diff;
		Inc(i); Inc(j); Inc(k);
	end;
// 	WriteLn;
End;

Function ReportsToTrends(reports: IArr2D): IArr2D;
Var
	i: Integer;
Begin
	SetLength(result, Length(reports));
	for i := 0 to Length(reports)-1 do
	begin
		result[i] := ReportToTrend(reports[i]);
	end;
End;

Function IsTrendSafe(trend: IArr1D): Boolean;
Var
	j,posCount,negCount: Integer;
Begin
	result := True;
	posCount := 0;
	negCount := 0;
	for j := 0 to High(trend) do
	begin
		if Abs(trend[j]) > 3 then result := False
		else if trend[j] > 0 then Inc(posCount)
		else if trend[j] < 0 then Inc(negCount)
		else result := False; // zero
	end;
	if (posCount > 0) and (negCount > 0) then result := False;
End;

Procedure SolvePart1(reports: IArr2D);
Var
	i,safeCount: Integer;
	trends: IArr2D;
	isSafe: Boolean;
Begin
	WriteLn('Part 1: How many of the reports are safe?');
	
	safeCount := 0;
	trends := ReportsToTrends(reports);
	for i := 0 to High(trends) do
	begin
		isSafe := IsTrendSafe(trends[i]);
		if isSafe then Inc(safeCount);
	end;
	
	WriteLn(Format('Part One Solution: %d', [safeCount]));
End;

Procedure SolvePart2(reports: IArr2D);
Var
	i,j,safeCount: Integer;
	trend: IArr1D;
	isSafe: Boolean;
Begin
	WriteLn('Part 2: How many reports are safe with the Problem Dampener?');
	
	safeCount := 0;
	
	for i := 0 to High(reports) do
	begin
		trend := ReportToTrend(reports[i]);
		if IsTrendSafe(trend) then
			Inc(safeCount)
		else
			for j := 0 to High(reports[i]) do
			begin
				trend := ReportToTrend(reports[i], j);
				if IsTrendSafe(trend) then
				begin
					Inc(safeCount);
// 					WriteLn( Format('Report %d is made safe by removing value %d', [i,j]));
					break;
				end;
			end;
	end;
	
	WriteLn(Format('Part Two Solution: %d', [safeCount]));
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
	SolvePart1(reports);
	SolvePart2(reports);
End.
