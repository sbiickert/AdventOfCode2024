Program AOC_2024_Day03;
{$mode objfpc} // directive to be used for defining classes
{$m+}		   // directive to be used for using constructor
{$H+}		   // directive to be used for ANSI strings
{$J-}		   // directive to be used for turning off writeable const

Uses SysUtils, StrUtils, AoCUtils, RegExpr, Classes;

Const
	DAY = 3;

Procedure SolvePart1(pgm: String);
Var
	re: TRegExpr;
	sum,a,b,product: Integer;
Begin
	WriteLn('Part 1: What is the sum of the multiplication commands?');
	
	WriteLn(pgm);
	sum := 0;
	
	re := TRegExpr.Create('mul\((\d+),(\d+)\)');
	If re.Exec(pgm) Then
    Begin
    	a := StrToInt(re.Match[1]);
    	b := StrToInt(re.Match[2]);
    	product := a * b;
    	sum := product;
		While re.ExecNext Do
		Begin
			a := StrToInt(re.Match[1]);
			b := StrToInt(re.Match[2]);
			product := a * b;
			sum := sum + product;
		End;
    End;
	
	WriteLn(Format('Part One Solution: %d', [sum]));
End;

Procedure SolvePart2(pgm: String);
Var
	a, b, c: Integer;
Begin
	WriteLn('Part 2: DESCRIPTION');
	WriteLn(Format('Part Two Solution: %d', [13]));
End;

Var
	iFileName: String;
	groups: AoCStringListGroup;
	input: TStringList;
	sArr: AoCStringArray;
	singleLine: String;
Begin
	WriteLn('AoC 2015 Day 3: Mull It Over');
	iFileName := InputFileName(DAY, False);
	groups := ReadGroupedInput(iFileName);
	input := groups[0];
	sArr := StrListToStrArray(input);
	singleLine := JoinStrArray('', sArr);
	SolvePart1(singleLine);
	SolvePart2(singleLine);
End.
