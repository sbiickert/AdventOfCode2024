Program AOC_2024_Day03;
{$mode objfpc} // directive to be used for defining classes
{$m+}		   // directive to be used for using constructor
{$H+}		   // directive to be used for ANSI strings
{$J-}		   // directive to be used for turning off writeable const

Uses SysUtils, StrUtils, AoCUtils, RegExpr, Classes;

Const
	DAY = 3;
	
Var
	re: TRegExpr;

Function GetMul(pgm: String): Integer;
Var
	a,b: Integer;
Begin
	result := 0;
	
	If re.Exec(pgm) Then
    Begin
    	a := StrToInt(re.Match[1]);
    	b := StrToInt(re.Match[2]);
    	result := a * b;
		While re.ExecNext Do
		Begin
			a := StrToInt(re.Match[1]);
			b := StrToInt(re.Match[2]);
			result := result + a * b;
		End;
    End;
End;

Procedure SolvePart1(pgm: String);
Var
	sum: Integer;
Begin
	WriteLn('Part 1: What is the sum of the multiplication commands?');
	
	sum := GetMul(pgm);
	
	WriteLn(Format('Part One Solution: %d', [sum]));
End;

Procedure SolvePart2(pgm: String);
Var
	i,sum: Integer;
	sections: TStringArray;
	section: String;
Begin
	WriteLn('Part 2: What is the sum, taking disabled sections into account?');
	
	sum := 0;
	
	sections := SplitString(pgm, 'do()');
	for i := 0 to High(sections) do
	begin
		section := SplitString(sections[i], 'don''t()')[0];
		sum := sum + GetMul(section);
	end;

	WriteLn(Format('Part Two Solution: %d', [sum]));
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
	
	re := TRegExpr.Create('mul\((\d+),(\d+)\)');

	SolvePart1(singleLine);
	SolvePart2(singleLine);
End.
