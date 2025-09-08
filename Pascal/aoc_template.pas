Program AOC_2024_Day<##>;
{$mode objfpc} // directive to be used for defining classes
{$m+}		   // directive to be used for using constructor
{$H+}		   // directive to be used for ANSI strings
{$J-}		   // directive to be used for turning off writeable const

Uses SysUtils, StrUtils, AoCUtils, Classes;

Const
	DAY = <##>;

Procedure SolvePart1(values: TStringList);
Var
	x: Integer;
Begin
	WriteLn('Part 1: DESCRIPTION');
	WriteLn(Format('Part One Solution: %d', [13]));
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
Begin
	WriteLn('AoC 2015 Day <##>: <##>');
	iFileName := InputFileName(DAY, True);
	input := ReadInput(iFileName);
	SolvePart1(input);
	SolvePart2(input);
End.
