Program AOC_2024_Day<##>;
{$mode objfpc} // directive to be used for defining classes
{$m+}		   // directive to be used for using constructor
{$H+}		   // directive to be used for ANSI strings
{$J-}		   // directive to be used for turning off writeable const

Uses SysUtils, StrUtils, AoCUtils, Classes;

Const
	IN_FILE = '../Input/day<##>_test.txt';
//	IN_FILE = '../Input/day<##>_challenge.txt';

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
	input: TStringList;
Begin
	WriteLn('AoC 2015 Day <##>: <##>');
	input := ReadGroupedInput(IN_FILE, 0);
	SolvePart1(input);
	SolvePart2(input);
End.
