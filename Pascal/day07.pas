Program AOC_2024_Day07;
{$mode objfpc} // directive to be used for defining classes
{$m+}		   // directive to be used for using constructor
{$H+}		   // directive to be used for ANSI strings
{$J-}		   // directive to be used for turning off writeable const

Uses SysUtils, StrUtils, AoCUtils, Classes;

Const
	DAY = 7;
	
Type
	OpD7 = (ADD, MUL, CAT);
	OpD7Array = array of OpD7;

Function ParseLine(line: String): AoCIntArray;
var
	noColon: String;
	sArr: AoCStringArray;
	iArr: AoCIntArray;
	i: Integer;
Begin
	noColon := ReplaceText(line, ':', '');
	sArr := SplitString(noColon, ' ');
	iArr := StrArrayToIntArray(sArr);
	result := [iArr[0]];
	for i := High(iArr) downto 1 do PushInt(result, iArr[i]);
End;

Function DoOp(v1,v2: Int64; op: OpD7): Int64;
Begin
	case op of
	ADD: result := v1 + v2;
	MUL: result := v1 * v2;
	CAT: result := StrToInt64(IntToStr(v2) + IntToStr(v1)); // Left to Right
	end;
End;

Function IsEquationValid(eq: AoCIntArray; ops: OpD7Array): Boolean;
Var
	newEq: AoCIntArray;
	op: OpD7;
Begin
	if (Length(eq) = 2) then
	begin
		result := eq[0] = eq[1];
		exit;
	end;
	
	for op in ops do
	begin
		newEq := SliceIntArray(eq, 0, Length(eq)-2);
		PushInt(newEq, DoOp(eq[High(eq)-1], eq[High(eq)], op));
		result := IsEquationValid(newEq, ops);
		if result = true then break;
	end;
End;


Procedure SolvePart1(values: TStringList);
Var
	eq: AoCIntArray;
	sum: Int64;
	line: String;
	ops: OpD7Array;
Begin
	WriteLn('Part 1: What is the sum of the results of valid equations (+ and *)?');
	
	sum := 0;
	ops := [ADD, MUL];
	
	for line in values do
	begin
		eq := ParseLine(line);
		if IsEquationValid(eq, ops) then
		begin
			sum := sum + eq[0];
		end;
	end;
	
	WriteLn(Format('Part One Solution: %d', [sum]));
End;

Procedure SolvePart2(values: TStringList);
Var
	eq: AoCIntArray;
	sum: Int64;
	line: String;
	ops: OpD7Array;
Begin
	WriteLn('Part 2: What is the sum of the results of valid equations (including concatenate)?');
	
	sum := 0;
	ops := [CAT, ADD, MUL];
	
	for line in values do
	begin
		eq := ParseLine(line);
		if IsEquationValid(eq, ops) then
		begin
			sum := sum + eq[0];
		end;
	end;
	
	WriteLn(Format('Part Two Solution: %d', [sum]));
End;

Var
	iFileName: String;
	input: TStringList;
Begin
	WriteLn('AoC 2015 Day 7: Bridge Repair');
	iFileName := InputFileName(DAY, False);
	input := ReadInput(iFileName);
	SolvePart1(input);
	SolvePart2(input);
End.
