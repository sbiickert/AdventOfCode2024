Program AOC_2024_Day11;
{$mode objfpc} // directive to be used for defining classes
{$m+}		   // directive to be used for using constructor
{$H+}		   // directive to be used for ANSI strings
{$J-}		   // directive to be used for turning off writeable const

Uses SysUtils, StrUtils, AoCUtils, Classes;

Const
	DAY = 11;

Var
	memo: AoCMemo;

Function CountStones(stone: Int64; blinks: Integer): Int64;
Var
	stoneStr: String;
	key: String;
	left,right: Int64;
	len: Integer;
Begin
	//WriteLn('CountStones ', stone, ' ', blinks);
	result := 0;
	stoneStr := IntToStr(stone);
	key := stoneStr + ' ' + IntToStr(blinks);

	if memo.KeyExists(key) then
	begin
		//WriteLn('memo hit');
		result := memo.GetValue(key);
		exit;
	end;

	if blinks <= 0 then
	begin
		//WriteLn('zero blinks');
		result := 1;
		memo.SetKeyValue(key, 1);
		exit;
	end;
	
	if stone = 0 then
	begin
		//WriteLn('stone zero');
		result := CountStones(1, blinks-1);
		memo.SetKeyValue(key, result);
	end
	else if Length(stoneStr) mod 2 = 0 then
	begin
		//WriteLn('split ', stoneStr, ' in two');
		len := Trunc(Length(stoneStr) / 2);
		left := StrToInt(AnsiLeftStr(stoneStr, len));
		right := StrToInt(AnsiRightStr(stoneStr, len));
		result := CountStones(left, blinks-1) + CountStones(right, blinks-1);
		memo.SetKeyValue(key, result);
	end
	else
	begin
		//WriteLn('multiply by 2024');
		result := CountStones(stone * 2024, blinks-1);
		memo.SetKeyValue(key, result);
	end;
End;


Function CountAllStones(stones: AoCIntArray; blinks: Integer): Int64;
Var
	stone: Integer;
Begin
	result := 0;
	for stone in stones do
		result := result + CountStones(stone, blinks);
End;

Procedure SolveParts(stones: AoCIntArray);
Var
	count: Int64;
Begin
	WriteLn('Part 1: How many stones after 25 blinks?');
	count := CountAllStones(stones, 25);
	WriteLn(Format('Part One Solution: %d', [count]));
	WriteLn('Part 2: How many stones after 75 blinks?');
	count := CountAllStones(stones, 75);
	WriteLn(Format('Part Two Solution: %d', [count]));
End;

Function ParseStones(input: String): AoCIntArray;
Var
	sArr: TStringArray;
	s: String;
Begin
	sArr := SplitString(input, ' ');
	result := [];
	for s in sArr do
		PushInt(result, StrToInt(s));
End;

Var
	iFileName: String;
	input: TStringList;
	stones: AoCIntArray;
Begin
	WriteLn('AoC 2015 Day 11: Plutonian Pebbles');
	iFileName := InputFileName(DAY, False);
	input := ReadGroupedInput(iFileName, 0);
	stones := ParseStones(input[0]);
	memo := AoCMemo.Create;
	SolveParts(stones);
End.
