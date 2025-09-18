Program AOC_2024_Day09;
{$mode objfpc} // directive to be used for defining classes
{$m+}		   // directive to be used for using constructor
{$H+}		   // directive to be used for ANSI strings
{$J-}		   // directive to be used for turning off writeable const

Uses SysUtils, StrUtils, AoCUtils, Classes;

Const
	DAY = 9;

Function CheckSumForDiskMap(diskMap: AoCStringArray): Integer;
Var
	blockPtr: Integer;
Begin
	result := 0;
	for blockPtr := 0 to High(diskMap) do
	begin
		if diskMap[blockPtr] = '.' then break;
		result := result + (blockPtr * StrToInt(diskMap[blockPtr]));
	end;
End;

Procedure SolvePart1(diskMap: AoCStringArray);
Var
	spacePtr,blockPtr: Integer;
	checkSum: Integer;
Begin
	WriteLn('Part 1: What is the filesystem checksum after compacting blocks?');
	
	spacePtr := 0; blockPtr := High(diskMap);
	while diskMap[spacePtr] <> '.' do Inc(spacePtr);
	while diskMap[blockPtr] =  '.' do Dec(blockPtr);
	
	while spacePtr < blockPtr do
	begin
		diskMap[spacePtr] := diskMap[blockPtr];
		diskMap[blockPtr] := '.';
		while diskMap[spacePtr] <> '.' do Inc(spacePtr);
		while diskMap[blockPtr] =  '.' do Dec(blockPtr);
	end;
	
	checkSum := CheckSumForDiskMap(diskMap);
	
	WriteLn(Format('Part One Solution: %d', [checkSum]));
End;

Procedure SolvePart2(values: TStringList);
Var
	a, b, c: Integer;
Begin
	WriteLn('Part 2: DESCRIPTION');
	WriteLn(Format('Part Two Solution: %d', [13]));
End;

Function StrToDiskMap(str: String): AoCStringArray;
var
	i,j,id,size: Integer;
	isFile: Boolean;
Begin
	result := [];
	isFile := True;
	id := 0;
		
	for i := 1 to str.Length do
	begin
		size := StrToInt(str[i]);
		for j := 1 to size do
			if isFile then
				PushString(result, IntToStr(id))
			else
				PushString(result, '.');
				
		if isFile then Inc(id);
		isFile := not isFile;
	end;
// 	WriteLn(JoinStrArray('', result));
End;

Var
	iFileName: String;
	input: TStringList;
	diskMap: AoCStringArray;
Begin
	WriteLn('AoC 2015 Day 09: Disk Fragmenter');
	iFileName := InputFileName(DAY, True);
	input := ReadGroupedInput(iFileName, 1);
	diskMap := StrToDiskMap(input[0]);
	SolvePart1(diskMap);
	SolvePart2(input);
End.
