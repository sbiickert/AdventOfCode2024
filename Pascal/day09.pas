Program AOC_2024_Day09;
{$mode objfpc} // directive to be used for defining classes
{$m+}		   // directive to be used for using constructor
{$H+}		   // directive to be used for ANSI strings
{$J-}		   // directive to be used for turning off writeable const

Uses SysUtils, StrUtils, AoCUtils, Classes;

Const
	DAY = 9;

Type
	FileInfo = Record
		id: String;
		start: Integer;
		size: Integer;
	end;
	FileInfoArray = array of FileInfo;

Function CheckSumForDiskMap(diskMap: AoCStringArray): Int64;
Var
	blockPtr: Integer;
Begin
	result := 0;
	for blockPtr := 0 to High(diskMap) do
	begin
		if diskMap[blockPtr] = '.' then continue;
// 		WriteLn(Format('%d + (%d * %s)', [result, blockPtr, diskMap[blockPtr]]));
		result := result + (blockPtr * StrToInt(diskMap[blockPtr]));
	end;
End;

Procedure SolvePart1(diskMap: AoCStringArray);
Var
	spacePtr,blockPtr: Integer;
	checkSum: Int64;
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


Function GetFileInfo(diskMap: AoCStringArray): FileInfoArray;
Var
	ptrStart,ptr: Integer;
	id,valueAtPtr: String;
	info: FileInfo;
Begin
	result := [];
	id := '.';
	ptrStart := -1;
	PushString(diskMap, '.'); // Add a space at the end 
	
	for ptr := 0 to High(diskMap) do
	begin
		valueAtPtr := diskMap[ptr];
		if valueAtPtr <> id then
		begin
			if id <> '.' then
			begin
				info.id := id;
				info.start := ptrStart;
				info.size := ptr - ptrStart;
				SetLength(result, Length(result)+1);
				result[High(result)] := info;
			end;
			id := valueAtPtr;
			ptrStart := ptr;
		end;
	end;
End;

Function FindFirstSpace(diskMap: AoCStringArray; requestedSize: Integer): Integer;
Var
	ptrStart,ptr: Integer;
	id,valueAtPtr: String;
	spaceSize: Integer;
Begin
	id := '';
	result := -1;
	ptrStart := -1;
	
	for ptr := 0 to High(diskMap) do
	begin
		valueAtPtr := diskMap[ptr];
		if valueAtPtr <> id then
		begin
			if id = '.' then
			begin
				spaceSize := ptr - ptrStart;
				if spaceSize >= requestedSize then
				begin
					result := ptrStart;
					exit;
				end;
			end;
			id := valueAtPtr;
			ptrStart := ptr;
		end;
	end;
End;


Procedure SolvePart2(diskMap: AoCStringArray);
Var
	fiArr: FileInfoArray;
	fi: FileInfo;
	i,j,spaceStart,s,f: Integer;
	checkSum: Int64;
Begin
	WriteLn('Part 2: What is the filesystem checksum after compacting files?');
	
	fiArr := GetFileInfo(diskMap);
	for i := High(fiArr) downto Low(fiArr) do
	begin
		fi := fiArr[i];
		spaceStart := FindFirstSpace(diskMap, fi.size);
		if (spaceStart >= 0) and (spaceStart < fi.start) then
		begin
			s := spaceStart; f := fi.start;
			for j := 1 to fi.size do
			begin
				diskMap[s] := diskMap[f];
				diskMap[f] := '.';
				Inc(s); Inc(f);
			end;
		end;
// 		WriteLn(JoinStrArray('', diskMap));
	end;
	
	checkSum := CheckSumForDiskMap(diskMap);
	
	WriteLn(Format('Part Two Solution: %d', [checkSum]));
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
	iFileName := InputFileName(DAY, False);
	input := ReadGroupedInput(iFileName, 0);
	diskMap := StrToDiskMap(input[0]);
	SolvePart1(diskMap);
	diskMap := StrToDiskMap(input[0]);
	SolvePart2(diskMap);
End.
