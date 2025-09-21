// Utility module for Advent of Code
{$mode objfpc} // directive to be used for defining classes
{$m+}          // directive to be used for using constructor
{$H+}		   // directive to be used for ANSI strings

Unit AoCUtils;

Interface

Uses SysUtils, StrUtils, fgl, Classes, Types, ContNrs;

Const
	INPUT_PATH = '../Input';


Type
    AoCStringArray =     TStringDynArray;
    AoCIntArray =        TInt64DynArray;
    AoCStringListGroup = array of TStringList;
    AoCStringMap =       specialize TFPGMap<String, String>;
    AoCIntegerMap =      specialize TFPGMap<String, Int64>;
    AoCWordMap =      	 specialize TFPGMap<String, Word>;
    
    AoCCache = Class
    	private
    		_hash: TFPHashList;
    	public
    		Constructor Create();
    		Procedure SetKeyValue(key: String; value: Int64);
    		Function GetValue(key: String): Int64;
    		Function KeyExists(key: String): Boolean;
    end;
    
	TCompareFunc = function (const i1,i2:Integer): Integer;

	TPermutator = Class
		private
			_originalValues: AoCIntArray;
			_values: AoCIntArray;
			_c: AoCIntArray;
			_i: Integer;
			procedure _Swap(idx1, idx2: Integer);
		public
			constructor Create(values: AoCIntArray);
			function NextPermutation(): AoCIntArray;
			procedure Reset();
	end;

Function AssertStrEqual(actual: String; expected: String; description: String): Boolean;
Function AssertIntEqual(actual: Int64; expected: Int64; description: String): Boolean;
Function AssertTrue(value: Boolean; description: String): Boolean;
Function AssertFalse(value: Boolean; description: String): Boolean;

Function InputFileName(day:Integer; isTest: Boolean): String;
Function ReadInput(inputFilename: String):   TStringList;
Function ReadGroupedInput(inputFilename: String):   AoCStringListGroup;
Function ReadGroupedInput(inputFilename: String; index: Integer):	TStringList;

Function StrListToStrArray(input: TStringList): AoCStringArray;
Function StrListToIntArray(input: TStringList): AoCIntArray;
Function StrArrayToIntArray(var input: AoCStringArray): AoCIntArray;
Function IntArrayToStrArray(var input: AoCIntArray): AoCStringArray;

Function JoinStrArray(delim: String; input: AoCStringArray): String;
Function SumIntArray(var input: AoCIntArray): Int64;
Function CopyIntArray(input: AoCIntArray): AoCIntArray;
Procedure PushInt(var input: AoCIntArray; value: Int64);
Procedure PushString(var input: AoCStringArray; value: String);
Function InitIntArray(size, value: Int64): AoCIntArray;
Procedure SortIntArray(var arr: AoCIntArray; ascending: Boolean);
Function SliceIntArray(input: AoCIntArray; start: Integer; length: Integer): AoCIntArray;
Function FrequencyMap(values: AoCStringArray): AoCIntegerMap;

Function GCD(a, b: Int64): Int64;
Function LCM(values: AoCIntArray): Int64;
Function ApproxEqual(d1, d2, allowance: Double): Boolean;

Implementation

Function AssertStrEqual(actual: String; expected: String; description: String): Boolean;
begin
	Write(description, '... ');
	if actual <> expected then
	begin
		WriteLn( Format('String %s does not equal %s', [actual, expected] ));
		result := False;
	end
	else
	begin
		WriteLn('ok');
		result := True;
	end
end;

Function AssertIntEqual(actual: Int64; expected: Int64; description: String): Boolean;
begin
	Write(description, '... ');
	if actual <> expected then
	begin
		WriteLn( Format('Integer %d does not equal %d', [actual, expected] ));
		result := False;
	end
	else
	begin
		WriteLn('ok');
		result := True;
	end
end;

Function AssertTrue(value: Boolean; description: String): Boolean;
begin
	Write(description, '... ');
	if value = False then
	begin
		WriteLn( 'Asserted value is not True');
		result := False;
	end
	else
	begin
		WriteLn('ok');
		result := True;
	end
end;

Function AssertFalse(value: Boolean; description: String): Boolean;
begin
	Write(description, '... ');
	if value = True then
	begin
		WriteLn( 'Asserted value is not False');
		result := False;
	end
	else
	begin
		WriteLn('ok');
		result := True;
	end
end;


Function InputFileName(day:Integer; isTest: Boolean): String;
Begin
	If isTest Then result := Format('%s/day%2.2d_test.txt', [INPUT_PATH, day])
	Else result := Format('%s/day%2.2d_challenge.txt', [INPUT_PATH, day]);
End;

Function ReadInput(inputFilename: String): TStringList;
Begin
    WriteLn('Will read data from: ', inputFilename);
	result := TStringList.Create;
    result.LoadFromFile(inputFilename);
End;

Function ReadGroupedInput(inputFilename: String): AoCStringListGroup;
Var
    allInput, group: TStringList;
    i: Integer;
Begin
	result := [];
    SetLength(result, 0);
    
    allInput := ReadInput(inputFilename);
    group := TStringList.Create;
    
	For i := 0 To allInput.Count-1 Do
	Begin
		If Length(allInput[i]) = 0 Then
		Begin
			SetLength(result, Length(result)+1);
			result[Length(result)-1] := group;
			group := TStringList.Create;
			continue;
		End;
		group.Add(allInput[i]);
	End;

	If group.Count > 0 Then
	Begin
		SetLength(result, Length(result)+1);
		result[Length(result)-1] := group;
	End;
End;

Function ReadGroupedInput(inputFilename: String; index: Integer):	TStringList;
Var
	allGroups: AoCStringListGroup;
Begin
	result := TStringList.Create;
	
	allGroups := ReadGroupedInput(inputFilename);
	
	If Length(allGroups) > index Then
		result := allGroups[index];
End;

Function StrListToStrArray(input: TStringList): AoCStringArray;
Var
	i: Integer;
Begin
	result := [];
	SetLength(result, input.Count);
	For i := 0 To input.Count-1 Do
		result[i] := input[i];
End;

{ Utility function to transform an list of strings to array of ints }
Function StrListToIntArray(input: TStringList): AoCIntArray;
Var
	i: Integer;
Begin
	result := [];
	SetLength(result, input.Count);
	For i := 0 To input.Count-1 Do
		result[i] := StrToInt64(input[i]);
End;

{ Utility function to transform an array of strings to array of ints }
Function StrArrayToIntArray(var input: AoCStringArray): AoCIntArray;
Var
    i: Integer;
Begin
	result := [];
    SetLength(result, Length(input));
    For i := 0 To Length(input)-1 Do
        result[i] := StrToInt64(input[i]);
End;

Function IntArrayToStrArray(var input: AoCIntArray): AoCStringArray;
Var
    i: Integer;
Begin
	result := [];
    SetLength(result, Length(input));
    For i := 0 To Length(input)-1 Do
        result[i] := IntToStr(input[i]);
End;


Function JoinStrArray(delim: String; input: AoCStringArray): String;
var
	i: Integer;
begin
	result := '';
	for i := 0 to Length(input)-1 do
	begin
		result := Concat(result, input[i]); // could use +
		if i < Length(input)-1 then
			result := result + delim;
	end;
end;

Function SumIntArray(var input: AoCIntArray): Int64;
Var
	i: Integer;
Begin
	result := 0;
	For i := 0 To Length(input)-1 Do
		result := result + input[i];
End;

Function CopyIntArray(input: AoCIntArray): AoCIntArray;
begin
	result := input; // Was passed in by value
end;

procedure PushInt(var input: AoCIntArray; value: Int64);
begin
	SetLength(input, Length(input)+1);
	input[Length(input)-1] := value;
end;

Procedure PushString(var input: AoCStringArray; value: String);
begin
	SetLength(input, Length(input)+1);
	input[Length(input)-1] := value;
end;

Function InitIntArray(size, value: Int64): AoCIntArray;
var
	i: Integer;
begin
	result := [];
	SetLength(result, size);
	for i := 0 to Length(result)-1 do
		result[i] := value;
end;

Function SliceIntArray(input: AoCIntArray; start: Integer; length: Integer): AoCIntArray;
var
	offset,i: Integer;
begin
	offset := Low(input) - start;
	result := [];
	SetLength(result, length);
	for i := start to start + length - 1 do
		result[i+offset] := input[i];
end;

Function FrequencyMap(values: AoCStringArray): AoCIntegerMap;
var
	i: Integer;
begin
	result := AoCIntegerMap.Create;
	
	for i := Low(values) to High(values) do
	begin
		if result.IndexOf(values[i]) = -1 then
			result[values[i]] := 0;
		result[values[i]] := result[values[i]] + 1;
	end
end;

// Sorting Arrays of Integer

// Private
function CompareIntAsc(const i1,i2:Integer): integer;
begin
  if i1=i2 then Result:=0
  else if i1<i2 then Result:=-1
  else Result:=1;
end;

// Private
function CompareIntDesc(const i1,i2:Integer): integer;
begin
  if i1=i2 then Result:=0
  else if i1>i2 then Result:=-1
  else Result:=1;
end;

// Private
function QSPartition(var arr: AoCIntArray; low:Integer; high:Integer; comp:TCompareFunc):Integer;
var
	pivot,i,j,temp: Integer;
begin
	pivot := arr[high];
	i := (low - 1); 
	for j := low to high-1 do begin
		if comp(arr[j], pivot) < 0 then begin
			Inc(i);
			// Swap arr[i] and arr[j]
			temp := arr[i];
			arr[i] := arr[j];
			arr[j] := temp;
		end;
	end;
	temp := arr[i+1];
	arr[i+1] := arr[high];
	arr[high] := temp;
	
	result := i+1;
end;

// Private
procedure QuickSortIntArray(var arr: AoCIntArray; low: Integer; high: Integer; comp:TCompareFunc);
var
	pi: Integer;
begin
	if low < high then begin
		// Partitioning index
		pi := QSPartition(arr, low, high, comp);
		
		// Recursively sort elements before, after partition
		QuickSortIntArray(arr, low, pi-1, comp);
		QuickSortIntArray(arr, pi+1, high, comp);
	end;
end;

// Public
procedure SortIntArray(var arr: AoCIntArray; ascending: Boolean);
begin
	if ascending then
		QuickSortIntArray(arr, 0, Length(arr)-1, @CompareIntAsc)
	else
		QuickSortIntArray(arr, 0, Length(arr)-1, @CompareIntDesc);
end;

constructor TPermutator.Create(values: AoCIntArray);
begin
	_originalValues := values;
	
	_values := [];
	SetLength(_values, Length(values));
	_c := [];
	SetLength(_c, Length(values));
	
	self.Reset;
end;

procedure TPermutator._Swap(idx1, idx2: Integer);
var
	temp: Integer;
begin
	temp := _values[idx1];
	_values[idx1] := _values[idx2];
	_values[idx2] := temp;
end;

function TPermutator.NextPermutation(): AoCIntArray;
var
	n: Integer;
begin
	// https://en.wikipedia.org/wiki/Heap%27s_algorithm
	n := Length(_originalValues);
	if _i >= n then begin
		result := [];
		exit;
	end;
	
	while _i < n do begin
		if _c[_i] < _i then begin
			if (_i mod 2) = 0 then
				self._Swap(0, _i)
			else
				self._Swap(_c[_i], _i);
			result := _values;
			Inc(_c[_i]);
			_i := 1;
			exit;
		end
		else begin
			_c[_i] := 0;
			Inc(_i);
		end;
	end;
end;

procedure TPermutator.Reset();
var
	i: Integer;
begin
	_i := 1;
	for i := 0 to Length(_values)-1 do begin
		_c[i] := 0;
		_values[i] := _originalValues[i];
	end;
end;

Function GCD(a, b: Int64): Int64;
var
	temp: Int64;
begin
	while b <> 0 do
	begin
		temp := b;
		b := a mod b;
		a := temp
	end;
	result := a;
end;

Function LCM(values: AoCIntArray): Int64;
var
	next,g,running: Integer;
	nextValues: AoCIntArray;
begin
	if Length(values) = 0 then
		result := 0
	else if Length(values) = 1 then
		result := values[0]
	else
	begin
		next := values[1];
		g := GCD(values[0],values[1]);
		running := values[0] div g * next;
		nextValues := Concat([running], SliceIntArray(values, 2, Length(values) - 2)); 
		result := LCM(nextValues)
	end
end;

Function ApproxEqual(d1, d2, allowance: Double): Boolean;
begin
	result := Abs(d1-d2) <= allowance;
end;


Constructor AoCCache.Create();
Begin
	_hash := TFPHashList.Create;
End;

Procedure AoCCache.SetKeyValue(key: String; value: Int64);
Var
	idx: Integer;
	ptr: PInt64;
Begin
	New(ptr);
	If Not Assigned(ptr) Then
	begin
		WriteLn('Cache Error - Unable to allocate memory to set ', value, ' at ', key);
		Exit;
	end;
	
    ptr^ := value;
	idx := _hash.FindIndexOf(key);
	If idx <> -1 Then
		_hash.Delete(idx);

    _hash.Add(key, ptr);
End;

Function AoCCache.GetValue(key: String): Int64;
Var
	idx: Integer;
	ptr: PInt64;
Begin
	idx := _hash.FindIndexOf(key);
	if idx <> -1 then
	begin
		ptr := PInt64(_hash[idx]);
		result := ptr^;
	end
	else
	begin
		WriteLn('Key ', key , ' does not exist, returning -1');
		result := -1;
	end;
// 	WriteLn(key, ' ', idx, ' ', result);
End;

Function AoCCache.KeyExists(key: String): Boolean;
Begin
	result := _hash.FindIndexOf(key) <> -1;
End;


End.
