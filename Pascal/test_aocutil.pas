Program AocTest;

Uses
	AoCUtils, Classes;

Const
	INPUT_DAY = 0;

Procedure TestReadInput();
Var
	iFileName: String;
	input: TStringList;
Begin
	iFileName := InputFileName(0, False);
	AssertStrEqual(iFileName, '../Input/day00_challenge.txt', 'Checking challenge input filename creation');
	iFileName := InputFileName(0, True);
	AssertStrEqual(iFileName, '../Input/day00_test.txt', 'Checking test input filename creation');
	
	input := ReadInput(iFileName);
	AssertIntEqual(input.Count, 10, 'Checking input line count');
	AssertStrEqual(input[0], 'G0, L0', 'Checking the first line of input');
	AssertStrEqual(input[3], '', 'Checking an empty line of input');
End;

Procedure TestReadGroupedInput();
Var
	iFileName: String;
	input: AoCStringListGroup;
Begin
	iFileName := InputFileName(0, True);
	input := ReadGroupedInput(iFileName);
	AssertIntEqual(Length(input), 3, 'Checking number of groups');
	AssertIntEqual(input[0].Count, 3, 'Checking length of group 0');
	AssertIntEqual(input[1].Count, 2, 'Checking length of group 1');
	AssertIntEqual(input[2].Count, 3, 'Checking length of group 2');
	AssertStrEqual(input[2][0], 'G2, L0', 'Checking group 2 line 0');
End;

Procedure TestArrayOperations;
Var
	sArr: AoCStringArray;
	iArr,iArr2,iArrCopy: AoCIntArray;
	sList: TStringList;
	s: string;
	iMap: AoCIntegerMap;
Begin
	sArr := ['1', '2', '3'];
	iArr := StrArrayToIntArray(sArr);
	AssertIntEqual(SumIntArray(iArr), 6, 'Checking array sum');
	
	sList := TStringList.Create;
	sList.Delimiter := ',';
	sList.DelimitedText := '14,5,6';
	iArr := StrListToIntArray(sList);
	AssertIntEqual(Length(iArr), 3, 'Checking parsed ints length is 3');
	sArr := StrListToStrArray(sList);
	AssertIntEqual(Length(sArr), 3, 'Checking string array length is 3');
	sList.Free;

	AssertIntEqual(SumIntArray(iArr), 25, 'Checking array sum');
	SortIntArray(iArr, true);
	sArr := IntArrayToStrArray(iArr);
	s := JoinStrArray(',', sArr);
	AssertStrEqual(s, '5,6,14', 'Checking array sorting');
	SortIntArray(iArr, false);
	sArr := IntArrayToStrArray(iArr);
	s := JoinStrArray(',', sArr);
	AssertStrEqual(s, '14,6,5', 'Checking array reverse sorting');
	
	iArrCopy := CopyIntArray(iArr);
	PushInt(iArrCopy, 2);
	AssertIntEqual(Length(iArrCopy), 4, 'Checking array length after push');
	AssertIntEqual(iArrCopy[3], 2, 'Checking pushed int value');
	AssertIntEqual(Length(iArr), 3, 'Checking original array was unaffected');
	
	iArr := InitIntArray(10, 3);
	AssertIntEqual(Length(iArr), 10, 'Check length of initialized array');
	sArr := IntArrayToStrArray(iArr);
	s := JoinStrArray(',', sArr);
	AssertStrEqual(s, '3,3,3,3,3,3,3,3,3,3', 'Checking init array content');
	
	iArr := [0,1,2,3,4];
	iArrCopy := SliceIntArray(iArr, 2, 2);
	AssertIntEqual(Length(iArrCopy), 2, 'Checking length of array slice');
	AssertIntEqual(iArrCopy[0], 2, 'Checking first value of array slice');
	AssertIntEqual(Length(iArr), 5, 'Checking original array is unchanged');
	sArr := IntArrayToStrArray(iArrCopy);
	s := JoinStrArray(',', sArr);
	AssertStrEqual(s, '2,3', 'Check array slice');
	iArr2 := [13,14];
	iArrCopy := Concat(iArr2, SliceIntArray(iArr, 1, 3));
	sArr := IntArrayToStrArray(iArrCopy);
	s := JoinStrArray(',', sArr);
	AssertStrEqual(s, '13,14,1,2,3', 'Check concatenating to slice');
	
	sArr := ['a','b','c','a','b','a','x'];
	iMap := FrequencyMap(sArr);
	AssertIntEqual(iMap['a'], 3, 'Checking frequency of a');
	AssertIntEqual(iMap['b'], 2, 'Checking frequency of b');
	AssertIntEqual(iMap['x'], 1, 'Checking frequency of x');

End;

Procedure TestMath;
var
	iArr: AoCIntArray;
const
	dVal = 1.2345;
begin
	AssertIntEqual( GCD(2, 4), 2, 'Check GCD of 2 and 4');
	AssertIntEqual( GCD(15, 20), 5, 'Check GCD of 15 and 20');
	AssertIntEqual( GCD(13, 20), 1, 'Check GCD of 13 and 20');
	iArr := [2,3,4];
	AssertIntEqual( LCM(iArr), 12, 'Check LCM of 2,3,4');
	iArr := [3,4,13];
	AssertIntEqual( LCM(iArr), 156, 'Check LCM of 3,4,13');
	
	AssertTrue( ApproxEqual(dVal, 1.21, 0.1), 'Checking approximation');
	AssertFalse( ApproxEqual(dVal, 1.21, 0.01), 'Checking approximation');
end;

Procedure TestMemo;
var
	cache: AoCMemo;
	value: Integer;
	ptr: PInt64;
begin
	cache := AoCMemo.Create;
	AssertFalse(cache.KeyExists('boo'), 'Checking nonexistent key does not exist');
	value := 13;
	cache.SetKeyValue('boo', value);
	AssertTrue(cache.KeyExists('boo'), 'Checking key exists');
	value := cache.GetValue('boo');
	AssertIntEqual(value, 13, 'Checking cached value');
end;

Begin
	TestReadInput;
	TestReadGroupedInput;
	TestArrayOperations;
	TestMath;
	TestMemo;
End.
