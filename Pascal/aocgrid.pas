// Utility module for Advent of Code
{$mode objfpc} // directive to be used for defining classes
{$m+}          // directive to be used for using constructor

Unit AoCGrid;

Interface

Uses SysUtils, StrUtils, Math, AoCUtils, AoCGeometry, ContNrs;

Type
    //AoCStrPtr = ^String;
    
    GridData = Class
    	Public
	    	Function Glyph(): String; virtual; abstract;
	    	Function StrValue(): String; virtual; abstract;
	    	Function IntValue(): Integer; virtual; abstract;
    end;
    
    GridDataPtr = ^GridData;
    
    GridDataInt = Class(GridData)
    	Private
    		_value: Integer;
    	Public
    		Constructor Create(value: Integer);
    		Property Value: Integer Read _value;
    		Function Glyph(): String; override;
	    	Function StrValue(): String; override;
	    	Function IntValue(): Integer; override;
    end;
    
    GridDataStr = Class(GridData)
    	Private
    		_value: String;
    	Public
    		Constructor Create(value: String);
    		Property Value: String Read _value;
    		Function Glyph(): String; override;
	    	Function StrValue(): String; override;
	    	Function IntValue(): Integer; override;
    end;
    	
    
    Grid2D = Class
    	Private
    		_defaultValue: String;
    		_aRule: Adjacency;
    		_data: TFPHashList;
    		Function GetPtr(coord: Coord2D): Pointer;
    		Procedure SetPtr(ptr: Pointer; coord: Coord2D);
    	Public
    		Constructor Create(default: String; adjacency: Adjacency = rook);
    		Property Default: String Read _defaultValue;
    		Property Rule: Adjacency Read _aRule;
    		Function GetString(coord: Coord2D): String;
    		Procedure SetString(v: String; coord: Coord2D);
    		Function GetInteger(coord: Coord2D): Integer;
    		Procedure SetInteger(v: Integer; coord: Coord2D);
    		Function GetData(coord: Coord2D): GridData;
    		Procedure SetData(v: GridData; coord: Coord2D);
			Function IsEmpty(): Boolean;
    		Function GetExtent(): Extent2D;
    		Function GetCoords(): Coord2DArray;
    		Function GetCoords(withValue: String): Coord2DArray;
    		Function GetHistogram(includeUnset: Boolean = False): AoCIntegerMap;
    		Function GetNeighbourOffsets(): Coord2DArray;
    		Function GetNeighbourCoords(fromCoord: Coord2D): Coord2DArray;
    		Function SPrint(): String;
    		Procedure Print();
    end;

Implementation


Constructor GridDataInt.Create(value: Integer);
begin
	_value := value;
end;

Function GridDataInt.Glyph(): String;
begin
	result := LeftStr(IntToStr(_value), 1);
end;

Function GridDataInt.StrValue(): String;
begin
	result := IntToStr(_value);
end;

Function GridDataInt.IntValue(): Integer;
begin
	result := _value;
end;

Constructor GridDataStr.Create(value: String);
begin
	_value := value;
end;

Function GridDataStr.Glyph(): String;
begin
	result := LeftStr(_value, 1);
end;

Function GridDataStr.StrValue(): String;
begin
	result := _value;
end;

Function GridDataStr.IntValue(): Integer;
begin
	result := StrToInt(_value);
end;


// -------------------------------------------------------
// Grid2D
// -------------------------------------------------------

Constructor Grid2D.Create(default: String; adjacency: Adjacency = rook);
begin
	_defaultValue := default;
	_aRule := adjacency;
	_data := TFPHashList.Create;
end;

Function Grid2D.GetString(coord: Coord2D): String;
Var
	key: String;
	idx: Integer;
	data: GridData;
	ptr: GridDataPtr;
begin
	key := coord.AsKey;
	idx := _data.FindIndexOf(key);
	If idx = -1 Then
		result := _defaultValue
	Else
	begin
		ptr := _data[idx];
		data := ptr^;
		result := data.StrValue;
	end;
end;

Procedure Grid2D.SetString(v: String; coord: Coord2D);
Var
	key: String;
	data: GridData;
	ptr: GridDataPtr;
begin
	// https://www.tutorialspoint.com/pascal/pascal_memory.htm
	New(ptr);
	If Not Assigned(ptr) Then
	begin
		key := coord.AsKey;
		WriteLn('Error - Unable to allocate memory to set ', v, ' at ', key);
		Exit;
	end;
	
	data := GridDataStr.Create(v);
	ptr^ := data;
	SetPtr(ptr, coord);
end;

Function Grid2D.GetInteger(coord: Coord2D): Integer;
Var
	key: String;
	idx: Integer;
	data: GridData;
	ptr: GridDataPtr;
begin
	key := coord.AsKey;
	idx := _data.FindIndexOf(key);
	If idx = -1 Then
		result := StrToInt(_defaultValue)
	Else
	begin
		ptr := _data[idx];
		data := ptr^;
		data.IntValue;
	end;
end;

Procedure Grid2D.SetInteger(v: Integer; coord: Coord2D);
Var
	key: String;
	data: GridData;
	ptr: GridDataPtr;
begin
	// https://www.tutorialspoint.com/pascal/pascal_memory.htm
	New(ptr);
	If Not Assigned(ptr) Then
	begin
		key := coord.AsKey;
		WriteLn('Error - Unable to allocate memory to set ', v, ' at ', key);
		Exit;
	end;
	
	data := GridDataInt.Create(v);
	ptr^ := data;
	SetPtr(ptr, coord);
end;

Function Grid2D.GetData(coord: Coord2D): GridData;
Var
	key: String;
	idx: Integer;
	ptr: GridDataPtr;
begin
	key := coord.AsKey;
	idx := _data.FindIndexOf(key);
	If idx = -1 Then
		result := GridDataStr.Create(_defaultValue)
	Else
	begin
		ptr := _data[idx];
		result := ptr^
	end;
end;

Procedure Grid2D.SetData(v: GridData; coord: Coord2D);
Var
	key: String;
	ptr: GridDataPtr;
begin
	// https://www.tutorialspoint.com/pascal/pascal_memory.htm
	New(ptr);
	If Not Assigned(ptr) Then
	begin
		key := coord.AsKey;
		WriteLn('Error - Unable to allocate memory to set ', v.Glyph, ' at ', key);
		Exit;
	end;
	
	ptr^ := v;
	SetPtr(ptr, coord);
end;

Function Grid2D.GetPtr(coord: Coord2D): Pointer;
Var
	key: String;
	idx: Integer;
begin
	key := coord.AsKey;
	idx := _data.FindIndexOf(key);
	If idx = -1 Then
		result := @_defaultValue
	Else
	begin
		result := _data[idx];
	end;
end;

Procedure Grid2D.SetPtr(ptr: Pointer; coord: Coord2D);
Var
	key: String;
	idx: Integer;
begin
	key := coord.AsKey;
	// There doesn't seem to be a function to replace the value for a key
	idx := _data.FindIndexOf(key);
	If idx <> -1 Then
		_data.Delete(idx);

	_data.Add(key, ptr);
end;

Function Grid2D.IsEmpty(): Boolean;
begin
	result := _data.Count = 0
end;

Function Grid2D.GetExtent(): Extent2D;
begin
	result := Extent2D.Create(GetCoords);
end;

Function Grid2D.GetCoords(): Coord2DArray;
Var
	i: Integer;
	c: Coord2D;
begin
	result := [];
    For i := 0 To _data.Count-1 Do
    begin
    	c := Coord2D.Create(_data.NameOfIndex(i));
    	PushCoord(c, result);
    end;
end;

Function Grid2D.GetCoords(withValue: String): Coord2DArray;
Var
	key: String;
	idx: Integer;
	ptr: GridDataPtr;
	data: GridData;
	c: Coord2D;
begin
	result := [];
    For idx := 0 To _data.Count-1 Do
    begin
    	key := _data.NameOfIndex(idx);
    	ptr := _data[idx];
    	data := ptr^;
    	If data.StrValue = withValue Then
    	begin
			c := Coord2D.Create(key);
			PushCoord(c, result);
    	end;
    end;
end;

Function Grid2D.GetHistogram(includeUnset: Boolean = False): AoCIntegerMap;
Var
	idx: Integer;
	c: Coord2D;
	ptr: GridDataPtr;
	data: GridData;
	val: String;
begin
	result := AoCIntegerMap.Create;
	if includeUnset then
		for c in GetExtent.AllCoords do
		begin
			val := GetString(c);
			If (result.IndexOf(val) = -1) Then
				result[val] := 0;
			result[val] := result[val] + 1;
		end
	else
	begin
		For idx := 0 To _data.Count-1 Do
		begin
			//key := _data.NameOfIndex(idx);
			ptr := _data[idx];
			data := ptr^;
			val := data.StrValue;
			If (result.IndexOf(val) = -1) Then
				result[val] := 0;
			result[val] := result[val] + 1;
		end;
	end;	
end;

Function Grid2D.GetNeighbourOffsets(): Coord2DArray;
begin
	result := [];
	If (_aRule = queen) Or (_aRule = rook) Then
	begin
		PushCoord(Coord2D.XOffset(NORTH), result);
		PushCoord(Coord2D.XOffset(SOUTH), result);
		PushCoord(Coord2D.XOffset(EAST), result);
		PushCoord(Coord2D.XOffset(WEST), result);
	end;
	If (_aRule = queen) Or (_aRule = bishop) Then
	begin
		PushCoord(Coord2D.XOffset(NW), result);
		PushCoord(Coord2D.XOffset(SW), result);
		PushCoord(Coord2D.XOffset(NE), result);
		PushCoord(Coord2D.XOffset(SE), result);
	end;
end;

Function Grid2D.GetNeighbourCoords(fromCoord: Coord2D): Coord2DArray;
Var
	offsets: Coord2DArray;
	i: Integer;
	c: Coord2D;
begin
	result := [];
	offsets := GetNeighbourOffsets;
	For i := 0 To Length(offsets)-1 Do
	begin
		c := offsets[i];
		PushCoord(Coord2D.Create(fromCoord.X+c.X, fromCoord.Y+c.Y), result);
	end;
end;

Function Grid2D.SPrint(): String;
Var
	r, c: Integer;
	ext: Extent2D;
begin
	result := '';
	ext := GetExtent;
	For r := ext.GetMin.Y To ext.GetMax.Y Do
	begin
		For c := ext.GetMin.X To ext.GetMax.X Do
		begin
			result := result + GetData(Coord2D.Create(c, r)).Glyph;
		end;
		result := result + sLineBreak;
	end;
end;


Procedure Grid2D.Print();
begin
	Write(SPrint);
end;



end.