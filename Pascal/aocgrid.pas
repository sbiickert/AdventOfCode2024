// Utility module for Advent of Code
{$mode objfpc} // directive to be used for defining classes
{$m+}          // directive to be used for using constructor

Unit AoCGrid;

Interface

Uses SysUtils, StrUtils, Math, AoCUtils, AoCGeometry, ContNrs;

Type
    AoCStrPtr = ^String;
    
    Grid2D = Class
    	Private
    		_defaultValue: String;
    		_aRule: Adjacency;
    		_data: TFPHashList;
    	Public
    		Constructor Create(default: String; adjacency: Adjacency = rook);
    		Function GetString(coord: Coord2D): String;
    		Procedure SetString(v: String; coord: Coord2D);
    		Function GetInteger(coord: Coord2D): Integer;
    		Procedure SetInteger(v: Integer; coord: Coord2D);
    		Function GetPtr(coord: Coord2D): Pointer;
    		Procedure SetPtr(ptr: Pointer; coord: Coord2D);
    		Function GetExtent(): Extent2D;
    		Function GetCoords(): Coord2DArray;
    		Function GetCoords(withValue: String): Coord2DArray;
    		Function GetHistogram(): AoCIntegerMap;
    		Function GetNeighbourOffsets(): Coord2DArray;
    		Function GetNeighbourCoords(fromCoord: Coord2D): Coord2DArray;
    		Procedure Print();
    end;

Implementation

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
	strPtr: AoCStrPtr;
	idx: Integer;
begin
	key := coord.AsKey;
	idx := _data.FindIndexOf(key);
	If idx = -1 Then
		result := _defaultValue
	Else
	begin
		strPtr := _data[idx];
		result := strPtr^;
	end;
end;

Procedure Grid2D.SetString(v: String; coord: Coord2D);
Var
	key: String;
	ptr: ^String;
begin
	// https://www.tutorialspoint.com/pascal/pascal_memory.htm
	New(ptr);
	If Not Assigned(ptr) Then
	begin
		key := coord.AsKey;
		WriteLn('Error - Unable to allocate memory to set ', v, ' at ', key);
		Exit;
	end;
	
	ptr^ := v;
	SetPtr(ptr, coord);
end;

Function Grid2D.GetInteger(coord: Coord2D): Integer;
Var
	key: String;
	strPtr: AoCStrPtr;
	idx: Integer;
begin
	key := coord.AsKey;
	idx := _data.FindIndexOf(key);
	If idx = -1 Then
		result := StrToInt(_defaultValue)
	Else
	begin
		strPtr := _data[idx];
		result := StrToInt(strPtr^);
	end;
end;

Procedure Grid2D.SetInteger(v: Integer; coord: Coord2D);
Var
	key: String;
	ptr: ^String;
begin
	// https://www.tutorialspoint.com/pascal/pascal_memory.htm
	New(ptr);
	If Not Assigned(ptr) Then
	begin
		key := coord.AsKey;
		WriteLn('Error - Unable to allocate memory to set ', v, ' at ', key);
		Exit;
	end;
	
	ptr^ := IntToStr(v);
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
	strPtr: AoCStrPtr;
	val: String;
	c: Coord2D;
begin
	result := [];
    For idx := 0 To _data.Count-1 Do
    begin
    	key := _data.NameOfIndex(idx);
    	strPtr := _data[idx];
    	val := strPtr^;
    	If val = withValue Then
    	begin
			c := Coord2D.Create(key);
			PushCoord(c, result);
    	end;
    end;
end;

Function Grid2D.GetHistogram(): AoCIntegerMap;
Var
	idx: Integer;
	//key: String;
	strPtr: AoCStrPtr;
	val: String;
begin
	result := AoCIntegerMap.Create;
    For idx := 0 To _data.Count-1 Do
    begin
    	//key := _data.NameOfIndex(idx);
    	strPtr := _data[idx];
    	val := strPtr^;
    	If (result.IndexOf(val) = -1) Then
    		result[val] := 0;
    	result[val] := result[val] + 1;
	end;	
end;

Function Grid2D.GetNeighbourOffsets(): Coord2DArray;
begin
	result := [];
	If (_aRule = queen) Or (_aRule = rook) Then
	begin
		PushCoord(Coord2D.Offset(NORTH), result);
		PushCoord(Coord2D.Offset(SOUTH), result);
		PushCoord(Coord2D.Offset(EAST), result);
		PushCoord(Coord2D.Offset(WEST), result);
	end;
	If (_aRule = queen) Or (_aRule = bishop) Then
	begin
		PushCoord(Coord2D.Offset(NW), result);
		PushCoord(Coord2D.Offset(SW), result);
		PushCoord(Coord2D.Offset(NE), result);
		PushCoord(Coord2D.Offset(SE), result);
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

Procedure Grid2D.Print();
Var
	r, c: Integer;
	ext: Extent2D;
begin
	ext := GetExtent;
	For r := ext.GetMin.Y To ext.GetMax.Y Do
	begin
		For c := ext.GetMin.X To ext.GetMax.X Do
		begin
			Write(GetString(Coord2D.Create(c, r)));
		end;
		WriteLn;
	end;
end;



end.