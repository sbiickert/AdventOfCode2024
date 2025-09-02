// Utility module for Advent of Code
{$mode objfpc} // directive to be used for defining classes
{$m+}          // directive to be used for using constructor

Unit aocspatialutils;

Interface

Uses SysUtils, StrUtils, Math, AoCUtils, ContNrs;

Type
	Direction2D = (NORTH, SOUTH, EAST, WEST, NW, SW, NE, SE, UP, DOWN, LEFT, RIGHT);
	Rotation2D = (CW, CCW);
	
	Coord2D = Class
        Private
            _x, _y:   Integer;
            Procedure SetX(val: Integer);
            Procedure SetY(val: Integer);
        Public
            Constructor Create(ix, iy: Integer);
            Constructor Create(key: String); Virtual;
            Property X: Integer Read _x Write SetX;
            Property Y: Integer Read _y Write SetY;
            Property Row: Integer Read _y Write SetY;
            Property Col: Integer Read _x Write SetX;
            Class Function Origin: Coord2D;
            Class Function Offset(direction: Direction2D): Coord2D;
            Function IsEqualTo(other: Coord2D): Boolean;
            Function Add(other: Coord2D):Coord2D;
            Function DeltaTo(other: Coord2D): Coord2D;
            Function DistanceTo(other: Coord2D): Double;
            Function MDistanceTo(other: Coord2D): Integer;
            Procedure Print(); Virtual;
            Function AsKey(): String; Virtual;
    end;
    Coord2DArray =   array Of Coord2D;
    Coord2DPtr = ^Coord2D;
    
    Coord3D = class(Coord2D)
    	Private
    		_z: Integer;
            Procedure SetZ(val: Integer);
    	Public
            Constructor Create(ix, iy, iz: Integer);
            Constructor Create(key: String); Override;
            Property Z: Integer Read _z Write SetZ;
            Procedure Print(); Override;
            Function AsKey(): String; Override;
    end;
    Coord3DArray = array of Coord2D;
    Coord3DPtr = ^Coord3D;
    
    Position2D = class
    	Private
    		_location: Coord2D;
    		_direction: Direction2D;
            Procedure SetLocation(val: Coord2D);
            Procedure SetDirection(val: Direction2D);
    	Public
    		Constructor Create(cLocation: Coord2D; dDirection: Direction2D);
    		Property Location: Coord2D Read _location Write SetLocation;
    		Property Direction: Direction2D Read _direction Write SetDirection;
    		procedure Turn(dir: Rotation2D);
    		procedure MoveForward(iDistance: Integer);
    		procedure Print();
    end;
    Position2DArray = array of Position2D;
    Position2DPtr = ^Position2D;
    
    Extent2D = Class
    	Private
    		_min, _max:	Coord2D;
    	Public
    		Constructor Create(coords: Coord2DArray);
    		Function GetMin(): Coord2D;
    		Function GetMax(): Coord2D;
    		Function GetWidth(): Integer;
    		Function GetHeight(): Integer;
    		Function GetArea(): Integer;
    		Function Contains(coord: Coord2D): Boolean;
    		Function AllContainedCoords(): Coord2DArray;
    		Procedure ExpandToFit(coord: Coord2D);
    		Procedure Print();
    end;
    Extent2DPtr = ^Extent2D;

    Adjacency = (rook, bishop, queen);
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
// Coord2D
// -------------------------------------------------------

Constructor Coord2D.Create(ix, iy: Integer);
begin
    _x := ix;
    _y := iy;
end;

Constructor Coord2D.Create(key: String);
Var
	numbers: TStringArray;
begin
    numbers := SplitString(key, '|');
    _x := StrToInt(numbers[0]);
    _y := StrToInt(numbers[1]);
end;

Procedure Coord2D.SetX(val: Integer);
begin
    _x := val;
end;

Procedure Coord2D.SetY(val: Integer);
begin
    _y := val;
end;

Class function Coord2D.Origin: Coord2D;
begin
	result := Coord2D.Create(0,0);
end;

Class Function Coord2D.Offset(direction: Direction2D): Coord2D;
begin
	case direction of
	NORTH: 	result := Coord2D.Create( 0, -1);
	SOUTH: 	result := Coord2D.Create( 0,  1);
	WEST: 	result := Coord2D.Create(-1,  0);
	EAST: 	result := Coord2D.Create( 1,  0);
	NW: 	result := Coord2D.Create(-1, -1);
	SW: 	result := Coord2D.Create(-1,  1);
	NE: 	result := Coord2D.Create( 1, -1);
	SE: 	result := Coord2D.Create( 1,  1);
	UP: 	result := Coord2D.Create( 0, -1);
	DOWN: 	result := Coord2D.Create( 0,  1);
	LEFT: 	result := Coord2D.Create(-1,  0);
	RIGHT: 	result := Coord2D.Create( 1,  0);
	else 	result := Coord2D.Create(0,0);
	end;
end;

Function Coord2D.IsEqualTo(other: Coord2D):   Boolean;
begin
    If (X = other.X) And (Y = other.Y) Then
        result := true
    Else
        result := false;
end;

Function Coord2D.Add(other: Coord2D):Coord2D;
begin
	result := Coord2D.Create(X + other.X, Y + other.Y);
end;

Function Coord2D.DeltaTo(other: Coord2D): Coord2D;
begin
    result := Coord2D.Create(other.X - X, other.Y - Y);
end;

Function Coord2D.DistanceTo(other: Coord2D): Double;
Var
    delta: Coord2D;
begin
    delta := DeltaTo(other);
    result := Sqrt(Sqr(delta.X) + Sqr(delta.Y));
end;

Function Coord2D.MDistanceTo(other: Coord2D): Integer;
Var
    delta: Coord2D;
begin
    delta := DeltaTo(other);
    result := Abs(delta.X) + Abs(delta.Y);
end;

Procedure Coord2D.Print;
begin
    WriteLn('Coord2D(', _x, ',', _y, ')');
end;

Function Coord2D.AsKey(): String;
begin
	result := IntToStr(X) + '|' + IntToStr(Y);
end;

// -------------------------------------------------------
// Coord2D Utility Functions
// -------------------------------------------------------

Procedure PushCoord(coord: Coord2D; var arr: Coord2DArray);
Var
	len: Integer;
begin
	len := Length(arr)+1;
	SetLength(arr, len);
	arr[len-1] := coord;
end;

// -------------------------------------------------------
// Position2D
// -------------------------------------------------------

Constructor Position2D.Create(cLocation: Coord2D; dDirection: Direction2D);
begin
    _location := cLocation;
    _direction := dDirection;
end;

Procedure Position2D.SetLocation(val: Coord2D);
begin
    _location := val;
end;

Procedure Position2D.SetDirection(val: Direction2D);
begin
    _direction := val;
end;

procedure Position2D.Turn(dir: Rotation2D);
begin
	if dir = Rotation2D.CW then
	case _direction of
	NORTH: _direction := EAST;
	EAST: _direction := SOUTH;
	SOUTH: _direction := WEST;
	WEST: _direction := NORTH;
	end;
	
	if dir = Rotation2D.CCW then
	case _direction of
	NORTH: _direction := WEST;
	WEST: _direction := SOUTH;
	SOUTH: _direction := EAST;
	EAST: _direction := NORTH;
	end;
end;

procedure Position2D.MoveForward(iDistance: Integer);
var
	i: Integer;
begin
	for i := 1 to iDistance do
		_location := _location.Add(Coord2D.Offset(_direction));
end;

Procedure Position2D.Print;
begin
    WriteLn('Position2D(', _location.X, ',', _location.Y, ' -> ', _direction, ')');
end;


// -------------------------------------------------------
// Coord3D
// -------------------------------------------------------

Constructor Coord3D.Create(ix, iy, iz: Integer);
begin
    _x := ix;
    _y := iy;
    _z := iz;
end;

Constructor Coord3D.Create(key: String);
Var
	numbers: TStringArray;
begin
    numbers := SplitString(key, '|');
    _x := StrToInt(numbers[0]);
    _y := StrToInt(numbers[1]);
    _z := StrToInt(numbers[2]);
end;

Procedure Coord3D.SetZ(val: Integer);
begin
    _z := val;
end;

Procedure Coord3D.Print(); 
begin
    WriteLn('Coord3D(', _x, ',', _y, ',', _z, ')');
end;

Function Coord3D.AsKey(): String;
begin
	result := IntToStr(X) + '|' + IntToStr(Y) + '|' + IntToStr(Z);
end;

// -------------------------------------------------------
// Coord3D Utility Functions
// -------------------------------------------------------

Procedure PushCoord(coord: Coord3D; var arr: Coord3DArray);
Var
	len: Integer;
begin
	len := Length(arr)+1;
	SetLength(arr, len);
	arr[len-1] := coord;
end;


// -------------------------------------------------------
// Extent2D
// -------------------------------------------------------

Constructor Extent2D.Create(coords: Coord2DArray);
Var
	xmin, xmax, ymin, ymax: Integer;
	i: Integer;
begin
	xmin := coords[0].X;
	xmax := coords[0].X;
	ymin := coords[0].Y;
	ymax := coords[0].Y;
	for i := 1 to Length(coords)-1 do
	begin
		xmin := Min(xmin, coords[i].X);
		xmax := Max(xmax, coords[i].X);
		ymin := Min(ymin, coords[i].Y);
		ymax := Max(ymax, coords[i].Y);
	end;
	_min := Coord2D.Create(xmin, ymin);
	_max := Coord2D.Create(xmax, ymax);
end;

Function Extent2D.GetMin(): Coord2D;
begin
	result := _min;
end;

Function Extent2D.GetMax(): Coord2D;
begin
	result := _max;
end;

Function Extent2D.GetWidth(): Integer;
begin
	result := _max.X - _min.X + 1;
end;

Function Extent2D.GetHeight(): Integer;
begin
	result := _max.Y - _min.Y + 1;
end;

Function Extent2D.GetArea(): Integer;
begin
	result := GetWidth * GetHeight;
end;

Function Extent2D.Contains(coord: Coord2D): Boolean;
begin
	result := (GetMin.X <= coord.X)
		and (coord.X <= GetMax.X)
		and (GetMin.Y <= coord.Y)
		and (coord.Y <= GetMax.Y);
end;

Function Extent2D.AllContainedCoords(): Coord2DArray;
Var
	i, x, y: Integer;
begin
	result := [];
	SetLength(result, GetArea);
	i := 0;
	
	For x := GetMin.X To GetMax.X Do
		For y := GetMin.Y To GetMax.Y Do
		begin
			result[i] := Coord2D.Create(x,y);
			inc(i);
		end;
end;

Procedure Extent2D.ExpandToFit(coord: Coord2D);
begin
	if coord.X < _min.X then
		_min := Coord2D.Create(coord.X, _min.Y);
	if coord.X > _max.X then
		_max := Coord2D.Create(coord.X, _min.Y);
	if coord.Y < _min.Y then
		_min := Coord2D.Create(_min.X, coord.Y);
	if coord.Y > _max.Y then
		_max := Coord2D.Create(_max.X, coord.Y);
end;

Procedure Extent2D.Print();
begin
	WriteLn('Extent2D(');
	Write('  Min: ');
	GetMin.Print;
	Write('  Max: ');
	GetMax.Print;
	WriteLn(')');
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