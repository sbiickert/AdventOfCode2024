// Utility module for Advent of Code
{$mode objfpc} // directive to be used for defining classes
{$m+}          // directive to be used for using constructor

Unit AoCGeometry;

Interface

Uses SysUtils, StrUtils, Math, AoCUtils, ContNrs;

Type
	Dir2D = (NORTH, SOUTH, EAST, WEST, NW, SW, NE, SE, UP, DOWN, LEFT, RIGHT, NO_DIR);
	Rot2D = (CW, CCW, NO_ROT);
	
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
            Class Function Offset(direction: Dir2D): Coord2D;
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
    
    Pos2D = class
    	Private
    		_location: Coord2D;
    		_direction: Dir2D;
            Procedure SetLocation(val: Coord2D);
            Procedure SetDirection(val: Dir2D);
    	Public
    		Constructor Create(cLocation: Coord2D; dDirection: Dir2D);
    		Property Location: Coord2D Read _location Write SetLocation;
    		Property Direction: Dir2D Read _direction Write SetDirection;
    		procedure Turn(dir: Rot2D);
    		procedure MoveForward(iDistance: Integer);
    		procedure Print();
    end;
    Pos2DArray = array of Pos2D;
    Pos2DPtr = ^Pos2D;
    
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
    
    Procedure PushCoord(coord: Coord2D; var arr: Coord2DArray);
	Procedure PushCoord(coord: Coord3D; var arr: Coord3DArray);

	Function StrToDir2D(s: String):Dir2D;

Implementation

Function StrToDir2D(s: String):Dir2D;
var
	upper:String;
begin
	upper := UpperCase(s);
	case upper of
	'N','U','^':	result := Dir2D.NORTH;
	'E','R','>':	result := Dir2D.EAST;
	'W','L','<':	result := Dir2D.WEST;
	'S','D','V':	result := Dir2D.SOUTH;
	'NE':			result := Dir2D.NE;
	'SE':			result := Dir2D.SE;
	'NW':			result := Dir2D.NW;
	'SW':			result := Dir2D.SW;
	else
		result := Dir2D.NO_DIR;
	end;
end;

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

Class Function Coord2D.Offset(direction: Dir2D): Coord2D;
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
// Pos2D
// -------------------------------------------------------

Constructor Pos2D.Create(cLocation: Coord2D; dDirection: Dir2D);
begin
    _location := cLocation;
    _direction := dDirection;
end;

Procedure Pos2D.SetLocation(val: Coord2D);
begin
    _location := val;
end;

Procedure Pos2D.SetDirection(val: Dir2D);
begin
    _direction := val;
end;

procedure Pos2D.Turn(dir: Rot2D);
begin
	if dir = Rot2D.CW then
	case _direction of
	NORTH: _direction := EAST;
	EAST: _direction := SOUTH;
	SOUTH: _direction := WEST;
	WEST: _direction := NORTH;
	end;
	
	if dir = Rot2D.CCW then
	case _direction of
	NORTH: _direction := WEST;
	WEST: _direction := SOUTH;
	SOUTH: _direction := EAST;
	EAST: _direction := NORTH;
	end;
end;

procedure Pos2D.MoveForward(iDistance: Integer);
var
	i: Integer;
begin
	for i := 1 to iDistance do
		_location := _location.Add(Coord2D.Offset(_direction));
end;

Procedure Pos2D.Print;
begin
    WriteLn('Pos2D(', _location.X, ',', _location.Y, ' -> ', _direction, ')');
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


end.