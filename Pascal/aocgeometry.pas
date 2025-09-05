// Utility module for Advent of Code
{$mode objfpc} // directive to be used for defining classes
{$m+}          // directive to be used for using constructor

Unit AoCGeometry;

Interface

Uses SysUtils, StrUtils, Math, AoCUtils, ContNrs;

Type
	Dir2D = (NORTH, SOUTH, EAST, WEST, NW, SW, NE, SE, NO_DIR);
	Rot2D = (CW, CCW, NO_ROT);
    Adjacency = (rook, bishop, queen);
    
    Coord2D = class; // Forward declaration
    Coord2DArray =   array Of Coord2D;
    
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
            Class Function Offset(direction: Dir2D; size:Integer = 1): Coord2D;
            Function IsEqualTo(other: Coord2D): Boolean;
            Function Add(other: Coord2D):Coord2D;
            Function DeltaTo(other: Coord2D): Coord2D;
            Function DistanceTo(other: Coord2D): Double;
            Function MDistanceTo(other: Coord2D): Integer;
            Function IsAdjacentTo(other: Coord2D; rule: Adjacency): Boolean;
            Function AdjacentCoords(rule: Adjacency): Coord2DArray; // Needed forward declaration
            Procedure Print(); Virtual;
            Function AsKey(): String; Virtual;
    end;
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
            Function IsEqualTo(other: Pos2D): Boolean;
    		procedure Turn(dir: Rot2D);
    		procedure MoveForward(iDistance: Integer);
    		procedure Print();
    end;
    Pos2DArray = array of Pos2D;
    Pos2DPtr = ^Pos2D;
    
    Seg2D = class
    	Private
    		_a: Coord2D;
    		_b: Coord2D;
    		Procedure SetA(val: Coord2D);
    		Procedure SetB(val: Coord2D);
    	Public
    		Constructor Create(a,b:Coord2D);
    		Property A: Coord2D Read _a Write SetA;
    		Property B: Coord2D Read _b Write SetB;
            Function IsEqualTo(other: Seg2D): Boolean;
    		Function Length(): Integer;
    		Function IsHorizontal(): Boolean;
    		Function IsVertical(): Boolean;
    		Function Direction(): Dir2D;
    end;
    Seg2DArray = array of Seg2D;
    Seg2DPtr = ^Seg2D;
    
    
	Extent2D = class; // Forward declaration
    Extent2DArray = array of Extent2D;

    Extent2D = Class
    	Private
    		_min, _max:	Coord2D;
    	Public
    		Constructor Create(coords: Coord2DArray);
            Function IsEqualTo(other: Extent2D): Boolean;
    		Function GetMin(): Coord2D;
    		Function GetMax(): Coord2D;
    		Function NW(): Coord2D;
    		Function SW(): Coord2D;
    		Function NE(): Coord2D;
    		Function SE(): Coord2D;
    		Function Width(): Integer;
    		Function Height(): Integer;
    		Function Area(): Integer;
    		Function Contains(coord: Coord2D): Boolean;
    		Function AllCoords(): Coord2DArray;
    		Procedure ExpandToFit(coord: Coord2D);
    		Function Inset(i: Integer): Extent2D;
    		Function Intersect(other: Extent2D): Extent2DArray;
    		Procedure Print();
    end;
    Extent2DPtr = ^Extent2D;
	
	Function MkCoord2D(x,y: Integer): Coord2D;
	Function MkExtent2D(xmin,ymin,xmax,ymax: Integer): Extent2D;

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

Class Function Coord2D.Offset(direction: Dir2D; size:Integer = 1): Coord2D;
begin
	case direction of
	NORTH: 	result := Coord2D.Create( 0, -1 * size);
	SOUTH: 	result := Coord2D.Create( 0,  1 * size);
	WEST: 	result := Coord2D.Create(-1 * size,  0);
	EAST: 	result := Coord2D.Create( 1 * size,  0);
	NW: 	result := Coord2D.Create(-1 * size, -1 * size);
	SW: 	result := Coord2D.Create(-1 * size,  1 * size);
	NE: 	result := Coord2D.Create( 1 * size, -1 * size);
	SE: 	result := Coord2D.Create( 1 * size,  1 * size);
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

Function Coord2D.IsAdjacentTo(other: Coord2D; rule: Adjacency): Boolean;
begin
	case rule of
	ROOK:	result := MDistanceTo(other) = 1;
	BISHOP:	result := (Abs(x - other.x) = 1) and (Abs(y - other.y) = 1);
	QUEEN:	result := (MDistanceTo(other) = 1) or (Abs(x - other.x) = 1) and (Abs(y - other.y) = 1);
	end;
end;

Function Coord2D.AdjacentCoords(rule: Adjacency): Coord2DArray;
begin
	result := [];
	if (rule = Adjacency.QUEEN) or (rule = Adjacency.ROOK) then
	begin
		PushCoord(Offset(Dir2D.NORTH), result);
		PushCoord(Offset(Dir2D.EAST), result);
		PushCoord(Offset(Dir2D.SOUTH), result);
		PushCoord(Offset(Dir2D.WEST), result);
	end;
	if (rule = Adjacency.QUEEN) or (rule = Adjacency.BISHOP) then
	begin
		PushCoord(Offset(Dir2D.NE), result);
		PushCoord(Offset(Dir2D.SE), result);
		PushCoord(Offset(Dir2D.SW), result);
		PushCoord(Offset(Dir2D.NW), result);
	end;
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

Function MkCoord2D(x,y: Integer): Coord2D;
begin
	result := Coord2D.Create(x, y);
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

Function Pos2D.IsEqualTo(other: Pos2D): Boolean;
begin
	result := (_location.IsEqualTo(other.Location)) and (_direction = other.Direction);
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
// Seg2D
// -------------------------------------------------------

Constructor Seg2D.Create(a,b: Coord2D);
begin
	_a := a;
	_b := b;
end;

Procedure Seg2D.SetA(val: Coord2D);
begin
	_a := val;
end;

Procedure Seg2D.SetB(val: Coord2D);
begin
	_b := val;
end;

Function Seg2D.IsEqualTo(other: Seg2D): Boolean;
begin
	result := (_a.IsEqualTo(other.A)) and (_b.IsEqualTo(other.B));
end;

Function Seg2D.Length(): Integer;
begin
	result := _a.MDistanceTo(_b);
end;

Function Seg2D.IsHorizontal(): Boolean;
begin
	result := (_a.y = _b.y) and (_a.x <> _b.x);
end;

Function Seg2D.IsVertical(): Boolean;
begin
	result := (_a.y <> _b.y) and (_a.x = _b.x);
end;

Function Seg2D.Direction(): Dir2D;
begin
	if (Length = 0) then
		result := Dir2D.NO_DIR
	else if IsHorizontal then
	begin
		if _a.x < _b.x then
			result := Dir2D.EAST
		else
			result := Dir2D.WEST;
	end
	else if IsVertical then
	begin
		if _a.y < _b.y then
			result := Dir2D.SOUTH
		else
			result := Dir2D.NORTH;
	end
	else if _a.x < _b.x then
	begin
		if _a.y < _b.y then
			result := Dir2D.SE
		else
			result := Dir2D.NE;
	end
	else
	begin
		if _a.y < _b.y then
			result := Dir2D.SW
		else
			result := Dir2D.NW;
	end;
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

Function Extent2D.IsEqualTo(other: Extent2D): Boolean;
begin
	result := (_min.IsEqualTo(other.GetMin)) and (_max.IsEqualTo(other.GetMax));
end;

Function Extent2D.GetMin(): Coord2D;
begin
	result := _min;
end;

Function Extent2D.GetMax(): Coord2D;
begin
	result := _max;
end;

Function Extent2D.NW(): Coord2D;
begin
	result := GetMin;
end;

Function Extent2D.SW(): Coord2D;
begin
	result := Coord2D.Create(_min.x, _max.y);
end;

Function Extent2D.NE(): Coord2D;
begin
	result := Coord2D.Create(_max.x, _min.y);
end;

Function Extent2D.SE(): Coord2D;
begin
	result := GetMax;
end;

Function Extent2D.Width(): Integer;
begin
	result := _max.X - _min.X + 1;
end;

Function Extent2D.Height(): Integer;
begin
	result := _max.Y - _min.Y + 1;
end;

Function Extent2D.Area(): Integer;
begin
	result := Width * Height;
end;

Function Extent2D.Contains(coord: Coord2D): Boolean;
begin
	result := (GetMin.X <= coord.X)
		and (coord.X <= GetMax.X)
		and (GetMin.Y <= coord.Y)
		and (coord.Y <= GetMax.Y);
end;

Function Extent2D.AllCoords(): Coord2DArray;
Var
	i, x, y: Integer;
begin
	result := [];
	SetLength(result, Area);
	i := 0;
	
	For y := GetMin.Y To GetMax.Y Do
		For x := GetMin.X To GetMax.X Do
		begin
			result[i] := Coord2D.Create(x,y);
			inc(i);
		end;
end;

Procedure Extent2D.ExpandToFit(coord: Coord2D);
var
	xmin,xmax,ymin,ymax: Integer;
begin
	xmin := Min(coord.x, _min.x);
	ymin := Min(coord.y, _min.y);
	xmax := Max(coord.x, _max.x);
	ymax := Max(coord.y, _max.y);
	_min := Coord2D.Create(xmin, ymin);
	_max := Coord2D.Create(xmax, ymax);
end;

Function Extent2D.Inset(i: Integer): Extent2D;
begin
	result := MkExtent2D(_min.x + i, _min.y + i, _max.x - i, _max.y - i);
end;

Function Extent2D.Intersect(other: Extent2D): Extent2DArray;
var
	commonXMin,commonXMax,commonYMin,commonYMax:Integer;
begin
	commonXMin := Max(GetMin.X, other.GetMin.X);
	commonXMax := Min(GetMax.X, other.GetMax.X);
	commonYMin := Max(GetMin.Y, other.GetMin.Y);
	commonYMax := Min(GetMax.Y, other.GetMax.Y);
	if commonXMax < commonXMin then result := []
	else if commonYMax < commonYMin then result := []
	else result := [MkExtent2D(commonXMin,commonYMin,commonXMax,commonYMax)];
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

Function MkExtent2D(xmin,ymin,xmax,ymax: Integer): Extent2D;
begin
	result := Extent2D.Create([Coord2D.Create(xmin,ymin), Coord2D.Create(xmax,ymax)]);
end;

end.