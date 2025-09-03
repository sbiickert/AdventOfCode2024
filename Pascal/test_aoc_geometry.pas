Program AocTest;

Uses
	AoCUtils, AoCGeometry, Classes;

Const
	INPUT_DAY = 0;

Procedure TestDirection();
Var
	n,sw: Coord2D;
	dir: Dir2D;
Begin
	n := Coord2D.Offset(Dir2D.NORTH);
	AssertTrue( n.IsEqualTo( Coord2D.Create(0, -1) ), 'Checking NORTH offset');
	sw := Coord2D.Offset(Dir2D.SW);
	AssertTrue( sw.IsEqualTo( Coord2D.Create(-1, 1) ), 'Checking SW offset');
	
	dir := StrToDir2D('n');
	AssertTrue( dir = Dir2D.NORTH, 'Checking Dir2D from "n"');
	dir := StrToDir2D('L');
	AssertTrue( dir = Dir2D.WEST, 'Checking Dir2D from "L"');
	dir := StrToDir2D('>');
	AssertTrue( dir = Dir2D.EAST, 'Checking Dir2D from ">"');
	dir := StrToDir2D('x');
	AssertTrue( dir = Dir2D.NO_DIR, 'Checking Dir2D from "x"');
End;

Begin
	TestDirection;
End.
