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
	sw := Coord2D.Offset(Dir2D.SW, 5);
	AssertTrue( sw.IsEqualTo( Coord2D.Create(-5, 5) ), 'Checking big SW offset');

	dir := StrToDir2D('n');
	AssertTrue( dir = Dir2D.NORTH, 'Checking Dir2D from "n"');
	dir := StrToDir2D('L');
	AssertTrue( dir = Dir2D.WEST, 'Checking Dir2D from "L"');
	dir := StrToDir2D('>');
	AssertTrue( dir = Dir2D.EAST, 'Checking Dir2D from ">"');
	dir := StrToDir2D('x');
	AssertTrue( dir = Dir2D.NO_DIR, 'Checking Dir2D from "x"');
End;

Procedure TestCoordCreate();
var
	c,fromStr: Coord2D;
begin
	c := Coord2D.Create(10, 30);
	AssertIntEqual(c.x, 10, 'Checking coord x');
	AssertIntEqual(c.y, 30, 'Checking coord y');
	fromStr := Coord2D.Create('10|40');
	AssertIntEqual(fromStr.x, 10, 'Checking coord x');
	AssertIntEqual(fromStr.y, 40, 'Checking coord y');
end;

Procedure TestCoordAddSub();
var
	c1,c2,sum,delta: Coord2D;
begin
	c1 := Coord2D.Create(10, 30);
	c2 := Coord2D.Create(5, 20);
	sum := c1.Add(c2);
	AssertIntEqual(sum.x, 15, 'Checking x of coordinate add');
	AssertIntEqual(sum.y, 50, 'Checking y of coordinate add');
	delta := c1.DeltaTo(c2);
	AssertTrue(delta.IsEqualTo(Coord2D.Create(-5, -10)), 'Checking coordinate delta');
end;

Procedure TestCoordDistance();
var
	c1,c2: Coord2D;
	md: Integer;
	d: Double;
begin
	c1 := Coord2D.Create(10, 30);
	c2 := Coord2D.Create(5, 20);
	md := c1.MDistanceTo(c2);
	AssertIntEqual(md, 15, 'Checking Manhattan distance');
	d := c1.DistanceTo(c2);
	AssertTrue(ApproxEqual(d, 11.18, 0.01), 'Checking straight line distance');
end;

Procedure TestCoordAdjacency();
var
	o,cHorz,cDiag,cFar,c: Coord2D;
	rookAdj,bishopAdj,queenAdj: Coord2DArray;
begin
	o := Coord2D.Origin;
	cHorz := o.Offset(Dir2D.EAST);
	AssertTrue(o.IsAdjacentTo(cHorz, Adjacency.ROOK), 'Checking adjacency H/ROOK');
	AssertFalse(o.IsAdjacentTo(cHorz, Adjacency.BISHOP), 'Checking adjacency H/BISHOP');
	AssertTrue(o.IsAdjacentTo(cHorz, Adjacency.QUEEN), 'Checking adjacency H/QUEEN');
	cDiag := o.Offset(Dir2D.SE);
	AssertFalse(o.IsAdjacentTo(cDiag, Adjacency.ROOK), 'Checking adjacency D/ROOK');
	AssertTrue(o.IsAdjacentTo(cDiag, Adjacency.BISHOP), 'Checking adjacency D/BISHOP');
	AssertTrue(o.IsAdjacentTo(cDiag, Adjacency.QUEEN), 'Checking adjacency D/QUEEN');
	cFar := o.Offset(Dir2D.WEST, 2);
	AssertFalse(o.IsAdjacentTo(cFar, Adjacency.QUEEN), 'Checking adjacency Far/QUEEN');
	
	rookAdj := o.AdjacentCoords(Adjacency.ROOK);
	AssertIntEqual(Length(rookAdj), 4, 'Checking number of rook adjacent coords.');
	AssertTrue(rookAdj[0].IsEqualTo(Coord2D.Create(0,-1)), 'Checking first rook adjacent coord.');
	bishopAdj := o.AdjacentCoords(Adjacency.BISHOP);
	AssertIntEqual(Length(bishopAdj), 4, 'Checking number of bishop adjacent coords.');
	AssertTrue(bishopAdj[0].IsEqualTo(Coord2D.Create(1,-1)), 'Checking first bishop adjacent coord.');
	queenAdj := o.AdjacentCoords(Adjacency.QUEEN);
	AssertIntEqual(Length(queenAdj), 8, 'Checking number of queen adjacent coords.');
	AssertTrue(queenAdj[0].IsEqualTo(Coord2D.Create(0,-1)), 'Checking first queen adjacent coord.');
	
	// for c in queenAdj do
 	//	c.Print;
end;

Begin
	TestDirection;
	TestCoordCreate;
	TestCoordAddSub;
	TestCoordDistance;
	TestCoordAdjacency;
End.
