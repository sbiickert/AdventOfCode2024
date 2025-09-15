Program AocTest;

Uses
	AoCUtils, AoCGeometry, SysUtils, Classes;

Procedure TestDirection();
Var
	n,sw: Coord2D;
	dir: Dir2D;
Begin
	n := Coord2D.XOffset(Dir2D.NORTH);
	AssertTrue( n.IsEqualTo( Coord2D.Create(0, -1) ), 'Checking NORTH offset');
	sw := Coord2D.XOffset(Dir2D.SW);
	AssertTrue( sw.IsEqualTo( Coord2D.Create(-1, 1) ), 'Checking SW offset');
	sw := Coord2D.XOffset(Dir2D.SW, 5);
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
	o,cHorz,cDiag,cFar: Coord2D;
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

Procedure TestPositionCreate();
var
	p: Pos2D;
begin
	p := Pos2D.Create(Coord2D.Create(5,6), Dir2D.EAST);
	AssertIntEqual( p.Location.x, 5, 'Checking Pos2D location x');
	AssertIntEqual( p.Location.y, 6, 'Checking Pos2D location y');
	AssertTrue(p.Direction = Dir2D.EAST, 'Checking Pos2D direction');
	AssertStrEqual(p.ToStr, 'Pos2D(5,6) -> 2)', 'Checking Pos2D ToStr');
end;

Procedure TestPositionTurn();
var
	p1: Pos2D;
	i: Integer;
begin
	p1 := Pos2D.Create( Coord2D.Origin, Dir2D.NORTH );
	p1.Turn(Rot2D.CW);
	AssertTrue(p1.Direction = Dir2D.EAST, 'Checking single CW rotation');
	p1.Turn(Rot2D.CCW);
	AssertTrue(p1.Direction = Dir2D.NORTH, 'Checking single CCW rotation');
	
	for i := 1 to 5 do
		p1.Turn(Rot2D.CW);
	AssertTrue(p1.Direction = Dir2D.EAST, 'Checking multiple CW rotation');
end;

Procedure TestPositionMove();
var
	p: Pos2D;
begin
	p := Pos2D.Create( Coord2D.Origin, Dir2D.SOUTH );
	p.MoveForward(1);
	AssertTrue(p.Location.IsEqualTo(Coord2D.Create(0,1)), 'Checking moving 1 step');
	p.MoveForward(99);
	AssertTrue(p.Location.IsEqualTo(Coord2D.Create(0,100)), 'Checking moving 99 steps');
	p.MoveBackward(1);
	AssertTrue(p.Location.IsEqualTo(Coord2D.Create(0,99)), 'Checking moving back 1 step');
	p.MoveBackward(49);
	AssertTrue(p.Location.IsEqualTo(Coord2D.Create(0,50)), 'Checking moving back 49 steps');
end;

Procedure TestSegmentLength();
var
	s,sZero: Seg2D;
begin
	s := Seg2D.Create( Coord2D.Origin, Coord2D.Create(5,6) );
	AssertIntEqual(s.Length, 11, 'Checking segment length');

	sZero := Seg2D.Create( Coord2D.Create(5,6), Coord2D.Create(5,6) );
	AssertIntEqual(sZero.Length, 0, 'Checking zero length segment');
end;

Procedure TestSegmentDirection();
var
	s,sZero: Seg2D;
	d: Dir2D;
begin
	for d in Dir2D do
	begin
		s := Seg2D.Create( Coord2D.Origin, Coord2D.Origin.Offset(d));
		AssertTrue(s.Direction = d, Format('Checking seg direction %d', [d]));
	end;
	
	s := Seg2D.Create( Coord2D.Origin, Coord2D.Create(-2,0) );
	AssertTrue(s.IsHorizontal, 'Checking seg horizontal');
	AssertTrue(s.Direction = Dir2D.WEST, 'Checking seg direction');
	
	s := Seg2D.Create( Coord2D.Origin, Coord2D.Create(0,-2) );
	AssertTrue(s.IsVertical, 'Checking seg vertical');
	AssertTrue(s.Direction = Dir2D.NORTH, 'Checking seg direction');

	sZero := Seg2D.Create( Coord2D.Create(5,6), Coord2D.Create(5,6) );
	AssertTrue(sZero.Direction = Dir2D.NO_DIR, 'Checking zero seg has no direction');
end;

Procedure TestExtentCreate();
var
	c1,c2,c3,c4: Coord2D;
	e1,e2: Extent2D;
begin
	c1 := Coord2D.Create(-1,1);
	c2 := Coord2D.Create(2,8);
	c3 := Coord2D.Create(3,3);
	c4 := Coord2D.Create(4,4);
	e1 := Extent2D.Create([c1,c2]);
	AssertTrue(e1.GetMin.IsEqualTo( Coord2D.Create(-1,1) ), 'Checking e1 min');
	AssertTrue(e1.GetMax.IsEqualTo( Coord2D.Create(2,8) ), 'Checking e1 max');
	e2 := Extent2D.Create([c3,c2,c1]);
	AssertTrue(e2.GetMin.IsEqualTo( Coord2D.Create(-1,1) ), 'Checking e2 min');
	AssertTrue(e2.GetMax.IsEqualTo( Coord2D.Create(3,8) ), 'Checking e2 max');
	e2.ExpandToFit(c4);
	AssertTrue(e2.GetMin.IsEqualTo( Coord2D.Create(-1,1) ), 'Checking expanded min');
	AssertTrue(e2.GetMax.IsEqualTo( Coord2D.Create(4,8) ), 'Checking expanded max');
end;

Procedure TestExtentBounds();
var
	e0,e1,e2: Extent2D;
begin
	e0 := MkExtent2D(-1,1,2,8);
	AssertIntEqual(e0.Width, 4, 'Checking ext width');
	AssertIntEqual(e0.Height, 8, 'Checking ext height');
	e1 := MkExtent2D(-1,1,3,8);
	AssertIntEqual(e1.Area, 40, 'Checking ext area');
	e2 := MkExtent2D(-2,-3,5,6);
	AssertTrue(e2.NW.IsEqualTo(Coord2D.Create(-2,-3)), 'Checking ext NW');
	AssertTrue(e2.NE.IsEqualTo(Coord2D.Create(5,-3)), 'Checking ext NE');
	AssertTrue(e2.SW.IsEqualTo(Coord2D.Create(-2,6)), 'Checking ext SW');
	AssertTrue(e2.SE.IsEqualTo(Coord2D.Create(5,6)), 'Checking ext SE');
end;

Procedure TestExtentCoords();
var
	e1: Extent2D;
	cArr: Coord2DArray;
begin
	e1 := MkExtent2D(-1,1,3,8);
	cArr := e1.AllCoords;
	AssertIntEqual(Length(cArr), e1.Area, 'Checking the number of coords in extent are correct');
	// Check reading order
	AssertTrue(cArr[0].IsEqualTo(MkCoord2D(-1,1))
		and cArr[1].IsEqualTo(MkCoord2D(0,1))
		and cArr[High(cArr)].IsEqualTo(MkCoord2D(3,8)), 'Checking reading order');
end;

Procedure TestExtentInset();
var
	e1,eInset: Extent2D;
begin
	e1 := MkExtent2D(-1,1,2,8);
	eInset := e1.Inset(1);
	AssertTrue(eInset.IsEqualTo(MkExtent2D(0,2,1,7)), 'Checking extent inset 1');
	eInset := e1.Inset(2);
	AssertTrue(eInset.IsEqualTo(MkExtent2D(0,3,1,6)), 'Checking extent inset 2');
	eInset := e1.Inset(-1);
	AssertTrue(eInset.IsEqualTo(MkExtent2D(-2,0,3,9)), 'Checking extent inset -1');
end;

Procedure TestExtentIntersect();
var
	e1: Extent2D;
	eArr1,eArr2,eArr3,eArr4,eArr5: Extent2DArray;
begin
	e1 := MkExtent2D(1,1,10,10);
	eArr1 := e1.Intersect(MkExtent2D(5,5,12,12));
	eArr2 := e1.Intersect(MkExtent2D(5,5,7,7));
	eArr3 := e1.Intersect(MkExtent2D(1,1,12,2));
	eArr4 := e1.Intersect(MkExtent2D(11,11,12,12));
	eArr5 := e1.Intersect(MkExtent2D(1,10,10,20));
	
	AssertIntEqual(Length(eArr1), 1, 'Checking intersect 1 was valid.');
	AssertTrue(eArr1[0].IsEqualTo(MkExtent2D(5,5,10,10)), 'Checking intersect 1 result');
	AssertIntEqual(Length(eArr2), 1, 'Checking intersect 2 was valid.');
	AssertTrue(eArr2[0].IsEqualTo(MkExtent2D(5,5,7,7)), 'Checking intersect 2 result');
	AssertIntEqual(Length(eArr3), 1, 'Checking intersect 3 was valid.');
	AssertTrue(eArr3[0].IsEqualTo(MkExtent2D(1,1,10,2)), 'Checking intersect 3 result');
	AssertIntEqual(Length(eArr4), 0, 'Checking intersect 4 was invalid.');
	AssertIntEqual(Length(eArr5), 1, 'Checking intersect 5 was valid.');
	AssertTrue(eArr5[0].IsEqualTo(MkExtent2D(1,10,10,10)), 'Checking intersect 5 result');
end;

// Procedure PrintExtents(arr: Extent2DArray);
// var
// 	e: Extent2D;
// begin
// 	for e in arr do
// 		e.Print;
// end;

Procedure TestExtentUnion();
var
	ext: Extent2D;
	products,expected: Extent2DArray;
begin
	ext := MkExtent2D(1,1,10,10);
	
	products := ext.Union(MkExtent2D(5,5,12,12));
	expected := [MkExtent2D(5,5,10,10),MkExtent2D(1,5,4,10),MkExtent2D(1,1,4,4),MkExtent2D(5,1,10,4),MkExtent2D(11,5,12,10),MkExtent2D(11,11,12,12),MkExtent2D(5,11,10,12)];
	AssertIntEqual(Length(products), Length(expected), 'Checking union result count');
	
	products := ext.Union(MkExtent2D(5,5,7,7));
	expected := [MkExtent2D(5,5,7,7),MkExtent2D(1,5,4,7),MkExtent2D(1,1,4,4),MkExtent2D(5,1,7,4),MkExtent2D(8,1,10,4),MkExtent2D(8,5,10,7),MkExtent2D(8,8,10,10),MkExtent2D(5,8,7,10),MkExtent2D(1,8,4,10)];
	AssertIntEqual(Length(products), Length(expected), 'Checking union result count');
	
	products := ext.Union(MkExtent2D(1,1,12,2));
	expected := [MkExtent2D(1,1,10,2),MkExtent2D(1,3,10,10),MkExtent2D(11,1,12,2)];
	AssertIntEqual(Length(products), Length(expected), 'Checking union result count');
	
	products := ext.Union(MkExtent2D(11,11,12,12));
	expected := [ext,MkExtent2D(11,11,12,12)];
	AssertIntEqual(Length(products), Length(expected), 'Checking union result count');
	
	products := ext.Union(MkExtent2D(1,10,10,20));
	expected := [MkExtent2D(1,10,10,10),MkExtent2D(1,1,10,9),MkExtent2D(1,11,10,20)];
	AssertIntEqual(Length(products), Length(expected), 'Checking union result count');
end;

Begin
	TestDirection;
	
	TestCoordCreate;
	TestCoordAddSub;
	TestCoordDistance;
	TestCoordAdjacency;
	
	TestPositionCreate;
	TestPositionTurn;
	TestPositionMove;
	
	TestSegmentLength;
	TestSegmentDirection;
	
	TestExtentCreate;
	TestExtentBounds;
	TestExtentCoords;
	TestExtentInset;
	TestExtentIntersect;
	TestExtentUnion;
End.
