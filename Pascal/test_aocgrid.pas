{$mode objfpc} // directive to be used for defining classes
{$m+}          // directive to be used for using constructor
{$H+}		   // directive to be used for ANSI strings

Program AocTest;

Uses
	AoCUtils, AoCGeometry, AoCGrid, SysUtils, Classes;

Type
	GameObj = Class(GridData)
		Private
			_name: String;
			_hp: Integer;
			Procedure SetName(name: String);
			Procedure SetHP(hp: Integer);
		Public
			Constructor Create(name: String; hp: Integer);
			Property Name: String Read _name Write SetName;
			Property HP: Integer Read _hp Write SetHP;
	    	Function Glyph(): String; override;
	    	Function StrValue(): String; override;
	    	Function IntValue(): Integer; override;
			Procedure Print();
	end;
	

Constructor GameObj.Create(name: String; hp: Integer);
begin
	_name := name;
	_hp := hp;
end;

Procedure GameObj.SetName(name: String);
begin
	_name := name;
end;

Procedure GameObj.SetHP(hp: Integer);
begin
	_hp := hp;
end;

Function GameObj.Glyph():String;
begin
	result := LeftStr(_name, 1);
end;

Function GameObj.StrValue():String;
begin
	result := _name;
end;

Function GameObj.IntValue():Integer;
begin
	result := _hp;
end;

Procedure GameObj.Print();
begin
	WriteLn( Format('%s(%d)', [_name, _hp]) );
end;

Procedure TestGrid();
var
	grid: Grid2D;
	ext: Extent2D;
	elf,goblin,santa,goTest: GameObj;
	allCoords,coordsWithB: Coord2DArray;
	hist: AoCIntegerMap;
	str: String;
	i: Integer;
begin
	grid := Grid2D.Create('.', Adjacency.ROOK);
	AssertStrEqual(grid.Default, '.', 'Checking default value');
	AssertTrue(grid.Rule = Adjacency.ROOK, 'Checking adjacency');
	AssertTrue(grid.IsEmpty, 'Checking grid is empty');
	
	grid.SetString('A', MkCoord2D(1,1));
	grid.SetString('B', MkCoord2D(2,2));
	grid.SetString('D', MkCoord2D(4,4));
	AssertFalse(grid.IsEmpty, 'Checking grid is not empty');
	
	AssertStrEqual(grid.GetString(MkCoord2D(1,1)), 'A', 'Checking GetString A');
	AssertStrEqual(grid.GetString(MkCoord2D(2,2)), 'B', 'Checking GetString B');
	AssertStrEqual(grid.GetString(MkCoord2D(3,3)), grid.Default, 'Checking GetString Default');
	AssertStrEqual(grid.GetString(MkCoord2D(4,4)), 'D', 'Checking GetString D');
	
	ext := grid.GetExtent;
	AssertTrue(ext.GetMin.IsEqualTo(MkCoord2D(1,1)), 'Checking extent min');
	AssertTrue(ext.GetMax.IsEqualTo(MkCoord2D(4,4)), 'Checking extent max');
	
	elf := GameObj.Create('Elf', 100);
	goblin := GameObj.Create('Goblin', 95);
	santa := GameObj.Create('Santa', 1000);
	
	grid.SetData(elf, MkCoord2D(1,4));
	grid.SetData(goblin, MkCoord2D(2,4));
	grid.SetData(santa, MkCoord2D(3,4));
	goTest := grid.GetData(MkCoord2D(1,4)) as GameObj;
	AssertStrEqual(goTest.Name, 'Elf', 'Checking elf name');
	AssertStrEqual(	grid.GetData(MkCoord2D(2,4)).Glyph, 'G', 'Checking goblin glyph');
	goTest := grid.GetData(MkCoord2D(3,4)) as GameObj;
	AssertIntEqual(goTest.HP, 1000, 'Checking santa HP');
	
	allCoords := grid.GetCoords;
	AssertIntEqual(Length(allCoords), 6, 'Checking all coords count');
	coordsWithB := grid.GetCoords('B');
	AssertTrue(coordsWithB[0].IsEqualTo(MkCoord2D(2,2)), 'Checking coord with B');

	grid.SetString('B', MkCoord2D(3,3));
	hist := grid.GetHistogram;
	AssertIntEqual(hist['A'], 1, 'Checking there is 1 A');
	AssertIntEqual(hist['B'], 2, 'Checking there are 2 Bs');
	AssertTrue(hist.IndexOf(grid.Default) = -1, 'Checking the hist does not include unset');
	
	hist := grid.GetHistogram(True);
	AssertIntEqual(hist[grid.Default], 9, 'Checking count of unset'); 
	
	grid.Print;
	str := grid.SPrint;
	AssertTrue( str = ('A...' + sLineBreak + '.B..' + sLineBreak +
					   '..B.' + sLineBreak + 'EGSD' + sLineBreak),
				'Checking grid string representation');
	
	grid.SetString('6', MkCoord2D(2,1));
	str := grid.GetString(MkCoord2D(2,1));
	AssertStrEqual(str, '6', 'Checking GetString on stored String');
	i := grid.GetInteger(MkCoord2D(2,1));
	AssertIntEqual(i, 6, 'Checking GetInteger on stored String');
end;

Procedure TestGridLoad();
var
	input: TStringList;
	grid: Grid2D;
	gridContent: String;
begin
	input := TStringList.Create;
	input.Add('ABCD');
	input.Add('.G.H');
	input.Add('IJK..');
	
	grid := Grid2D.Create('.', Adjacency.ROOK);
	grid.Load(input);
	grid.Print;
	gridContent := grid.SPrint;
	AssertStrEqual(gridContent, 'ABCD'+sLineBreak+'.G.H'+sLineBreak+'IJK.'+sLineBreak, 'Checking loaded grid');
end;

Begin
	TestGrid;
	TestGridLoad;
End.
