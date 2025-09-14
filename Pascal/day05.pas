Program AOC_2024_Day05;
{$mode objfpc} // directive to be used for defining classes
{$m+}		   // directive to be used for using constructor
{$H+}		   // directive to be used for ANSI strings
{$J-}		   // directive to be used for turning off writeable const

Uses SysUtils, StrUtils, AoCUtils, Classes, fgl;

Const
	DAY = 5;

Type

	OrderRule = Class
		Private
			_page: String;
			_follows:  AoCStringMap; // These numbers follow _page
			_precedes: AoCStringMap; // These numbers precede _page
		Public
			Constructor Create(page: String);
			Procedure AddPageThatFollows(other: String);
			Procedure AddPageThatPrecedes(other: String);
			Function DoesPageFollow(other: String): Boolean;
			Function DoesPagePrecede(other: String): Boolean;
	end;
		
	AoCRuleMap =       specialize TFPGMap<String, OrderRule>;

Var
	RULES: AoCRuleMap;
	
Constructor OrderRule.Create(page: String);
Begin
	_page := page;
	_follows := AoCStringMap.Create;
	_precedes := AoCStringMap.Create;
End;

Procedure OrderRule.AddPageThatFollows(other: String);
Begin
	_follows.Add(other,'');
End;

Procedure OrderRule.AddPageThatPrecedes(other: String);
Begin
	_precedes.Add(other,'');
End;

Function OrderRule.DoesPageFollow(other: String): Boolean;
Begin
	if _precedes.IndexOf(other) <> -1 then result := true else result := false;
End;

Function OrderRule.DoesPagePrecede(other: String): Boolean;
Begin
	if _follows.IndexOf(other) <> -1 then result := true else result := false;
End;

Function SortOrder(page1: String; page2: String): Integer;
Begin
	result := 0; // No sort defined
	if (RULES.IndexOf(page1) = -1) or (RULES.IndexOf(page2) = -1) then exit;
	if RULES[page1].DoesPageFollow(page2) then
		result := -1
	else if RULES[page2].DoesPageFollow(page1) then
		result := 1;
End;


Function GetMidValue(pages: AoCStringArray): Integer;
Var
	mid: Integer;
Begin
	mid := Trunc(Length(pages) / 2);
	result := StrToInt(pages[mid]);
End;

Function IsOrderCorrect(pages: AoCStringArray): Boolean;
Var
	i,j: Integer;
Begin
	result := true;
	for i := 0 to High(pages)-1 do
		for j := i+1 to High(pages) do
		begin
			if SortOrder(pages[i], pages[j]) = -1 then
			begin
				result := false;
				exit;
			end;
		end;
End;

Procedure SolvePart1(updates: TStringList);
Var
	sum: Integer;
	update: String;
	pages: AoCStringArray;
Begin
	WriteLn('Part 1: What is the sum of the middle pages of correct updates?');
	
	sum := 0;
	
	for update in updates do
	begin
		pages := SplitString(update,',');
		if IsOrderCorrect(pages) then
			sum := sum + GetMidValue(pages);
	end;
	
	WriteLn(Format('Part One Solution: %d', [sum]));
End;

Procedure SortPages(pages: AoCStringArray);
Var
	i: Integer;
	temp: String;
Begin
	while IsOrderCorrect(pages) = false do
		for i := 0 to High(pages)-1 do
			if SortOrder(pages[i],pages[i+1]) = -1 then
			begin
				temp := pages[i];
				pages[i] := pages[i+1];
				pages[i+1] := temp;
			end;
End;

Procedure SolvePart2(updates: TStringList);
Var
	sum: Integer;
	update: String;
	pages: AoCStringArray;
Begin
	WriteLn('Part 2: What is the sum of the middle pages of corrected updates?');
	
	sum := 0;
	
	for update in updates do
	begin
		pages := SplitString(update,',');
		if IsOrderCorrect(pages) = false then
		begin
			SortPages(pages);
			sum := sum + GetMidValue(pages);
		end;
	end;
	
	WriteLn(Format('Part Two Solution: %d', [sum]));
End;

Function ParseRules(input: TStringList): AoCRuleMap;
Var
	split: AoCStringArray;
	line: String;
	rule: OrderRule;
Begin
	result := AoCRuleMap.Create;
	for line in input do
	begin
		split := SplitString(line, '|');
		if result.IndexOf(split[0]) = -1 then
		begin
			rule := OrderRule.Create(split[0]);
			result.Add(split[0],rule);
		end;
		rule := result[split[0]];
		rule.AddPageThatFollows(split[1]);
		
		if result.IndexOf(split[1]) = -1 then
		begin
			rule := OrderRule.Create(split[1]);
			result.Add(split[1],rule);
		end;
		rule := result[split[1]];
		rule.AddPageThatPrecedes(split[0]);
	end;
End;

Var
	iFileName: String;
	input: AoCStringListGroup;
	updates: TStringList;
Begin
	WriteLn('AoC 2015 Day 5: Print Queue');
	iFileName := InputFileName(DAY, False);
	input := ReadGroupedInput(iFileName);
	RULES := ParseRules(input[0]);
	updates := input[1];
	SolvePart1(updates);
	SolvePart2(updates);
End.
