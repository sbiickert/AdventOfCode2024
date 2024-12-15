#!/usr/bin/env raku

use lib $*PROGRAM.dirname ~ '/lib';
use AOC::Util;
use AOC::Geometry;
use AOC::Grid;

my $INPUT_PATH = '../Input';
#my $INPUT_FILE = 'day15_test.txt';
my $INPUT_FILE = 'day15_challenge.txt';
my @input = read_grouped_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2024, Day 15: Warehouse Woes";

my ($map, @moves) = parse_input(@input);

solve_part_one($map, @moves[0]);

($map, @moves) = parse_wide_input(@input);

solve_part_two($map, @moves[0]);

exit( 0 );

sub solve_part_one(Grid $map, @moves) {
	my $robot = $map.coords('@')[0];
	for @moves -> $move {
		if push_box($map, $robot, $move) {
			$robot = $robot.offset($move);
		}
	}
	#$map.print;

	my $gps = sum_gps($map);

	say "Part One: the sum of GPS coords is $gps";
}

sub solve_part_two(Grid $map, @moves) {
	my $robot = $map.coords('@')[0];
	#$map.print;
	for @moves -> $move {
		#say $move;
		if can_push_wide_box($map, [$robot], $move) {
			push_wide_box($map, [$robot], $move);
			$robot = $robot.offset($move);
		}
		#else { say "Can't move" }
	}
	#$map.print;

	my $gps = sum_gps($map);

	say "Part Two: the sum of GPS coords is $gps";	
}

sub push_box(Grid $map, Coord $loc, Str $dir --> Bool) {
	#die $loc if !$map.extent.contains($loc);
	my $offset = $loc.offset($dir);
	my $adj = $map.get($offset);
	if $adj eq '.' {
		$map.set($offset, $map.get($loc));
		$map.clear($loc);
		return True;
	}
	elsif $adj eq '#' {
		return False;
	}
	if push_box($map, $offset, $dir) {
		$map.set($offset, $map.get($loc));
		$map.clear($loc);
		return True;		
	}
	False;
}

sub can_push_wide_box(Grid $map, @loc, Str $dir --> Bool) {
	for @loc -> $l {
		my $offset = $l.offset($dir);
		my $adj = $map.get($offset);
		return False if $adj eq '#';
		next if $adj eq '.';
		
		my @pushed = ($offset);
		if $dir eq '^' || $dir eq 'v' {
			my $second_offset = $adj eq '['
				?? $offset.offset('>') !! $offset.offset('<');
			@pushed.push($second_offset);
		}

		my $can_push = can_push_wide_box($map, @pushed, $dir);
		return False if !$can_push;
	}
	True;
}

sub push_wide_box(Grid $map, @loc, Str $dir) {
	for @loc -> $l {
		my $offset = $l.offset($dir);
		my $adj = $map.get($offset);
		if $adj eq '.' {
			$map.set($offset, $map.get($l));
			$map.clear($l);
		}
		else {
			my @pushed = ($offset);
			if $dir eq '^' || $dir eq 'v' {
				my $second_offset = $adj eq '['
					?? $offset.offset('>') !! $offset.offset('<');
				@pushed.push($second_offset);
			}
			push_wide_box($map, @pushed, $dir);
			$map.set($offset, $map.get($l));
			$map.clear($l);
		}
	}
}

sub sum_gps($map --> Int) {
	my @box_locations = $map.coords('O');
	@box_locations = $map.coords('[') if @box_locations.elems == 0; # Part 2
	my @values = @box_locations.map( -> $loc { $loc.x + ($loc.y * 100) });
	@values.sum;
}

sub parse_input(@input) {
	my $grid = Grid.new(default => '.', rule => AdjacencyRule::ROOK);
	$grid.load(@input[0]);
#	$grid.print;

	my $all_moves = @input[1].join;
	my @moves = $all_moves.split('', :skip-empty);
	return $grid, @moves;
}

sub parse_wide_input(@input) {
	my $grid = Grid.new(default => '.', rule => AdjacencyRule::ROOK);

	for 0..@input[0].end -> $r {
		my @cols = @input[0][$r].split('', :skip-empty);
		for 0..@cols.end -> $c {
			if @cols[$c] ne $grid.default {
				my $glyph = @cols[$c];
				my $x = $c*2;
				my $coord0 = Coord.from_ints($x, $r);
				my $coord1 = Coord.from_ints($x+1, $r);
				if $glyph eq 'O' {
					$grid.set($coord0, '[');
					$grid.set($coord1, ']');
				}
				elsif $glyph eq '#' {
					$grid.set($coord0, $glyph);
					$grid.set($coord1, $glyph);
				}
				else {
					$grid.set($coord0, $glyph);
				}
			}
		}
	}
	
	my $all_moves = @input[1].join;
	my @moves = $all_moves.split('', :skip-empty);
	return $grid, @moves;
}
