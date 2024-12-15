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
#solve_part_two(@input);

exit( 0 );

sub solve_part_one(Grid $map, @moves) {
	my $robot = $map.coords('@')[0];
	for @moves -> $move {
		#$map.print;
		say $move.raku;
		#say "Robot $robot offset $move is $offset";
		if push_box($map, $robot, $move) {
			$robot = $robot.offset($move);
		}
	}
	#$map.print;

	my $gps = sum_gps($map);

	say "Part One: the sum of GPS coords is $gps";
}

sub solve_part_two(@input) {
	
}

sub push_box(Grid $map, Coord $loc, Str $dir --> Bool) {
	die $loc if !$map.extent.contains($loc);
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

sub sum_gps($map --> Int) {
	my @box_locations = $map.coords('O');
	my @values = @box_locations.map( -> $loc { $loc.x + ($loc.y * 100) });
	@values.sum;
}

sub parse_input(@input) {
	my $grid = Grid.new(default => '.', rule => AdjacencyRule::ROOK);
	$grid.load(@input.[0]);
#	$grid.print;

	my $all_moves = @input[1].join;
	my @moves = $all_moves.split('', :skip-empty);
	return $grid, @moves;
}
