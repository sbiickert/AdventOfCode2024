#!/usr/bin/env raku

use lib $*PROGRAM.dirname ~ '/lib';
use AOC::Util;
use AOC::Geometry;
use AOC::Grid;

my $INPUT_PATH = '../Input';
#my $INPUT_FILE = 'day06_test.txt';
my $INPUT_FILE = 'day06_challenge.txt';
my @input = read_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2024, Day 6: Guard Gallivant";

my $map = Grid.new(rule => AdjacencyRule::ROOK);
$map.load(@input);

#$map.print();

solve_part_one($map);
#solve_part_two(@input);

exit( 0 );

sub solve_part_one(Grid $map) {
	my $start = $map.coords('^').first;
	my $pos = Position.new( coord => $start, dir => 'N' );
	my $ext = $map.extent();
	while $ext.contains($pos.coord) {
		$map.set($pos.coord, 'X');
		my $coord_in_front = $pos.coord.offset($pos.dir);
		if $map.get($coord_in_front) eq "#" {
			$pos = $pos.turn('CW');
		}
		else {
			$pos = $pos.move_forward();
		}
	}
	my $count = $map.coords('X').elems;

	say "Part One: the number of places the guard walks is $count";
}

sub solve_part_two(@input) {
	
}
