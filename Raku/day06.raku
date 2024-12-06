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

my @visited = solve_part_one(@input);
solve_part_two(@input, @visited);

exit( 0 );

sub solve_part_one(@input) {
	my $map = Grid.new(rule => AdjacencyRule::ROOK);
	$map.load(@input);
	my $end_pos = walk($map);
	my @visited = $map.coords('X');	
	my $count = @visited.elems;
	
	say "Part One: the number of places the guard walks is $count";
	@visited;
}

sub solve_part_two(@input, @path) {
	my @block_points = ();
	# for every point on the vanilla path (except the start)
	# replace the point with an obstacle and see if it loops
	for @path -> $coord {
		my $map = Grid.new(rule => AdjacencyRule::ROOK); $map.load(@input);
		my $start = $map.coords('^').first;
		next if $coord eqv $start; # can't put an obstacle here
		$map.set($coord, '#');
		my $end_pos = walk($map);
		if $map.extent.contains($end_pos.coord) {
			@block_points.push($coord);
		}
	}

	#my $map = Grid.new(rule => AdjacencyRule::ROOK); $map.load(@input);
	#for @block_points -> $bp {
	#	$map.set($bp, 'O');
	#}
	#$map.print();

	say "Part Two: the number of potential obstacles is " ~ @block_points.elems;
}

sub walk(Grid $map --> Position) {
	my $start = $map.coords('^').first;
	my $pos = Position.new( coord => $start, dir => 'N' );
	my $ext = $map.extent();
	my %positions = ($pos => 1);
	
	while $ext.contains($pos.coord) {
		$map.set($pos.coord, 'X');
		my $coord_in_front = $pos.coord.offset($pos.dir);
		if $map.get($coord_in_front) eq "#" {
			$pos = $pos.turn('CW');
		}
		else {
			$pos = $pos.move_forward();
		}
		if %positions{$pos}:exists {
			#looped
			return $pos;
		}
		%positions{$pos} = 1;
	}
	$pos;
}
