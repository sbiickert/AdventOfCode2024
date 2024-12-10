#!/usr/bin/env raku

use lib $*PROGRAM.dirname ~ '/lib';
use AOC::Util;
use AOC::Geometry;
use AOC::Grid;

my $INPUT_PATH = '../Input';
#my $INPUT_FILE = 'day10_test.txt';
my $INPUT_FILE = 'day10_challenge.txt';
my @input = read_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2024, Day 10: Hoof It";

my $map = Grid.new(default => '.', rule => AdjacencyRule::ROOK);
$map.load(@input);

$map.print();

solve_part_one($map);
#solve_part_two(@input);

exit( 0 );

sub solve_part_one(Grid $map) {
	my @trailheads = $map.coords('0');
	my $sum = 0;
	
	for @trailheads -> $trailhead {
		my %nines = ();
		score_trail($map, $trailhead, %nines);
		#say "Trailhead $trailhead has score " ~ %nines.elems;
		$sum += %nines.elems;
	}

	say "Part One: the sum of trailhead scores is $sum"
}

sub solve_part_two(@input) {
	
}

sub score_trail(Grid $map, Coord $th, %nines) {
	my $elev = $map.get($th);
	if $elev eq '9' {
		%nines{$th} = 1;
		return;
	}
	my @n = $map.neighbors($th).grep( -> $n {$map.get($n) eq $elev+1});
	for @n -> $next {
		score_trail($map, $next, %nines);
	}
}
