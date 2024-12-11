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

solve_parts($map);

exit( 0 );

sub solve_parts(Grid $map) {
	my @trailheads = $map.coords('0');
	my $score_sum = 0;
	my $rating_sum = 0;
	
	for @trailheads -> $trailhead {
		my %nines = ();
		my $rating = score_and_rate_trail($map, $trailhead, %nines);
		$score_sum += %nines.elems;
		$rating_sum += $rating;
	}

	say "Part One: the sum of trailhead scores is $score_sum";
	say "Part Two: the sum of trailhead ratings is $rating_sum";
}

sub score_and_rate_trail(Grid $map, Coord $th, %nines) {
	my $elev = $map.get($th);
	if $elev eq '9' {
		%nines{$th} = 1;
		return 1;
	}
	$map.neighbors($th)
		==> grep( -> $n { $map.get($n) eq $elev+1 })
		==> map( -> $n {score_and_rate_trail($map, $n, %nines);})
		==> sum()
		==> my @rating;
	@rating.first;
}
