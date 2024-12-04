#!/usr/bin/env raku

use lib $*PROGRAM.dirname ~ '/lib';
use AOC::Util;
use AOC::Geometry;
use AOC::Grid;

my $INPUT_PATH = '../Input';
#my $INPUT_FILE = 'day04_test.txt';
my $INPUT_FILE = 'day04_challenge.txt';
my @input = read_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2024, Day 4: Ceres Search";

my $puzzle = Grid.new(default => '.', rule => AdjacencyRule::QUEEN);
$puzzle.load(@input);

#$puzzle.print();

solve_part_one($puzzle);
solve_part_two($puzzle);

exit( 0 );

sub solve_part_one(Grid $puzzle) {
	my $xmas_count = 0;
	for $puzzle.coords('X') -> $x {
		for adjacent_dirs(AdjacencyRule::QUEEN) -> $dir {
			($x, $x.offset($dir, 1), $x.offset($dir, 2), $x.offset($dir, 3))
				==> map( -> $coord {$puzzle.get($coord)} )
				==> join('')
				==> my $word;
			$xmas_count += 1 if $word eq 'XMAS';
		}
	}
	say "Part One: The number of XMAS is $xmas_count";
}

sub solve_part_two(Grid $puzzle) {
	my $xmas_count = 0;
	$puzzle.set_rule(AdjacencyRule::BISHOP);
	my $valid = set <MMSS MSSM SSMM SMMS>;
	for $puzzle.coords('A') -> $a {
		#say "A $a";
		$puzzle.neighbors($a) ==> map( -> $n { $puzzle.get($n) } )
			==> join('')
			==> my $neighbor_string;
		#say $neighbor_string;
		if $neighbor_string[0] (elem) $valid {
			$xmas_count += 1;
		}
	}
	say "Part Two: The number of X-MAS is $xmas_count";	
}
