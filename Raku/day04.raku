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
		#say "X $x";
		for $puzzle.neighbors($x) -> $m {
			if $puzzle.get($m) eq 'M' {
				#say "M $m";
				my $d = $x.delta($m);
				my $a = $m.add($d);
				my $s = $a.add($d);
				if $puzzle.get($a) eq 'A' && $puzzle.get($s) eq 'S' {
					$xmas_count += 1;
				}
			}
		}
	}
	say "Part One: The number of XMAS is $xmas_count";
}

sub solve_part_two(Grid $puzzle) {
	my $xmas_count = 0;
	$puzzle.set_rule(AdjacencyRule::BISHOP);
	my $valid = ('MMSS', 'MSSM', 'SSMM', 'SMMS');
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
	say "Part One: The number of XMAS is $xmas_count";	
}
