#!/usr/bin/env raku

use lib $*PROGRAM.dirname ~ '/lib';
use AOC::Util;
#use AOC::Geometry;
#use AOC::Grid;

my $INPUT_PATH = '../Input';
#my $INPUT_FILE = 'day02_test.txt';
my $INPUT_FILE = 'day02_challenge.txt';
my @input = read_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2024, Day 2: Red-Nosed Reports";

solve_part_one(@input);
#solve_part_two(@input);

exit( 0 );

sub solve_part_one(@input) {
	my $safe_count = 0;
	for @input -> $line {
		my @nums = split(' ', $line);
		my $is_safe = True;
		my $direction = 0; # Unset to start
		for (1..@nums.elems-1) -> $i {
			my $diff = @nums[$i] - @nums[$i-1];
			if abs($diff) < 1 || abs($diff) > 3 {
				$is_safe = False;
				last;
			}
			my $current_direction = $diff > 0 ?? 1 !! -1;
			if $direction == 0 {
				$direction = $current_direction;
			}
			if $current_direction != $direction {
				$is_safe = False;
				last;
			}
		}
		$safe_count += 1 if $is_safe;
	}
	say "Part One: Number of safe reports is $safe_count";
}

sub solve_part_two(@input) {
	
}
