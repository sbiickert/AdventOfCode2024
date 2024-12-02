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
solve_part_two(@input);

exit( 0 );

sub solve_part_one(@input) {
	@input ==> map( -> $line {split(' ', $line)} )
		   ==> grep( &is_report_safe )
		   ==> my @safe;
	say "Part One: Number of safe reports is " ~ @safe.elems;
}

sub solve_part_two(@input) {
	@input ==> map( -> $line {split(' ', $line)} )
		   ==> grep( &is_report_unsafe )
		   ==> my @unsafe;

	my $safe_count = @input.elems - @unsafe.elems;

	for @unsafe -> @u {
		#dd @u;
		my $is_safe = False;
		for (0..@u.elems-1) -> $skip {
			my @slice_list = ();
			for (0..@u.elems-1) -> $i {
				@slice_list.push($i) if $i != $skip;
			}
			my @dampened = @u[@slice_list];
			#dd @dampened;
			$is_safe = is_report_safe(@dampened);
			if $is_safe { last; }
		}
		if $is_safe { $safe_count += 1; }
	}
		

	say "Part Two: Number of safe reports is " ~ $safe_count;
}

sub is_report_unsafe(@report --> Bool) {
	!is_report_safe(@report);
}

sub is_report_safe(@report --> Bool) {
	my $is_safe = True;
	my $direction = 0; # Unset to start
	for (1..@report.elems-1) -> $i {
		my $diff = @report[$i] - @report[$i-1];
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
	$is_safe;
}
