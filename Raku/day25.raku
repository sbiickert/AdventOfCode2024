#!/usr/bin/env raku

use lib $*PROGRAM.dirname ~ '/lib';
use AOC::Util;
use AOC::Geometry;
use AOC::Grid;

my $INPUT_PATH = '../Input';
#my $INPUT_FILE = 'day25_test.txt';
my $INPUT_FILE = 'day25_challenge.txt';
my @input = read_grouped_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2024, Day 25: Code Chronicle";

my @keys;
my @locks;

parse_keys_and_locks(@input);

solve_part_one();
#solve_part_two(@input);

exit( 0 );

sub solve_part_one() {
	my $fit_count = 0;
	for @keys -> %k {
		for @locks -> %l {
			my $fit = True;
			for (0..%k{'h'}.end) -> $col {
				if %k{'h'}[$col] + %l{'h'}[$col] > 5 {
					$fit = False;
				}
			}
			if $fit {
				$fit_count++;
				#say "Key %k{'h'} and lock %l{'h'} fit"
			}
		}
	}

	say "Part One: the number of fitting pairs is $fit_count";
}

sub solve_part_two(@input) {
	
}

sub parse_keys_and_locks(@input) {
	@keys = ();
	@locks = ();

	for @input -> @group {
		my $grid = Grid.new(default => 'X', rule => AdjacencyRule::ROOK);
		$grid.load(@group);
		my $is_key = $grid.get(Coord.origin) eq '.';
		
		my %thing = (grid => $grid);
		my @heights = ();
		for (0..$grid.extent.max.x) -> $col {
			for (0..$grid.extent.max.y) -> $y {
				my $row = $is_key ?? $y !! $grid.extent.max.y - $y;
				if $grid.get(Coord.from_ints($col,$row)) eq '#' {
					my $h = $is_key ?? $grid.extent.max.y - $row !! $row;
					@heights.push($h);
					last;
				}
			}
		}
		%thing{'h'} = @heights;
		if $is_key { @keys.push(%thing) }
		else       { @locks.push(%thing) }
	}
}
