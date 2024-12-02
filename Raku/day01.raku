#!/usr/bin/env raku

use lib $*PROGRAM.dirname ~ '/lib';
use AOC::Util;
#use AOC::Geometry;
#use AOC::Grid;

my $INPUT_PATH = '../Input';
#my $INPUT_FILE = 'day01_test.txt';
my $INPUT_FILE = 'day01_challenge.txt';
my @input = read_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2024, Day 1: Historian Hysteria";

my @list1; my @list2;
parse_data(@input);

solve_part_one();
solve_part_two();

exit( 0 );

sub solve_part_one() {
	(0..@list1.elems-1) ==> map( -> $x { abs(@list1[$x] - @list2[$x]); })
						==> sum() #reduce(&infix:<+>)
						==> my $total;
	say "Part One: $total";
}

sub solve_part_two() {
	my %frequency_map = ();
	for @list2 -> $num {
		%frequency_map{$num} += 1;
	}
	my $similarity = 0;
	for @list1 -> $num {
		if %frequency_map{$num}:exists {
			$similarity += $num * %frequency_map{$num};
		}
	}
	say "Part Two: $similarity";
}


sub parse_data(@input) {
	my @data = ();
	for @input -> $line {
		my @nums = split(/\s+/, $line);
		@data.push(@nums);
	}
	
	@data = pivot_matrix(@data);
	
	@list1 = @data[0].sort;
	@list2 = @data[1].sort;
}