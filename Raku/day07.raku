#!/usr/bin/env raku

use lib $*PROGRAM.dirname ~ '/lib';
use AOC::Util;
#use AOC::Geometry;
#use AOC::Grid;

my $INPUT_PATH = '../Input';
#my $INPUT_FILE = 'day07_test.txt';
my $INPUT_FILE = 'day07_challenge.txt';
my @input = read_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2024, Day 7: Bridge Repair";

@input = @input.map( -> $line { $line.split(/ \:?\s /); } );

solve_part_one(@input);
#solve_part_two(@input);

exit( 0 );

sub solve_part_one(@input) {
	my @possibles = @input.grep(&is_value_possible);
	my $sum = 0;
	for @possibles -> @poss {
		#say "Possible: " ~ @poss.raku;
		$sum += @poss.first;
	}

	say "Part One: the sum of the possibles is $sum";
}

sub solve_part_two(@input) {
	
}

sub is_value_possible(@arr --> Bool) {
	#dd @arr;
	if @arr.elems == 2 {
		#say "Break";
		return @arr[0] == @arr[1];
	}
	my @mul_arr = (@arr.first, @arr[1] * @arr[2], @arr[3..*]).flat;
	if is_value_possible(@mul_arr) { return True; }
	my @add_arr = (@arr.first, @arr[1] + @arr[2], @arr[3..*]).flat;
	is_value_possible(@add_arr);
}
