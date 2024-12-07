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

my @possibles = solve_part_one(@input);
solve_part_two(@input, @possibles);

exit( 0 );

sub solve_part_one(@input) {
	my @possibles = @input.grep(&is_value_possible1);
	my $sum = 0;
	for @possibles -> @poss {
		#say "Possible: " ~ @poss.raku;
		$sum += @poss.first;
	}

	say "Part One: the sum of the possibles is $sum";
	@possibles;
}

sub solve_part_two(@input, @part_one_possibles) {
	@part_one_possibles = map( -> @poss {@poss} ); # Seq to Array
	#dd @part_one_possibles;
	my @unsolved = ();
	for @input -> @equation {
		if (@equation.Seq (elem) @part_one_possibles) == False {
			@unsolved.push(@equation);
		}
	}
	#dd @unsolved;
	my @possibles = @unsolved.grep(&is_value_possible2);
	my $sum = 0;
	for @part_one_possibles -> @poss {
		$sum += @poss.first;
	}
	for @possibles -> @poss {
		#say "Possible: " ~ @poss.raku;
		$sum += @poss.first;
	}

	say "Part Two: the sum of the possibles is $sum";	
}

sub is_value_possible1(@arr --> Bool) {
	#dd @arr;
	if @arr.elems == 2 {
		#say "Break";
		return @arr[0] == @arr[1];
	}
	my @mul_arr = (@arr.first, @arr[1] * @arr[2], @arr[3..*]).flat;
	if is_value_possible1(@mul_arr) { return True; }
	my @add_arr = (@arr.first, @arr[1] + @arr[2], @arr[3..*]).flat;
	is_value_possible1(@add_arr);
}

sub is_value_possible2(@arr --> Bool) {
	#dd @arr;
	if @arr.elems == 2 {
		#say "Break";
		return @arr[0] == @arr[1];
	}
	my @mul_arr = (@arr.first, @arr[1] * @arr[2], @arr[3..*]).flat;
	if is_value_possible2(@mul_arr) { return True; }
	my @add_arr = (@arr.first, @arr[1] + @arr[2], @arr[3..*]).flat;
	if is_value_possible2(@add_arr) { return True; }
	my @con_arr = (@arr.first, @arr[1] ~ @arr[2], @arr[3..*]).flat;
	is_value_possible2(@con_arr);
}
