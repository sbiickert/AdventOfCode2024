#!/usr/bin/env raku

use lib $*PROGRAM.dirname ~ '/lib';
use AOC::Util;
#use AOC::Geometry;
#use AOC::Grid;

my $INPUT_PATH = '../Input';
#my $INPUT_FILE = 'day22_test.txt';
my $INPUT_FILE = 'day22_challenge.txt';
my @input = read_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2024, Day 22: Monkey Market";

solve_part_one(@input);
#solve_part_two(@input);

exit( 0 );

sub solve_part_one(@input) {
	my $sum = 0;
	my @nums = @input.map({.Int});
	for @nums -> $n {
		my $num = $n;
		for (1..2000) -> $i {
			$num = pseudo($num);
		}
		say $num;
		$sum += $num;
	}

	say "Part One: the sum of the 2000th numbers is $sum";
}

sub solve_part_two(@input) {
	
}

sub pseudo(Int $in --> Int) {
	my $temp = $in * 64;
	my $out = prune(mix($temp, $in));
	$temp = Int($out / 32);
	$out = prune(mix($temp, $out));
	$temp = $out * 2048;
	$out = prune(mix($temp, $out));
	$out;	
}

sub mix(Int $value, Int $secret --> Int) {
	$value +^ $secret;
}

sub prune(Int $secret --> Int) {
	$secret % 16777216;
}
