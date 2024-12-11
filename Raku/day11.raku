#!/usr/bin/env raku

use lib $*PROGRAM.dirname ~ '/lib';
use AOC::Util;
#use AOC::Geometry;
#use AOC::Grid;

my $INPUT_PATH = '../Input';
#my $INPUT_FILE = 'day11_test.txt';
my $INPUT_FILE = 'day11_challenge.txt';
my @input = read_grouped_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2024, Day 11: Plutonian Pebbles";

my @stones = parse_stones(@input[0].first);

my $count1 = solve_part(@stones, 25);
say "Part One: the number of stones is $count1";

my $count2 = solve_part(@stones, 75);
say "Part Two: the number of stones is $count2";

exit( 0 );


sub solve_part(@stones, Int $blinks) {
	my $sum = 0;
	for @stones -> $stone {
		$sum += count_stones($stone, $blinks);
	}
	return $sum;
}

sub parse_stones(Str $line --> Array) {
	$line.split(' ', :skip-empty)
		==> map( -> $s {Int($s)})
		==> my @numbers;
	@numbers;
}

my %_cache = ();
sub count_stones(Int $stone, Int $blinks --> Int) {
	my $key = "$stone $blinks";
	if %_cache{$key}:exists { return %_cache{$key} }

	if $blinks <= 0 {
		%_cache{$key} = 1;
		return 1;
	}

	my $stone_str = "$stone";
	if $stone == 0 {
		my $result = count_stones(1, $blinks-1);
		%_cache{$key} = $result;
		return $result;
	}
	elsif $stone_str.chars %% 2 {
		my $left = Int($stone_str.substr(0, $stone_str.chars/2));
		my $right = Int($stone_str.substr($stone_str.chars/2));
		my $result = count_stones($left, $blinks-1) + count_stones($right, $blinks-1);
		%_cache{$key} = $result;
		return $result;		
	}
	my $result = count_stones($stone * 2024, $blinks-1);
	%_cache{$key} = $result;
	return $result;
}
