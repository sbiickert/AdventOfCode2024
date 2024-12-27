#!/usr/bin/env raku

use lib $*PROGRAM.dirname ~ '/lib';
use AOC::Util;
#use AOC::Geometry;
#use AOC::Grid;

my $INPUT_PATH = '../Input';
#my $INPUT_FILE = 'day03_test.txt';
my $INPUT_FILE = 'day03_challenge.txt';
my @input = read_grouped_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2024, Day 3: Mull It Over";

my $joined = @input[0].join('');
solve_part_one($joined);
solve_part_two($joined);

exit( 0 );

sub solve_part_one(Str $input) {
	my $sum = sum_for_line($input);
	say "Part One: the sum is $sum";
}

sub solve_part_two(Str $input) {
	$input.split('do()', :skip-empty)
		==> map( -> $s { $s.split("don't()", :skip-empty).first } )
		==> my @enabled_sections;
	my $line = @enabled_sections.join('');
	my $sum = sum_for_line($line);	
	say "Part Two: the sum is $sum";
}

sub sum_for_line(Str $line --> Int) {
	$line ~~ m:global/ mul \( (\d+) \, (\d+) \) /
		==> map( -> $m { $m[0] * $m[1] })
		==> sum()
		==> my @line_sum;
	@line_sum[0];
}