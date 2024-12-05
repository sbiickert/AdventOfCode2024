#!/usr/bin/env raku

use lib $*PROGRAM.dirname ~ '/lib';
use AOC::Util;
#use AOC::Geometry;
#use AOC::Grid;

my $INPUT_PATH = '../Input';
#my $INPUT_FILE = 'day05_test.txt';
my $INPUT_FILE = 'day05_challenge.txt';
my @input = read_grouped_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2024, Day 5: Print Queue";

my %following = parse_following(@input[0]);
my @updates = parse_updates(@input[1]);

solve_part_one();
#solve_part_two(@input);

exit( 0 );

sub solve_part_one() {
	my $sum_middle = 0;

	for @updates -> @u {
		my $is_correct = True;

		for 0 .. @u.elems-2 -> $i {
			for $i+1 .. @u.elems-1 -> $j {
				my $num = Int(@u[$i]);
				my $set = %following{@u[$j]};
				if $num (elem) $set {
					$is_correct = False;
					last;
				}
			}
			last if !$is_correct;
		}

		if $is_correct {
			my $mid = (@u.elems-1) / 2;
			#say @u ~ " is correct";
			$sum_middle += @u[$mid];
		}
	}

	say "Part One: the sum of middle pages is $sum_middle";
}

sub solve_part_two(@input) {
	
}

sub parse_following(@input) {
	my %result = ();
	for @input -> $line {
		$line ~~ m/ (\d+) \| (\d+) /;
		my $key = Int($0);
		my $val = Int($1);
		if %result{$key}:exists == False {
			%result{$key} = ().SetHash;
		}
		(%result{$key}).set($val);
	}
	for %result.keys -> $key {
		%result{$key} = %result{$key}.Set();
	}
	#dd %result;
	return %result;
}

sub parse_updates(@input) {
	my @result = ();
	for @input -> $line {
		my @pages = $line.split(',');
		#dd @pages;
		@result.push(@pages);
	}
	return @result;
}

