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
#dd %following;
my @updates = parse_updates(@input[1]);

solve_part_one();
solve_part_two();

exit( 0 );

sub solve_part_one() {
	my $sum_middle = 0;

	for @updates -> @u {
		if is_update_correct(@u) {
			my $mid = (@u.elems-1) / 2;
			$sum_middle += @u[$mid];
		}
	}

	say "Part One: the sum of middle pages is $sum_middle";
}

sub solve_part_two() {
	my $sum_middle = 0;

	@updates
		==> grep(&is_update_incorrect)
		==> map( -> @u { correct_update(@u) })
		==> my @corrected;

	for @corrected -> @u {
		my $mid = (@u.elems-1) / 2;
		$sum_middle += @u[$mid];
	}
	
	say "Part Two: the sum of middle pages in corrected updates is $sum_middle";
}

sub is_update_incorrect(@u --> Bool) {
	!is_update_correct(@u);
}

sub is_update_correct(@u --> Bool) {
	my $is_correct = True;

	for 0 .. @u.elems-2 -> $i {
		$is_correct = is_page_at_index_correct(@u, $i);
		last if !$is_correct;
	}
	$is_correct;
}

sub is_page_at_index_correct(@u, $i --> Bool) {
	my $is_correct = True;
	for $i+1 .. @u.elems-1 -> $j {
		my $num = Int(@u[$i]);
		my $set = %following{@u[$j]};
		if $num (elem) $set {
			$is_correct = False;
			last;
		}
	}
	$is_correct;
}

sub correct_update(@u --> Array) {
	my @result = @u.map( -> $page {$page.Int} );

	#say @result;
	my $is_correct = False;
	my $i = 0;
	while !$is_correct {
		if !is_page_at_index_correct(@result, $i) {
			# Simple, bubble page towards end
			#say "Moving page at $i";
			my $temp = @result[$i];
			@result[$i] = @result[$i+1];
			@result[$i+1] = $temp;
			#say @result;
		}
		$is_correct = is_update_correct(@result);
		$i = ($i + 1) % @result.elems;
	}

	return @result;
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

