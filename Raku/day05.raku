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
	@updates
		==> grep(&is_update_correct)
		==> map( -> @u { @u[(@u.elems-1)/2] } ) # remove all but the middle value
		==> sum()
		==> my $sum_middle;	

	say "Part One: the sum of middle pages is $sum_middle";
}

sub solve_part_two() {
	@updates
		==> grep(&is_update_incorrect)
		==> map( -> @u { correct_update(@u) })
		==> map( -> @u { @u[(@u.elems-1)/2] } ) # remove all but the middle value
		==> sum()
		==> my $sum_middle;	

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
			# This page is followed by a page that should be before it
			$is_correct = False;
			last;
		}
	}
	$is_correct;
}

sub correct_update(@u --> Array) {
	my @result = @u.map( -> $page {$page.Int} );

	my $is_correct = is_update_correct(@result);
	my $i = 0;
	while !$is_correct {
		if !is_page_at_index_correct(@result, $i) {
			# Simple, bubble page towards end
			my $temp = @result[$i];
			@result[$i] = @result[$i+1];
			@result[$i+1] = $temp;
			$is_correct = is_update_correct(@result);
		}
		$i = ($i + 1) % @result.elems;
	}

	return @result;
}

sub parse_following(@input) {
	my %result = ();
	for @input -> $line {
		$line ~~ m/ (\d+) \| (\d+) /;
		my ($key, $val) = (Int($0), Int($1));
		push(%result{$key}, $val);
	}
	for %result.keys -> $key {
		%result{$key} = %result{$key}.Set();
	}
	return %result;
}

sub parse_updates(@input) {
	@input
		==> map( -> $line { $line.split(',') })
		==> my @result;
	@result;
}

