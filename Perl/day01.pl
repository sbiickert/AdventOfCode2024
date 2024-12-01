#!/usr/bin/env perl
use v5.40;
# use feature 'class';
# no warnings qw( experimental::class );

our $directory;
BEGIN { use Cwd; $directory = cwd; }
use lib $directory . '/lib';

use feature 'signatures';
use Data::Printer;
#use Storable 'dclone';

use AOC::Util;
#use AOC::Geometry;
#use AOC::Grid;

# my $INPUT_FILE = 'day01_test.txt';
my $INPUT_FILE = 'day01_challenge.txt';
my @input = read_input("../input/$INPUT_FILE");

say "Advent of Code 2024, Day 01: Historian Hysteria";

my @list1 = ();
my @list2 = ();

make_lists(@input);

solve_part_one();
solve_part_two();

exit( 0 );

sub solve_part_one(@input) {
	my $distance = 0;
	for my $i (0..$#list1) {
		my $d = abs($list2[$i] - $list1[$i]);
		$distance += $d;
	}
	say "Part One: The total distance is $distance";
}

sub solve_part_two() {
	# Turn List 2 into a frequency map
	my %map = {};
	for my $num (@list2) {
		$map{$num} ++;
	}

	my $similarity_score = 0;
	for my $num (@list1) {
		if (exists $map{$num}) {
			my $sim = $num * $map{$num};
			$similarity_score += $sim;
		}
	}
	say "Part Two: The similarity score is $similarity_score";
}

sub make_lists(@input) {
	for my $line (@input) {
		my @nums = split(/\s+/, $line);
		push(@list1, $nums[0]);
		push(@list2, $nums[1]);
	}
	@list1 = sort {$a <=> $b} @list1;
	@list2 = sort {$a <=> $b} @list2;
}