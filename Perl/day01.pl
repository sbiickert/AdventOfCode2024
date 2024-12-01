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

my @data = parse_data(@input);

solve_part_one(@data);
solve_part_two(@data);

exit( 0 );

sub solve_part_one(@data) {
	my $distance = 0;
	my $count = scalar(@{$data[0]});
	for my $i (0..$count-1) {
		my $d = abs($data[1][$i] - $data[0][$i]);
		$distance += $d;
	}
	say "Part One: The total distance is $distance";
}

sub solve_part_two(@data) {
	# Turn second list into a frequency map
	my %map = ();
	for my $num (@{$data[1]}) {
		$map{$num} ++;
	}

	my $similarity_score = 0;
	for my $num (@{$data[0]}) {
		if (exists $map{$num}) {
			my $sim = $num * $map{$num};
			$similarity_score += $sim;
		}
	}
	say "Part Two: The similarity score is $similarity_score";
}

sub parse_data(@input) {
	my @data = ();
	for my $line (@input) {
		my @nums = split(/\s+/, $line);
		push(@data, \@nums);
	}

	my @pivot = pivot_matrix(@data);
	for my $i (0..1) {
		my @sorted = sort {$a <=> $b} @{$pivot[$i]};
		$pivot[$i] = \@sorted;
	}

	return @pivot;
}