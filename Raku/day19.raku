#!/usr/bin/env raku

use lib $*PROGRAM.dirname ~ '/lib';
use AOC::Util;
#use AOC::Geometry;
#use AOC::Grid;

my $INPUT_PATH = '../Input';
#my $INPUT_FILE = 'day19_test.txt';
my $INPUT_FILE = 'day19_challenge.txt';
my @input = read_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2024, Day 19: Linen Layout";

my @patterns = @input[0].split(', ', :skip-empty).sort({ $^a.chars <=> $^b.chars });
#my %patterns = parse_available_patterns(@input[0]);
say @patterns.join(' ');
my @designs = @input[2..*];

solve_part_one(@designs);
#solve_part_two(@input);

exit( 0 );

sub solve_part_one(@designs) {
	my $possible = 0;
	my $i = 1;
	for @designs -> $d {
		$possible += 1 if design_is_possible($d);
		say $i++;
	}

	say "Part One: the number of possible designs is $possible"; # 278 wrong
}

sub solve_part_two(@input) {
	
}

sub design_is_possible($d --> Bool) {
	return True if $d.chars == 0;
	for @patterns -> $pat {
		last if $pat.chars > $d.chars;
		if $d.starts-with($pat) {
			my $rem = $d.substr($pat.chars);
			return True if design_is_possible($rem);
		}
	}
	return False;
}

sub parse_available_patterns(Str $line --> Hash) {
	my @patterns = $line.split(', ', :skip-empty);
	my %patterns = ();
	for @patterns -> $p {
		%patterns{$p} = 1;
	}
	%patterns;
}

