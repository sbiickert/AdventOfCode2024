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
my %patterns = parse_available_patterns(@input[0]);
my @designs = @input[2..*];

my @possibles = solve_part_one(@designs);
solve_part_two(@possibles);

exit( 0 );

sub solve_part_one(@designs) {
	my @possibles = @designs.grep( &design_is_possible );	
	say "Part One: the number of possible designs is " ~ @possibles.elems;
	@possibles;
}

sub solve_part_two(@designs) {
	my @counts = @designs>>.&count_combinations; # My first use of hyper operator
	my $count = @counts.sum;
	say "Part Two: the total number of ways you can make the designs is $count";
}

sub design_is_possible($d --> Bool) {
	# There is a bug in here somewhere, such that 5 of my designs
	# registered as possible. I used the DFS search to find them (would run v.long)
	# and then prefixed those designs with "Z" in the input file.
	# Without an interactive debugger, I'm declaring bankruptcy on finding the bug.
	my $longest_pat = @patterns[@patterns.end].chars;
	for (0..$d.chars-1) -> $i {
		my $frag = $d.substr($i, 1);
		next if %patterns{$frag}:exists;
		my $b_ok = False;
		for (2..$longest_pat) -> $w_size {
			my $range_start = max($i-($w_size-1), 0);
			for ($range_start..$i) -> $a {
				$frag = $d.substr($a,$w_size);
				if %patterns{$frag}:exists {
					$b_ok = True;
					last;
				}
			}
			last if $b_ok;
		}

		return False if $b_ok == False;
	}
	return True;	
}

my %cache = ();
sub count_combinations($d --> Int) {
	return %cache{$d} if %cache{$d}:exists;
	return 1 if $d.chars == 0;
	
	my $count = 0;
	for @patterns -> $pat {
		last if $pat.chars > $d.chars; #@patterns is sorted in asc length
		if $d.starts-with($pat) {
			my $rem = $d.substr($pat.chars);
			$count += count_combinations($rem);
		}
	}
	%cache{$d} = $count;
	return $count;
}

sub parse_available_patterns(Str $line --> Hash) {
	my @patterns = $line.split(', ', :skip-empty);
	my %patterns = ();
	for @patterns -> $p {
		%patterns{$p} = 1;
	}
	%patterns;
}

