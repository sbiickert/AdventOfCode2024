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

solve_part_one(@input[0]);
solve_part_two(@input[0]);

exit( 0 );

sub solve_part_one(@input) {
	my $sum = 0;
	for @input -> $line {
		given $line {
			m:global/ mul \( (\d+) \, (\d+) \) /
				==> map( -> $m { $m[0] * $m[1] })
				==> sum()
				==> my @line_sum;
			$sum += @line_sum.first;
		}
	}
	say "Part One: the sum is $sum";
}

sub solve_part_two(@input) {
	my $sum = 0;
	my $on = True;
	for @input -> $line {
		given $line {
			my @mul_matches = m:global/ mul \( (\d+) \, (\d+) \) /;
			my @mul = @mul_matches.map( -> $m { $m.from } );
			
			my @do_matches = m:global/ do \( \) /;
			my @do = @do_matches.map( -> $m { $m.from } );
			
			my @dont_matches = m:global/ don \'t \( \) /;
			my @dont = @dont_matches.map( -> $m { $m.from } );

			@do.push(100000); #arbitrary after string end
			@dont.push(100000); # ditto

			while @mul.elems > 0 {
				if @mul.first < @do.first && @mul.first < @dont.first {
					my $m = @mul_matches.shift();
					@mul.shift();
					my $product = $m[0] * $m[1];
					if $on {
						$sum += $product;
					}
					#say "$on\t$product\t" ~ ($on ?? $product !! 0);
				}
				elsif @do.first < @dont.first {
					$on = True;
					#say "on " ~ @do.first;
					@do.shift();
				}
				else {
					$on = False;
					#say "off " ~ @dont.first;
					@dont.shift();
				}
			}
		}
	}
	say "Part Two: the sum is $sum";
}

