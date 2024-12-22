#!/usr/bin/env raku

use lib $*PROGRAM.dirname ~ '/lib';
use AOC::Util;
#use AOC::Geometry;
#use AOC::Grid;

my $INPUT_PATH = '../Input';
#my $INPUT_FILE = 'day22_test.txt';
my $INPUT_FILE = 'day22_challenge.txt';
my @input = read_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2024, Day 22: Monkey Market";

class Buyer {...}

my @secrets = @input.map( -> $i {Int($i)} );
my @buyers = @secrets.map( -> $s { Buyer.new(secret => $s) } );

solve_part_one(@buyers);
solve_part_two(@buyers);

exit( 0 );

sub solve_part_one(@buyers) {
	@buyers ==> map({.secret_2000})
		==> sum()
		==> my $sum;
	say "Part One: the sum of the 2000th numbers is $sum";
}

sub solve_part_two(@buyers) {
	my %all_patterns = ();
	for @buyers -> $b {
		for $b.pattern_price.keys -> $pattern {
			%all_patterns{$pattern} = 1;
		}
	}
	say %all_patterns.elems;
	my $max = 0;
	my $max_pattern = '';
	for %all_patterns.keys -> $pattern {
		my @prices = @buyers.map( -> $b { $b.price($pattern) } );
		my $sum = @prices.sum();
		if $sum > $max {
			$max = $sum;
			$max_pattern = $pattern;
			say "$max_pattern $max";
		}
	}

	say "Part Two: the sum of prices for $max_pattern is $max"; # 1900 low
}

sub pseudo(Int $in --> Int) {
	my $temp = $in * 64;
	my $out = ($temp +^ $in) % 16777216;
	$temp = Int($out / 32);
	$out = ($temp +^ $out) % 16777216;
	$temp = $out * 2048;
	$out = ($temp +^ $out) % 16777216;
	$out;	
}

class Buyer {
	has Int $.secret;
	has %.pattern_price;
	has Int $.secret_2000;
#	has @.price_patterns;

	submethod TWEAK() {
		my @secrets = ($!secret);
		my @prices = (Int($!secret.Str.substr(*-1)));
		my @diffs = (0);
		my %pattern_price = ();
		my @price_patterns = ();
		for (1..2000) -> $i {
			my $secret = pseudo(@secrets[*-1]);
			@secrets.push($secret);
			my $price = Int($secret.Str.substr(*-1));
			@prices.push($price);
			@diffs.push(@prices[*-1] - @prices[*-2]);
			if @diffs.elems < 4 { next }
			if @diffs.tail < 1 { next }
			my $pattern = @diffs[*-4..*-1].join(',');
			if %pattern_price{$pattern}:exists == False {
				%pattern_price{$pattern} = $price;
#				my $c = @price_patterns[$price] ~~ Array ?? @price_patterns[$price].elems !! 0;
#				@price_patterns[$price][$c] = $pattern;
			}
		}
		%!pattern_price = %pattern_price;
		$!secret_2000 = @secrets.tail;
#		@!price_patterns = @price_patterns;
	}

	method price(Str $pattern --> Int) {
		if %.pattern_price{$pattern}:exists {
			return %.pattern_price{$pattern};
		}
		0
	}
}
