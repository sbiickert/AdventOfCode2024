#!/usr/bin/env raku

use lib $*PROGRAM.dirname ~ '/lib';
use AOC::Util;
#use AOC::Geometry;
#use AOC::Grid;

my $INPUT_PATH = '../Input';
my $INPUT_FILE = 'day11_test.txt';
#my $INPUT_FILE = 'day11_challenge.txt';
my @input = read_grouped_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2024, Day 11: Plutonian Pebbles";

solve_part_one(@input[2].first);
#solve_part_two(@input);

exit( 0 );

class Stone {
	has $.number: Int is rw;
	has $.prev: ?Stone is rw;
	has $.next: ?Stone is rw;

	method blink(--> Stone) {
		#say $.number;
		if $.number == 0 {
			#say "0 --> 1";
			$.number = 1;
		}
		elsif "$.number".chars %% 2 {
			#say "split $.number";
			my $str_num = "$.number";
			my $num1 = Int($str_num.substr(0, $str_num.chars/2));
			my $num2 = Int($str_num.substr($str_num.chars/2));
			my $stone1 = Stone.new(number => $num1, prev => $.prev, next => Nil);
			$.prev.next = $stone1 if $.prev.raku ne "Any";
			my $stone2 = Stone.new(number => $num2, prev => $stone1, next => $.next);
			$stone1.next = $stone2;
			$.next.prev = $stone2 if $.next.raku ne "Any";
			return $stone1;
		}
		else {
			#say "multiply by 2024";
			$.number *= 2024;
		}
		self;
	}

	method print_stones() {
		print $.number ~ " ";
		if $.next.raku eq "Any" {
			say '';
			return;
		}
		$.next.print_stones();
	}

	method count_stones(--> Int) {
		if $.next.raku eq "Any" { return 1 }
		return $.next.count_stones() + 1;
	}
}

sub solve_part_one(Str $input) {
	my $stone = parse_stones($input);
	for 1..10 -> $i {
		$stone.print_stones();
		say $i;
		$stone = blink($stone);
	}
	$stone.print_stones();
	say $stone.count_stones();
}

sub solve_part_two(@input) {
	
}

sub parse_stones(Str $line --> Stone) {
	my @numbers = $line.split(' ');
	my $first_stone = Stone.new(number => @numbers.shift(), prev => Nil, next => Nil);
	my $prev_stone = $first_stone;
	for @numbers -> $num {
		my $stone = Stone.new(number => $num, prev => $prev_stone, next => Nil);
		$prev_stone.next = $stone;
		$prev_stone = $stone;
	}
	return $first_stone
}

sub blink(Stone $first --> Stone) {
	my $stone = $first;
	my $next = $stone.next;
	$stone = $stone.blink();
	my $result = $stone;

	while True {
		if $next.raku eq "Any" { last }
		$stone = $next;
		$next = $stone.next;
		$stone = $stone.blink();
	}
	
	$result;
}
