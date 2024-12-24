#!/usr/bin/env raku

use lib $*PROGRAM.dirname ~ '/lib';
use AOC::Util;
#use AOC::Geometry;
#use AOC::Grid;

my $INPUT_PATH = '../Input';
#my $INPUT_FILE = 'day24_test.txt';
my $INPUT_FILE = 'day24_challenge.txt';
my @input = read_grouped_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2024, Day 24: Crossed Wires";

class Gate {...}

my %values = parse_inputs(@input[0]);
my %structure = parse_structure(@input[1]);


solve_part_one();
#solve_part_two(@input);

exit( 0 );

sub solve_part_one() {
	my $unset_count = 1;
	while $unset_count > 0 {
		$unset_count = 0;
		for %structure.kv -> $name,$gate {
			next if $gate.output >= 0; # Already done
			if (%values{$gate.inputA}:exists) && (%values{$gate.inputB}:exists) {
				$gate.valueA = %values{$gate.inputA};
				$gate.valueB = %values{$gate.inputB};
				$gate.calculate_output();
				%values{$name} = $gate.output;
			}
			else { $unset_count++ }
		}
	}

	my $z_value = make_z_value();

	say "Part One: the value in the z outputs is $z_value";
}

sub solve_part_two(@input) {
	
}

sub parse_inputs(@input --> Hash) {
	my %result = ();
	for @input -> $line {
		$line ~~ / (\w+) \: \s (\d) /;
		%result{$0.Str} = Int($1);
	}
	return %result;
}

sub parse_structure(@input --> Hash) {
	my %result = ();
	for @input -> $line {
		$line ~~ / (\w+) \s (\w+) \s (\w+)\W+(\w+) /;
		%result{$3.Str} = Gate.new(inputA => $0.Str, op => $1.Str, inputB => $2.Str);
	}
	return %result;
}

sub make_z_value( --> Int) {
	my @z_keys = %values.keys.grep( -> $key { $key.starts-with('z') } ).sort().reverse();
	my $z_str = '0b';
	for @z_keys -> $zk {
		$z_str ~= %values{$zk};
	}
	
	return Int($z_str);
}

class Gate {
	has Str $.inputA;
	has Str $.inputB;
	has Str $.op;
	has Int $.valueA is rw = -1;
	has Int $.valueB is rw = -1;
	has Int $.output is rw = -1;

	method calculate_output() {
		die if $.valueA < 0;
		die if $.valueB < 0;
		given $.op {
			when 'AND' { $.output = $.valueA && $.valueB }
			when 'OR'  { $.output = $.valueA || $.valueB }
			when 'XOR' {
				my $xor = $.valueA ^^ $.valueB;
				$.output = $xor ~~ Int ?? $xor !! 0;
			}
		}
		say self.debug_str;
	}

	method debug_str(--> Str) {
		"$.inputA [$.valueA] $.op $.inputB [$.valueB] is $.output";
	}
}
