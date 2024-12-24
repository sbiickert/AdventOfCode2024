#!/usr/bin/env raku

use lib $*PROGRAM.dirname ~ '/lib';
use AOC::Util;
#use AOC::Geometry;
#use AOC::Grid;

my $INPUT_PATH = '../Input';
my $INPUT_FILE = 'day24_test.txt';
#my $INPUT_FILE = 'day24_challenge.txt';
my @input = read_grouped_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2024, Day 24: Crossed Wires";

class Gate {...}

my %inputs = parse_inputs(@input[0]);
my %structure = parse_structure(@input[1]);


solve_part_one(%inputs.clone, %structure);
solve_part_two(%inputs.clone, %structure);

exit( 0 );

sub solve_part_one(%values, %structure) {
	my $z_value = calculate(%values, %structure);

	say "Part One: the value in the z outputs is $z_value";
}

sub solve_part_two(%values, %structure) {
	my $x_value = get_value(%values, 'x');
	my $y_value = get_value(%values, 'y');

	my @output_wires = %structure.keys;

#	say @output_wires.combinations(8).elems;
	my $result = '';
	
	for @output_wires.combinations(4) -> @set4 {
		my %copy = %structure.clone;
		say @set4;
		for @set4.combinations(2) -> @pair0 {
			my @pair1 = (@set4 (-) @pair0).keys;
			say "Swapping " ~ @pair0 ~ ' and ' ~ @pair1;
			my $temp = %copy{@pair0[0]};
			%copy{@pair0[0]} = %copy{@pair0[1]};
			%copy{@pair0[1]} = $temp;
			$temp = %copy{@pair1[0]};
			%copy{@pair1[0]} = %copy{@pair1[1]};
			%copy{@pair1[1]} = $temp;
			
			my $z_value = calculate(%values.clone, %copy);
			say "$x_value +& $y_value = $z_value";
			if $z_value == $x_value +& $y_value {
				$result = @set4.sort().join(',');
				last;
			}
		}
		last if $result.chars > 0;
	}
	
	say "Part Two: the names of the swapped wires is $result";
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

sub calculate(%values, %structure --> Int) {
	my $unset_count = 1;
	while $unset_count > 0 {
		$unset_count = 0;
		for %structure.kv -> $name,$gate {
			next if %values{$name}:exists; # Already done
			if (%values{$gate.inputA}:exists) && (%values{$gate.inputB}:exists) {
				$gate.valueA = %values{$gate.inputA};
				$gate.valueB = %values{$gate.inputB};
				$gate.calculate_output();
				%values{$name} = $gate.output;
			}
			else { $unset_count++ }
		}
	}

	get_value(%values, 'z');
}

sub get_value(%values, Str $prefix --> Int) {
	my @keys = %values.keys.grep( -> $key { $key.starts-with($prefix) } ).sort().reverse();
	my $str = '0b';
	for @keys -> $k {
		$str ~= %values{$k};
	}
	
	return Int($str);
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
		#say self.debug_str;
	}

	method debug_str(--> Str) {
		"$.inputA [$.valueA] $.op $.inputB [$.valueB] is $.output";
	}
}
