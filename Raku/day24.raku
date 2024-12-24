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

my %inputs = parse_inputs(@input[0]);
my %structure = parse_structure(@input[1]);

solve_part_one(%inputs.clone, %structure);
solve_part_two(%inputs.clone, %structure);

exit( 0 );

sub solve_part_one(%values, %structure) {
	my $z_value = calculate(%values, %structure);

	say "Part One: the value in the z outputs is $z_value";
}

# https://www.reddit.com/r/adventofcode/comments/1hl698z/comment/m3llouk/
sub solve_part_two(%values, %structure) {
	my @bad_gates = ();

	# z?? should be output of XOR, except $msb
	my @output_wires = %structure.keys;
	my @z_output_wires = @output_wires.grep( -> $o { $o.starts-with('z') }).sort();
	my $msb = @z_output_wires[*-1];
	for @z_output_wires -> $z {
		next if $z eq $msb;
		my $gate = %structure{$z};
		@bad_gates.push($z) if $gate.op ne 'XOR';
	}

	# all XOR should have x??, y?? for input, or z?? for output
	for %structure.kv -> $out, $gate {
		if $gate.op eq 'XOR' {
			my $are_inputs = $gate.has_xy_inputs;
			my $is_z_output = $out.starts-with('z');
			@bad_gates.push($out) if !$are_inputs && !$is_z_output;
		}
	}

	# input of OR should be always output of AND except for LSB
	my @inputs_of_or = ();
	my @outputs_of_and = ();
	for %structure.kv -> $out, $gate {
		@inputs_of_or.push($gate.inputA, $gate.inputB) if $gate.op eq 'OR';
		@outputs_of_and.push($out) if $gate.op eq 'AND' && !$gate.has_first_bit_inputs;
	}
	my $x = @inputs_of_or (^) @outputs_of_and;
	@bad_gates.push($x.keys.Slip);	
	my $result = @bad_gates.Set.keys.sort().join(',');

	say "Part Two: the names of the swapped wires are $result";
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

	multi infix:<eqv>(Gate $l, Gate $r --> Bool) {
		$l.inputA eq $r.inputA && $l.inputB eq $r.inputB && $l.op eq $r.op;
	} 
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

	method has_xy_inputs( --> Bool) {
		$.inputA.starts-with('x') && $.inputB.starts-with('y') ||
			$.inputB.starts-with('x') && $.inputA.starts-with('y');
	}

	method has_first_bit_inputs( --> Bool) {
		$.inputA.ends-with('00') && $.inputB.ends-with('00');
	}

	method debug_str(--> Str) {
		"$.inputA [$.valueA] $.op $.inputB [$.valueB] is $.output";
	}
}
