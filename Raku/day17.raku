#!/usr/bin/env raku

use lib $*PROGRAM.dirname ~ '/lib';
use AOC::Util;
#use AOC::Geometry;
#use AOC::Grid;

my $INPUT_PATH = '../Input';
#my $INPUT_FILE = 'day17_test.txt';
my $INPUT_FILE = 'day17_challenge.txt';
my @input = read_grouped_input("$INPUT_PATH/$INPUT_FILE", 0);

say "Advent of Code 2024, Day 17: Chronospatial Computer";

class Triputer {...}

my %state = parse_input(@input);

solve_part_one(%state);
solve_part_two_fast(%state);

exit( 0 );

sub solve_part_one(%state) {
	my $puter = Triputer.new;
	$puter.load(%state);
	$puter.run;

	my $output = $puter.output.join(',');

	say "Part One: the output is $output";
}

sub solve_part_two(%state) {
	# Works for test, killed after 136750000 iterations on challenge
	my $GOAL = %state{'program'}.join(',');
	my $puter = Triputer.new;
	my $a = 0;
	while $puter.output.join(',') ne $GOAL {
		$a++;
		say $a if $a %% 10000;
		%state{'A'} = $a;
		$puter.load(%state);
		$puter.run;
	}

	say $puter.output.join(',');
	say $GOAL;

	say "Part Two: the output equalled input program when A was $a";	
}

#https://www.reddit.com/r/adventofcode/comments/1hg38ah/comment/m2hilky/
sub solve_part_two_fast(%state) {
	my @pgm = %state{'program'}.flat;
	#say @pgm.join(',');
	my $puter = Triputer.new;
	my $a = 0;

	for (0..@pgm.end-1) -> $i {
		#say "shift";
		my $test = @pgm[(@pgm.end-$i-1)..*].join(',');
		$a = $a +< 3;
		$a -= 8;
		while True {
			++$a;
			%state{'A'} = $a;
			$puter.load(%state);
			$puter.run;
			my $out = $puter.output.join(',');
			
			#say "$a: $out    $test";
			if $out eq $test {
				last;
			}
		}
		#say $a;
	}

	#say $a;
	#say $puter.output.join(',');

	say "Part Two: the output equalled input program when A was $a";
}

sub parse_input(@input --> Hash) {
	my $register_regex = /Register \s (\w)\: \s (\d+)/;
	my %result = ();
	for (0..2) -> $i {
		@input[$i] ~~ $register_regex;
		%result{Str($0)} = Int($1);
	}

	@input[3] ~~ /Program\: \s (.+)/;
	my @program = Str($0).split(',', :skip-empty).map( -> $x {Int($x) });
	%result{'program'} = @program;
	%result;
}

class Triputer {
	has Int $!a;
	has Int $!b;
	has Int $!c;
	has @!program;
	has $!ptr;
	has @.output;

	method load(%state) {
		$!a = %state{'A'};
		$!b = %state{'B'};
		$!c = %state{'C'};
		@!program = %state{'program'}.flat;
		$!ptr = 0;
		@!output = ();
	}

	method Str(--> Str) {
		"[$!ptr] A: $!a B: $!b C: $!c Program: " ~ @!program.join(',');
	}

	method run() {
		#say self.Str;
		while $!ptr < @!program.elems {
			my $instr = @!program[$!ptr];
			my $operand = @!program[$!ptr+1];
			my $ptr_move = 0;
			given $instr {
				when 0 { $ptr_move = self.adv($operand) }
				when 1 { $ptr_move = self.bxl($operand) }
				when 2 { $ptr_move = self.bst($operand) }
				when 3 { $ptr_move = self.jnz($operand) }
				when 4 { $ptr_move = self.bxc($operand) }
				when 5 { $ptr_move = self.out($operand) }
				when 6 { $ptr_move = self.bdv($operand) }
				when 7 { $ptr_move = self.cdv($operand) }
			}
			$!ptr += $ptr_move;
			#say self.Str;
		}
	}

	method combo_operand_value(Int $op --> Int) {
		given $op {
			when 0..3 { return $op }
			when 4    { return $!a }
			when 5    { return $!b }
			when 6    { return $!c }
			when 7    { die "combo operand 7 found" }
		}
	}

	method adv(Int $operand --> Int) {
		my $numerator = $!a;
		my $denominator = 2 ** self.combo_operand_value($operand);
		$!a = Int($numerator / $denominator);
		2;
	}

	method bxl(Int $operand --> Int) {
		$!b = $!b +^ $operand; # Bitwise XOR
		2;
	}

	method bst(Int $operand --> Int) {
		$!b = self.combo_operand_value($operand) % 8;
		2;
	}

	method jnz(Int $operand --> Int) {
		if $!a != 0 {
			$!ptr = $operand;
			return 0;
		}
		2;
	}

	method bxc(Int $operand --> Int) {
		$!b = $!b +^ $!c; # Operand is ignored
		2;
	}

	method out(Int $operand --> Int) {
		my $value = self.combo_operand_value($operand) % 8;
		@!output.push($value);
		2;
	}

	method bdv(Int $operand --> Int) {
		my $numerator = $!a;
		my $denominator = 2 ** self.combo_operand_value($operand);
		$!b = Int($numerator / $denominator);
		2;
	}

	method cdv(Int $operand --> Int) {
		my $numerator = $!a;
		my $denominator = 2 ** self.combo_operand_value($operand);
		$!c = Int($numerator / $denominator);
		2;
	}
}
