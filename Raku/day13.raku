#!/usr/bin/env raku

use lib $*PROGRAM.dirname ~ '/lib';
use AOC::Util;
use AOC::Geometry;
#use AOC::Grid;

my $INPUT_PATH = '../Input';
#my $INPUT_FILE = 'day13_test.txt';
my $INPUT_FILE = 'day13_challenge.txt';
my @input = read_grouped_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2024, Day 13: Claw Contraption";

class ClawMachine {...}
class Button {...}

my @machines = @input.map( -> @lines { ClawMachine.new(lines => @lines) });

solve_part_one(@machines);
#solve_part_two(@input);

exit( 0 );

class Button {
	has $.label: Str;
	has $.move: Coord;
	has $.cost: Int;

	method new(Str :$line --> Button) {
		$line ~~ /Button \s (\w)\: \s X\+(\d+)\, \s Y\+(\d+)/;
		my $lab = Str($0);
		my $move = Coord.new(x => Int($1), y => Int($2));
		my $cost = $lab eq 'A' ?? 3 !! 1;
		return self.bless(label => $lab, move => $move, cost => $cost);
	}
}

class ClawMachine {
	has $.a: Button;
	has $.b: Button;
	has $.prize_location: Coord;

	method new(:@lines --> ClawMachine) {
		my $button_a = Button.new(line => @lines[0]);
		my $button_b = Button.new(line => @lines[1]);
		@lines[2] ~~ /Prize\: \s X\=(\d+)\, \s Y\=(\d+)/;
		my $loc = Coord.new(x => Int($0), y => Int($1));

		return self.bless(a => $button_a, b => $button_b, prize_location => $loc);
	}

	method find_cheapest_play(--> Int) {
		#say "Aiming for " ~ $.prize_location;
		my $a_count = 0;
		my $b_count = 100;
		my $claw = self.claw_location($a_count, $b_count);
		while self.is_win($claw) == False && $b_count >= 0 && $a_count <= 100 {
			while self.is_win($claw) == False && self.is_overshoot($claw) && $b_count >= 0 {
				$b_count--;
				$claw = self.claw_location($a_count, $b_count);
			}
			while self.is_win($claw) == False && self.is_overshoot($claw) == False && $a_count <= 100 {
				$a_count++;
				$claw = self.claw_location($a_count, $b_count);
			}
		}

		if self.is_win($claw) && $a_count <= 100 && $b_count <= 100 {
			my $tokens = $a_count * $.a.cost + $b_count * $.b.cost;
			return $tokens;
		}
		-1;
	}

	method claw_location(Int $a, Int $b --> Coord) {
		my $x = $a * $.a.move.x + $b * $.b.move.x;
		my $y = $a * $.a.move.y + $b * $.b.move.y;
		my $loc = Coord.new(x => $x, y => $y);
		#say "Claw location a: $a b: $b is $loc";
		return $loc;
	}

	method is_overshoot(Coord $loc --> Bool) {
		return $loc.x > $.prize_location.x || $loc.y > $.prize_location.y;
	}

	method is_win(Coord $loc --> Bool) {
		return $loc eqv $.prize_location;
	}
}

sub solve_part_one(@machines) {
	my $cost = 0;
	for @machines -> $machine {
		#say $machine.prize_location;
		my $val = $machine.find_cheapest_play(); # Returns -1 for no win
		$cost += $val > 0 ?? $val !! 0;
	}

	say "Part One: total tokens spent is $cost";
}

sub solve_part_two(@input) {
	
}

