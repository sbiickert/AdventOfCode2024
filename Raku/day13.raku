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

my $cost1 = solve_part(@machines, 100);
say "Part One: total tokens spent is $cost1";

my $p2_shift = 10000000000000;
my $offset = Coord.new(x => $p2_shift, y => $p2_shift);
for @machines -> $machine {
	$machine.prize_location = $machine.prize_location.add($offset);
}

my $cost2 = solve_part(@machines, $p2_shift); # Arbitrary number larger than the press count
say "Part Two: total tokens spent is $cost2";	

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
	has $.prize_location: Coord is rw;

	method new(:@lines --> ClawMachine) {
		my $button_a = Button.new(line => @lines[0]);
		my $button_b = Button.new(line => @lines[1]);
		@lines[2] ~~ /Prize\: \s X\=(\d+)\, \s Y\=(\d+)/;
		my $loc = Coord.new(x => Int($0), y => Int($1));

		return self.bless(a => $button_a, b => $button_b, prize_location => $loc);
	}

	method calc_cheapest_play(Int $press_limit --> Int) {
		#say "Aiming for " ~ $.prize_location;
		#say "A move: " ~ $.a.move ~ " B move: " ~ $.b.move;
		my $r_a = $.a.move.y / $.a.move.x; # Slope of A button presses
		my $r_b = $.b.move.y / $.b.move.x; # Slope of B button presses
		# A line intercepting prize
		my $y_intercept_a = $.prize_location.y - ($r_a * $.prize_location.x); 
		my $x = $y_intercept_a / ($r_b - $r_a); # B line crosses A line at X

		my $b_count = Int($x / $.b.move.x);
		my $a_count = Int(($.prize_location.x - $x) / $.a.move.x);

		# If there is no combination of A and B to get prize,
		# is_win($claw) will be false
		my $claw = self.claw_location($a_count, $b_count);
		
		#say "Calculated: $a_count, $b_count -> $claw";
		if self.is_win($claw) && $a_count <= $press_limit && $b_count <= $press_limit {
			my $tokens = $a_count * $.a.cost + $b_count * $.b.cost;
			return $tokens;
		}
		#say "No win.";
		-1;
	}

	method claw_location(Int $a, Int $b --> Coord) {
		my $x = $a * $.a.move.x + $b * $.b.move.x;
		my $y = $a * $.a.move.y + $b * $.b.move.y;
		my $loc = Coord.new(x => $x, y => $y);
		#say "Claw location a: $a b: $b is $loc";
		return $loc;
	}

	method is_win(Coord $loc --> Bool) {
		return $loc eqv $.prize_location;
	}
}

sub solve_part(@machines, Int $press_limit) {
	my $cost = 0;
	for @machines -> $machine {
		my $calc = $machine.calc_cheapest_play($press_limit);
		$cost += $calc > 0 ?? $calc !! 0;
	}
	$cost;
}
