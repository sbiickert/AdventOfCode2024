#!/usr/bin/env raku

use lib $*PROGRAM.dirname ~ '/lib';
use AOC::Util;
use AOC::Geometry;
use AOC::Grid;

class Pad {...}
class DirPad {...}
class NumPad {...}

my $INPUT_PATH = '../Input';
my $INPUT_FILE = 'day21_test.txt';
#my $INPUT_FILE = 'day21_challenge.txt';
my @input = read_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2024, Day 21: Keypad Conundrum";

solve_part_one(@input);
#solve_part_two(@input);

exit( 0 );

sub solve_part_one(@input) {
	my $numpad = NumPad.new;
	my $dirpad = DirPad.new;
	my $sum = 0;
	#for @input -> $code {
	for ('379A') -> $code {
		say "Code: $code";
		my @numpad_moves = $numpad.enter_code($code);
		my $m = @numpad_moves.join('A') ~ 'A';
		say $m;
		my @dirpad_moves = $dirpad.enter_code($m);
		$m = @dirpad_moves.join('A') ~ 'A';
		say $m;
		@dirpad_moves = $dirpad.enter_code($m);
		$m = @dirpad_moves.join('A') ~ 'A';
		say $m;
		my $numeric = Int($code.substr(0..$code.chars-2));
		my $complexity = $numeric * $m.chars;
		say "$complexity is $numeric times " ~ $m.chars;
		$sum += $complexity;

		# Debugging
		say "Mine:";
		debug_moves($m);
		say "Eric's:";
		debug_moves("<v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A");
	}
	my %results = ();


	say "Part One: the sum of complexity is $sum";
}

sub solve_part_two(@input) {
	
}

sub debug_moves(Str $m) {
	my $numpad = NumPad.new;
	my $dirpad = DirPad.new;
	say $m;
	my $o = $dirpad.output($m);
	say $o;
	$o = $dirpad.output($o);
	say $o;
	$o = $numpad.output($o);
	say $o;
}

class Pad {
	has Grid $.pad;

	method move(Str $from, Str $to --> Str) {
		my $c_from = self.key_coord($from);
		my $c_to = self.key_coord($to);
		#my $c_empty = self.key_coord('X');
		my $dx = $c_to.x - $c_from.x;
		my $dy = $c_to.y - $c_from.y;

		my $y_first = (self ~~ NumPad && $dy < 0) || (self ~~ DirPad && $dy > 0);
		my $move = '';
		# Ordering of moving x first or y first avoids the null square
		if $y_first {
			# Go up first, then left or right
			for (1..abs($dy)) -> $y {
				$move ~= $dy < 0 ?? '^' !! 'v';
			}
			for (1..abs($dx)) -> $x {
				$move ~= $dx < 0 ?? '<' !! '>';
			}
		}
		else {
			# Go left/right first then down
			for (1..abs($dx)) -> $x {
				$move ~= $dx < 0 ?? '<' !! '>';
			}
			for (1..abs($dy)) -> $y {
				$move ~= $dy < 0 ?? '^' !! 'v';
			}
		}
		say "$from to $to: $move";
		$move;
	}

	method key_coord(Str $key --> Coord) {
		return $.pad.coords($key).first;
	}

	method enter_code(Str $code --> Array) {
		self.print;
		my @chars = $code.split('', :skip-empty);
		@chars.unshift('A');
		say @chars.join(',');
		my @moves = ();
		for (1..$code.chars) -> $i {
			@moves.push(self.move(@chars[$i-1], @chars[$i]));
		}
		@moves;
	}

	method output(Str $code --> Str) {
		my $pos = self.key_coord('A');
		my @chars = $code.split('', :skip-empty);
		my $result = '';
		for @chars -> $ch {
			if $ch eq 'A' {
				$result ~= $.pad.get($pos);
			}
			else {
				$pos = $pos.offset($ch);
			}
		}
		$result;
	}

	method print() {
		$.pad.print;
	}
}

class DirPad is Pad {
	method new(--> DirPad) {
		my $pad = Grid.new(default => 'X', rule => AdjacencyRule::ROOK);
		my @layout = ('X^A', '<v>');
		$pad.load(@layout);
		return self.bless(pad => $pad);
	}
}

class NumPad is Pad {
	method new(--> NumPad) {
		my $pad = Grid.new(default => 'X', rule => AdjacencyRule::ROOK);
		my @layout = ('789', '456', '123', 'X0A');
		$pad.load(@layout);
		return self.bless(pad => $pad);
	}
}
