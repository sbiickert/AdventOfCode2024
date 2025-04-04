#!/usr/bin/env raku

use lib $*PROGRAM.dirname ~ '/lib';
use AOC::Util;
use AOC::Geometry;
use AOC::Grid;

class Pad {...}
class DirPad {...}
class NumPad {...}

my $INPUT_PATH = '../Input';
#my $INPUT_FILE = 'day21_test.txt';
my $INPUT_FILE = 'day21_challenge.txt';
my @input = read_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2024, Day 21: Keypad Conundrum";

my $pad = Pad.new;

my $complexity1 = solve_part(@input, 3);
say "Part One: the sum of complexity is $complexity1";
my $complexity2 = solve_part(@input, 26);
say "Part Two: the sum of complexity is $complexity2"; # 118152621365344 too low

exit( 0 );

sub solve_part(@input, $n_robots --> Int) {
	my $sum = 0;
	for @input -> $code {
		#say "Code: $code";
		$sum += calc_score($code, $n_robots);
	}
	$sum;
}

sub calc_score(Str $code, Int $robots --> Int) {
	my $numeric = Int($code.substr(0..$code.chars-2));
	my $length = calc_length($code, $robots);  
	$numeric * $length;
}

# https://www.reddit.com/r/adventofcode/comments/1hj2odw/comment/m36j01x/
my %cache = ();
sub calc_length(Str $seq, Int $depth --> Int) {
	my $cache_key = "$seq,$depth";
	return %cache{$cache_key} if %cache{$cache_key}:exists;

	my $length = 0;
	if $depth == 0 {
		$length = $seq.chars;
	}
	else {
		my $current = 'A';
		my @chars = $seq.split('', :skip-empty);
		for @chars -> $next {
			my $len = get_move_count($current, $next, $depth);
			$current = $next;
			$length += $len;
		}
	}

	%cache{$cache_key} = $length;
	$length;
}

sub get_move_count(Str $current, Str $next, Int $depth --> Int) {
	return 1 if $current eq $next;
	my $new_seq = $pad.paths{"$current,$next"};
	return calc_length($new_seq, $depth-1);
}

class Pad {
	has Grid $.pad;
	has %.paths;

	submethod TWEAK() {
		%!paths = (
			'<,^' => ">^A",
			'^,<' => "v<A",
			'<,v' => ">A",
			'v,<' => "<A",
			'<,>' => ">>A",
			'>,<' => "<<A",
			'<,A' => ">>^A",
			'A,<' => "v<<A",
			'^,v' => "vA",
			'v,^' => "^A",
			'^,>' => "v>A",
			'>,^' => "<^A",
			'^,A' => ">A",
			'A,^' => "<A",
			'v,>' => ">A",
			'>,v' => "<A",
			'v,A' => "^>A",
			'A,v' => "<vA",
			'>,A' => "^A",
			'A,>' => "vA",
			'A,0' => "<A",
			'0,A' => ">A",
			'A,1' => "^<<A",
			'1,A' => ">>vA",
			'A,2' => "<^A",
			'2,A' => "v>A",
			'A,3' => "^A",
			'3,A' => "vA",
			'A,4' => "^^<<A",
			'4,A' => ">>vvA",
			'A,5' => "<^^A",
			'5,A' => "vv>A",
			'A,6' => "^^A",
			'6,A' => "vvA",
			'A,7' => "^^^<<A",
			'7,A' => ">>vvvA",
			'A,8' => "<^^^A",
			'8,A' => "vvv>A",
			'A,9' => "^^^A",
			'9,A' => "vvvA",
			'0,1' => "^<A",
			'1,0' => ">vA",
			'0,2' => "^A",
			'2,0' => "vA",
			'0,3' => "^>A",
			'3,0' => "<vA",
			'0,4' => "^<^A",
			'4,0' => ">vvA",
			'0,5' => "^^A",
			'5,0' => "vvA",
			'0,6' => "^^>A",
			'6,0' => "<vvA",
			'0,7' => "^^^<A",
			'7,0' => ">vvvA",
			'0,8' => "^^^A",
			'8,0' => "vvvA",
			'0,9' => "^^^>A",
			'9,0' => "<vvvA",
			'1,2' => ">A",
			'2,1' => "<A",
			'1,3' => ">>A",
			'3,1' => "<<A",
			'1,4' => "^A",
			'4,1' => "vA",
			'1,5' => "^>A",
			'5,1' => "<vA",
			'1,6' => "^>>A",
			'6,1' => "<<vA",
			'1,7' => "^^A",
			'7,1' => "vvA",
			'1,8' => "^^>A",
			'8,1' => "<vvA",
			'1,9' => "^^>>A",
			'9,1' => "<<vvA",
			'2,3' => ">A",
			'3,2' => "<A",
			'2,4' => "<^A",
			'4,2' => "v>A",
			'2,5' => "^A",
			'5,2' => "vA",
			'2,6' => "^>A",
			'6,2' => "<vA",
			'2,7' => "<^^A",
			'7,2' => "vv>A",
			'2,8' => "^^A",
			'8,2' => "vvA",
			'2,9' => "^^>A",
			'9,2' => "<vvA",
			'3,4' => "<<^A",
			'4,3' => "v>>A",
			'3,5' => "<^A",
			'5,3' => "v>A",
			'3,6' => "^A",
			'6,3' => "vA",
			'3,7' => "<<^^A",
			'7,3' => "vv>>A",
			'3,8' => "<^^A",
			'8,3' => "vv>A",
			'3,9' => "^^A",
			'9,3' => "vvA",
			'4,5' => ">A",
			'5,4' => "<A",
			'4,6' => ">>A",
			'6,4' => "<<A",
			'4,7' => "^A",
			'7,4' => "vA",
			'4,8' => "^>A",
			'8,4' => "<vA",
			'4,9' => "^>>A",
			'9,4' => "<<vA",
			'5,6' => ">A",
			'6,5' => "<A",
			'5,7' => "<^A",
			'7,5' => "v>A",
			'5,8' => "^A",
			'8,5' => "vA",
			'5,9' => "^>A",
			'9,5' => "<vA",
			'6,7' => "<<^A",
			'7,6' => "v>>A",
			'6,8' => "<^A",
			'8,6' => "v>A",
			'6,9' => "^A",
			'9,6' => "vA",
			'7,8' => ">A",
			'8,7' => "<A",
			'7,9' => ">>A",
			'9,7' => "<<A",
			'8,9' => ">A",
			'9,8' => "<A");
	}
	
	method move(Str $from, Str $to --> Str) {
		return 'A' if $from eq $to;
		return %.paths{"$from,$to"};
	}

	method key_coord(Str $key --> Coord) {
		return $.pad.coords($key).first;
	}

	method enter_code(Str $code --> Array) {
		my @chars = $code.split('', :skip-empty);
		@chars.unshift('A');
		#say @chars.join(',');
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
