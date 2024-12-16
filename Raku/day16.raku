#!/usr/bin/env raku

use lib $*PROGRAM.dirname ~ '/lib';
use AOC::Util;
use AOC::Geometry;
use AOC::Grid;

my $INPUT_PATH = '../Input';
#my $INPUT_FILE = 'day16_test.txt';
my $INPUT_FILE = 'day16_challenge.txt';
my @input = read_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2024, Day 16: Reindeer Maze";

class Path {...}
class CostTracker {...}

my $maze = Grid.new(default => 'x', rule => AdjacencyRule::ROOK);
$maze.load(@input);

my $start = $maze.coords('S')[0];
my $end = $maze.coords('E')[0];
$maze.set($start, CostTracker.new);
$maze.set($end, CostTracker.new);
for $maze.coords('.') -> $c {
	$maze.set($c, CostTracker.new);
}

solve_part_one($maze);
#solve_part_two(@input);

exit( 0 );

sub solve_part_one(Grid $maze) {
	my $start_pos = Position.new(coord => $start, dir => 'E');
	my $path = Path.new(p => $start_pos);
	$maze.get($start).enter($path);

	my @next = ($path);
	while @next.elems > 0 {
		my @paths = @next;
		@next = ();
		for @paths -> $p {
			my $offset = $p.pos.coord.offset($p.pos.dir);
			if $maze.get_glyph($offset) ne '#' {
				my $fwd = $p.move_forward;
				@next.push($fwd) if $maze.get($offset).enter($fwd);
			}
			# Turns
			my $cw = $p.turn('CW');
			my $ccw = $p.turn('CCW');
			my $rev = $cw.turn('CW');
			my $tracker = $maze.get($p.pos.coord);
			@next.push($cw) if $tracker.enter($cw);
			@next.push($ccw) if $tracker.enter($ccw);
			@next.push($rev) if $tracker.enter($rev);
		}
		say @next.elems;
	}
	
	my $end_tracker = $maze.get($end);
	my $lc_path = $end_tracker.get_lowest_cost_path;

	say "Part One: the lowest cost path has a score of " ~ $lc_path.score;
}


sub solve_part_two(@input) {
	
}

class Path does GridGlyph {
	has $.score: Int;
	has $.pos: Position;
	has @.steps;

	method new(Position :$p, Int :$score = 0, :@steps = Array.new --> Path) {
		return self.bless(score => $score, pos => $p, steps => @steps);
	}
	
	method Str { $.score ~ ": " ~ $.pos ~ " " ~ $.steps.elems }
	method glyph(--> Str) { $.pos.dir }

	method move_forward(--> Path) {
		my $new_pos = $.pos.move_forward;
		my @new_steps = $.steps;
		@new_steps.push($new_pos);
		return Path.new(p => $new_pos, score => $.score + 1, steps => @new_steps);
	}

	method turn(Str $dir --> Path) {
		my $new_pos = $.pos.turn($dir);
		my @new_steps = $.steps;
		@new_steps.push($new_pos);
		return Path.new(p => $new_pos, score => $.score + 1000, steps => @new_steps);		
	}
}

class CostTracker does GridGlyph {
	has %.paths is rw;

	method new(--> CostTracker) {
		self.bless(paths => Hash.new);
	}

	method glyph(--> Str) {
		%.paths.elems.Str;
	}

	method enter(Path $p --> Bool) {
		my $current_score = self.cost_for(dir => $p.pos.dir);
		if $current_score > 0 && $current_score <= $p.score { return False }
		%.paths{$p.pos.dir} = $p;
		True;
	}

	method cost_for(Str :$dir --> Int) {
		if %.paths{$dir}:exists {
			return %.paths{$dir}.score;
		}
		-1;
	}

	method get_lowest_cost_path(--> Path) {
		my $lowest_cost = 10000000000000;
		my $lc_path;
		for %.paths.kv -> $dir, $path {
			if $path.score < $lowest_cost {
				$lowest_cost = $path.score;
				$lc_path = $path;
			}
		}
		$lc_path;
	}
}
