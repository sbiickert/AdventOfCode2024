#!/usr/bin/env raku

use lib $*PROGRAM.dirname ~ '/lib';
use AOC::Util;
use AOC::Geometry;
use AOC::Grid;

my $INPUT_PATH = '../Input';
my $INPUT_FILE = 'day16_test.txt';
#my $INPUT_FILE = 'day16_challenge.txt';
my @input = read_grouped_input("$INPUT_PATH/$INPUT_FILE", 0);

say "Advent of Code 2024, Day 16: Reindeer Maze";

class Path {...}
class CostTracker {...}

my $maze = Grid.new(default => 'x', rule => AdjacencyRule::ROOK);
$maze.load(@input).first;

my $start = $maze.coords('S')[0];
my $end = $maze.coords('E')[0];
$maze.set($start, CostTracker.new);
$maze.set($end, CostTracker.new);
for $maze.coords('.') -> $c {
	$maze.set($c, CostTracker.new);
}

solve_parts($maze);

exit( 0 );

sub solve_parts(Grid $maze) {
	my $start_pos = Position.new(coord => $start, dir => 'E');
	my $path = Path.new(p => $start_pos, steps => [$start_pos]);
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
			my $tracker = $maze.get($p.pos.coord);
			my $cw = $p.turn('CW');
			@next.push($cw) if $tracker.enter($cw);
			my $ccw = $p.turn('CCW');
			@next.push($ccw) if $tracker.enter($ccw);
			#my $rev = $cw.turn('CW');
			#@next.push($rev) if $tracker.enter($rev);
		}
		say @next.elems;
	}
	
	my $end_tracker = $maze.get($end);
	my $lc_path = $end_tracker.get_lowest_cost_path;

	say "Part One: the lowest cost path has a score of " ~ $lc_path.score;

	my @tiles = $lc_path.unique_tiles;
	for @tiles -> $t {
		$maze.set($t, 'X');
	}
	#$maze.print;
	say "Part Two: there are " ~ @tiles.elems ~ " tiles on best paths";
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
		my @new_steps = @.steps;
		@new_steps.push($new_pos);
		return Path.new(p => $new_pos, score => $.score + 1, steps => @new_steps);
	}

	method turn(Str $dir --> Path) {
		my $new_pos = $.pos.turn($dir);
		my @new_steps = @.steps;
		@new_steps.push($new_pos);
		return Path.new(p => $new_pos, score => $.score + 1000, steps => @new_steps);		
	}

	method merge(Path $other --> Path) {
		my %uni = Hash.new;
		for @.steps -> $s {
			%uni{$s.Str} = $s if $s.Str ne '';
		}
		for $other.steps -> $s {
			%uni{$s.Str} = $s if $s.Str ne '';
		}
		
		my @new_steps = %uni.values;
		return Path.new(p => $.pos, score => $.score, steps => @new_steps);
	}

	method unique_tiles(--> Array) {
		my %uni = Hash.new;
		for @.steps -> $s {
			%uni{$s.coord.Str} = $s.coord;
		}
		
		my @tiles = %uni.values;
		@tiles;
	}
}

class CostTracker does GridGlyph {
	has %.paths is rw;

	method new(--> CostTracker) {
		self.bless(paths => Hash.new);
	}

	method glyph(--> Str) {
		#%.paths.elems.Str;
		'.'
	}

	method enter(Path $p --> Bool) {
		my $current_score = self.cost_for(dir => $p.pos.dir);
		if $current_score >= 0 && $current_score <= $p.score {
			if $current_score < $p.score { return False }
			#merge paths
			if $current_score == $p.score {
				my $merged = $p.merge(self.paths{$p.pos.dir});
#				if $p.pos.coord eqv Coord.from_ints(13,1) {
#					say "Merge: " ~ %.paths{$p.pos.dir};
#					say "With: " ~ $p;
#					say "Result: " ~ $merged;
#				}
				%.paths{$p.pos.dir} = $merged;
			}
		}
		else {
			%.paths{$p.pos.dir} = $p;
		}
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
