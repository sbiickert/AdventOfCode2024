#!/usr/bin/env raku

use lib $*PROGRAM.dirname ~ '/lib';
use AOC::Util;
use AOC::Geometry;
use AOC::Grid;

my $INPUT_PATH = '../Input';
#my $INPUT_FILE = 'day14_test.txt';
my $INPUT_FILE = 'day14_challenge.txt';
my @input = read_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2024, Day 14: Restroom Redoubt";

class Robot {...}

my @robots = @input.map( -> $line { Robot.new(src => $line) });
my $space = Extent.from_coords( [Coord.origin, Coord.new(x => 10, y => 6)] );
if @robots.elems > 12 {
	$space = Extent.from_coords( [Coord.origin, Coord.new(x => 100, y => 102)] );
}

#draw_space(@robots, $space);

solve_part_one(@robots, $space);
solve_part_two(@robots, $space);

exit( 0 );

sub solve_part_one(@robots, Extent $space) {
	for (1..100) -> $iter {
		for @robots -> $robot {
			$robot.move_in($space);
		}
	}
	say '';
#	draw_space(@robots, $space);
	my @quad_counts = count_quadrants(@robots, $space);
#	dd @quad_counts;
	my $product = @quad_counts.reduce(&infix:<*>);

	say "Part One: the product of quadrants is $product";
}

sub solve_part_two(@robots, Extent $space) {
	for (101..10000) -> $iter {
		for @robots -> $robot {
			$robot.move_in($space);
		}
		say $iter;
		draw_space(@robots, $space);
	}
}

sub draw_space(@robots, Extent $space) {
	my $map = Grid.new(default => ' ', rule => AdjacencyRule::ROOK);
	for $space.all_coords -> $c { $map.set($c, '.') }
	for @robots -> $r {
		my $val = $map.get($r.pos);
		if $val eq '.' {
			$map.set($r.pos, 1);
		}
		else {
			$map.set($r.pos, $val + 1);
		}
	}
	$map.print;
}

sub count_quadrants(@robots, Extent $space --> Array) {
	my $mid_x = Int($space.max.x / 2);
	my $mid_y = Int($space.max.y / 2);
	my @quads = (
		Extent.from_coords( [Coord.origin, Coord.new(x => $mid_x-1, y => $mid_y-1)] ),
		Extent.from_coords( [Coord.new(x => $mid_x+1, y => 0), Coord.new(x => $space.max.x, y => $mid_y-1)] ),
		Extent.from_coords( [Coord.new(x => 0, y => $mid_y+1), Coord.new(x => $mid_x-1, y => $space.max.y)] ),
		Extent.from_coords( [Coord.new(x => $mid_x+1, y => $mid_y+1), Coord.new(x => $space.max.x, y => $space.max.y)] )
	);
	my @counts = (0,0,0,0);
	for @robots -> $r {
		for (0..3) -> $q {
			@counts[$q]++ if @quads[$q].contains($r.pos);
		}
	}
	@counts;
}

class Robot {
	has $.pos: Coord is rw;
	has $.vel: Coord;

	method new(Str :$src --> Robot) {
		$src ~~ /p \= (\d+)\,(\d+) \s v \= (\-? \d+)\,(\-? \d+)/;
		my $pos = Coord.new(x => Int($0), y => Int($1));
		my $vel = Coord.new(x => Int($2), y => Int($3));
		return self.bless(pos => $pos, vel => $vel);
	}

	method Str(--> Str) {
		return "P:$.pos V:$.vel";
	}

	method move_in(Extent $ext) {
		my $x = ($.pos.x + $.vel.x) % $ext.width;
		my $y = ($.pos.y + $.vel.y) % $ext.height;
		$.pos = Coord.new(x => $x, y => $y);
	}
}
