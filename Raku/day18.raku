#!/usr/bin/env raku

use lib $*PROGRAM.dirname ~ '/lib';
use AOC::Util;
use AOC::Geometry;
use AOC::Grid;

my $INPUT_PATH = '../Input';
#my $INPUT_FILE = 'day18_test.txt';
my $INPUT_FILE = 'day18_challenge.txt';
my @input = read_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2024, Day 18: RAM Run";

my @bytes = parse_falling_bytes(@input);
#say @bytes.map(-> $b { $b.Str }).join("\n");

my $byte_count = @input.elems <= 25 ?? 12 !! 1024;
my $size = @input.elems <= 25 ?? 6 !! 70;

solve_part_one(@bytes, $byte_count, $size);
#solve_part_two(@input);

exit( 0 );

sub solve_part_one(@danger, Int $byte_count, Int $grid_size) {
	my $ext = Extent.from_ints(0,0,$grid_size,$grid_size);
	my $grid = Grid.new(default => '#', rule => AdjacencyRule::ROOK);
	for $ext.all_coords -> $coord { $grid.set($coord, '.') }
	for (0..$byte_count-1) -> $i {
		$grid.clear(@danger[$i]);
	}

#	$grid.print;

	my $start = Coord.from_ints(0,0);
	my $end = Coord.from_ints($grid_size, $grid_size);

	my $step = 0;
	my @coords = ($start);
	my $at_end = False;
	while @coords.elems > 0 {
		my %next = ();
		for @coords -> $coord {
			$grid.set($coord, "O");
			if $coord eqv $end {
				$at_end = True;
				last;
			}
			my @unvisited = $grid.neighbors($coord).grep(-> $n {$grid.get($n) eq '.'} );
			for @unvisited -> $u {
				%next{$u.Str} = $u;
			}
		}
		
		last if $at_end;

		$step++;
		@coords = %next.values;
		say @coords.elems;
		$grid.print;
	}

	say "Part One: the min steps to reach end is $step";
}

sub solve_part_two(@input) {
	
}

sub parse_falling_bytes(@input --> Array) {
	my @coords = ();
	for @input -> $line {
		$line ~~ /(\d+) \, (\d+)/;
		my $coord = Coord.from_ints(Int($0), Int($1));
		@coords.push($coord);
	}
	@coords;
}
