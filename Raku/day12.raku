#!/usr/bin/env raku

use lib $*PROGRAM.dirname ~ '/lib';
use AOC::Util;
use AOC::Geometry;
use AOC::Grid;

my $INPUT_PATH = '../Input';
#my $INPUT_FILE = 'day12_test.txt';
my $INPUT_FILE = 'day12_challenge.txt';
my @input = read_grouped_input("$INPUT_PATH/$INPUT_FILE", 0);

say "Advent of Code 2024, Day 12: Garden Groups";

my $map = Grid.new(default => '.', rule => AdjacencyRule::ROOK);
$map.load(@input);

$map.print();

solve_part_one($map);
#solve_part_two(@input);

exit( 0 );

sub solve_part_one(Grid $map) {
	my %hist = $map.histogram();

	my $sum = 0;
	for %hist.kv -> $letter, $count {
		say $letter;
		my %counted = ();
		my @coords_with_letter = $map.coords($letter);
		while @coords_with_letter.elems > 0 {
			my $c = @coords_with_letter.pop();
			next if %counted{$c.Str}:exists;
			my %region = ($c.Str => $c);
			find_region($map, $c, %region);
			my $perimeter = 0;
			for %region.kv -> $key, $coord {
				my @neighbors = $map.neighbors($coord).grep(-> $n {$map.get($n) eq $letter});
				my $p = 4 - @neighbors.elems;
				say $coord ~ " $p";
				$perimeter += $p;
				%counted{$coord.Str} = 1;
			}
			say $perimeter * %region.elems;
			$sum += $perimeter * %region.elems;
		}
	}

	say "Part One: the total price is $sum";
}

sub solve_part_two(@input) {
	
}

sub find_region(Grid $map, Coord $c, %region) {
	my $letter = $map.get($c);
	my @neighbors = $map.neighbors($c).grep(-> $n {$map.get($n) eq $letter});
	for @neighbors -> $n {
		if !(%region{$n.Str}:exists) {
			%region{$n.Str} = $n;
			find_region($map, $n, %region);
		}
	}
}
