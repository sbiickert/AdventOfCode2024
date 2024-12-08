#!/usr/bin/env raku

use lib $*PROGRAM.dirname ~ '/lib';
use AOC::Util;
use AOC::Geometry;
use AOC::Grid;

my $INPUT_PATH = '../Input';
#my $INPUT_FILE = 'day08_test.txt';
my $INPUT_FILE = 'day08_challenge.txt';
my @input = read_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2024, Day 8: Resonant Collinearity";

my $map = Grid.new(default => ',', rule => AdjacencyRule::QUEEN);
$map.load(@input);

#$map.print();

solve_part_one($map);
#solve_part_two(@input);

exit( 0 );

sub solve_part_one($map) {
	my $antinode_map = Grid.new(default => ',', rule => AdjacencyRule::QUEEN);
	my %hist = $map.histogram;
	%hist{'.'}:delete;
	for %hist.keys -> $key {
		say $key;
		my @locations = $map.coords($key);
		for 0..@locations.elems-2 -> $i {
			for $i+1..@locations.elems-1 -> $j {
				my $c0 = @locations[$i];
				my $c1 = @locations[$j];
				my $a0 = $c0.delta($c1).add($c1);
				my $a1 = $c1.delta($c0).add($c0);
				$antinode_map.set($a0, '#') if $map.extent.contains($a0);
				$antinode_map.set($a1, '#') if $map.extent.contains($a1);
			}
		}
	}

	my $antinode_count = $antinode_map.coords.elems;

	say "Part One: the number of antinodes is $antinode_count";
}

sub solve_part_two(@input) {
	
}
