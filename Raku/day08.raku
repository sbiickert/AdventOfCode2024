#!/usr/bin/env raku

use lib $*PROGRAM.dirname ~ '/lib';
use AOC::Util;
use AOC::Geometry;
use AOC::Grid;

my $INPUT_PATH = '../Input';
#my $INPUT_FILE = 'day08_test.txt';
my $INPUT_FILE = 'day08_challenge.txt';
my @input = read_grouped_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2024, Day 8: Resonant Collinearity";

# Non-. default value b/c I want the extent to be preserved
my $map = Grid.new(default => ',', rule => AdjacencyRule::QUEEN);
$map.load(@input[0]);

#$map.print();

solve_part_one($map);
solve_part_two($map);

exit( 0 );

sub solve_part_one($map) {
	my $antinode_map = create_antinode_map($map, False);
	#$antinode_map.print();
	my $antinode_count = $antinode_map.coords.elems;

	say "Part One: the number of antinodes is $antinode_count";
}

sub solve_part_two($map) {
	my $antinode_map = create_antinode_map($map, True);
	#$antinode_map.print();
	my $antinode_count = $antinode_map.coords.elems;

	say "Part Two: the number of antinodes is $antinode_count";	
}

sub create_antinode_map(Grid $map, Bool $is_part_two --> Grid) {
	my $antinode_map = Grid.new(default => ',', rule => AdjacencyRule::QUEEN);
	my %hist = $map.histogram;
	%hist{'.'}:delete; # ignore the "empty" spaces
	for %hist.keys -> $key {
		my @locations = $map.coords($key);
		for 0..@locations.elems-2 -> $i {
			for $i+1..@locations.elems-1 -> $j {
				# calculate the delta between coords
				my $c0 = @locations[$i];
				my $c1 = @locations[$j];
				my $d0 = $c0.delta($c1);
				my $d1 = $c1.delta($c0);
				# for part two, the signal repeat starts at the node
				$antinode_map.set($c0, '#') if $is_part_two;
				$antinode_map.set($c1, '#') if $is_part_two;
				# mark antinode locations
				my $a0 = $c0.add($d1);
				my $a1 = $c1.add($d0);
				while $map.extent.contains($a0) {
					$antinode_map.set($a0, '#');
					$a0 = $a0.add($d1);
					last if !$is_part_two;
				}
				while $map.extent.contains($a1) {
					$antinode_map.set($a1, '#');
					$a1 = $a1.add($d0);
					last if !$is_part_two;
				}
			}
		}
	}
	$antinode_map;
}
