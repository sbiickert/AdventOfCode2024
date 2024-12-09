#!/usr/bin/env raku

use lib $*PROGRAM.dirname ~ '/lib';
use AOC::Util;
#use AOC::Geometry;
#use AOC::Grid;

my $INPUT_PATH = '../Input';
#my $INPUT_FILE = 'day09_test.txt';
my $INPUT_FILE = 'day09_challenge.txt';
my @input = read_grouped_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2024, Day 9: Disk Fragmenter";

my @disk_map = create_disk_map(@input[0].first);

solve_part_one(@disk_map);
#solve_part_two(@input);

exit( 0 );

sub solve_part_one(@map) {
	my $a = 0;
	my $b = @map.elems-1;
	while True {
		while @map[$a] ne '.' { $a++ }
		while @map[$b] eq '.' { $b-- }
		last if $a > $b;
		@map[$a] = @map[$b];
		@map[$b] = '.';
		#say @map.join('');
	}

	my $checksum = 0;
	for 0..@map.elems-1 -> $i {
		last if @map[$i] eq '.';
		$checksum += $i * @map[$i];
	}

	say "Part One: the checksum is $checksum";
}

sub solve_part_two(@input) {
	
}

sub create_disk_map($str --> Array) {
	my @chars = $str.split('', :skip-empty);
	my $is_file = True;
	my @map = ();
	my $id = 0;
	for @chars -> $c {
		my $append_char = '.';
		if $is_file {
			$append_char = $id;
			$id++;
		}
		for 1..$c -> $i {
			@map.push($append_char);
		}
		#dd @map;
		$is_file = !$is_file;
	}
	say "Created map.";
	@map;
}
