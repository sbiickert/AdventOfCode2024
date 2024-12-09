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

solve_part_one(@input[0].first);
solve_part_two(@input[0].first);

exit( 0 );

sub solve_part_one($input) {
	my @map = create_disk_map_from_str($input);
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

	my $checksum = calc_checksum(@map);
	say "Part One: the checksum is $checksum";
}

sub solve_part_two($input) {
	my @map = create_file_map($input);
	my $b = @map.elems-1;
	my $index_of_first_gap = 1;
	while True {
		last if $b < $index_of_first_gap;
		my $a = $index_of_first_gap;
		while @map[$b]{'id'} eq '.' { $b-- }
		while (@map[$a]{'id'} ne '.' ||
			@map[$a]{'size'} < @map[$b]{'size'}) &&
			$a < $b { $a++ }
		if $a >= $b {
			# No place to move file
			$b--;
			next;
		}
			
		my %temp = @map[$a];
		@map[$a] = @map[$b];
		@map[$b] = %temp;
		my $size_diff = @map[$b]{'size'} - @map[$a]{'size'};
		if $size_diff > 0 {
			my %gap = (id => '.', size => $size_diff);
			@map.splice($a+1, 0, %gap);
			$b++;
			@map[$b]{'size'} = @map[$b]{'size'} - $size_diff;
		}
		
		while @map[$index_of_first_gap]{'id'} ne '.' { $index_of_first_gap++ }

		$b--;
	}
	
	my @disk_map = create_disk_map_from_fm(@map);
	my $checksum = calc_checksum(@disk_map);
	say "Part One: the checksum is $checksum";	
}

multi sub create_disk_map_from_str(Str $str --> Array) {
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
		$is_file = !$is_file;
	}
	@map;
}

multi sub create_disk_map_from_fm(@file_map --> Array) {
	# Create disk map from file map
	my @map = ();
	for @file_map -> %file {
		for 1..%file{'size'} {
			@map.push(%file{'id'});
		}
	}
	@map;
}

sub create_file_map($str --> Array) {
	my @chars = $str.split('', :skip-empty);
	my $is_file = True;
	my @map = ();
	my $id = 0;
	for @chars -> $c {
		my %file = (id => '.', size => Int($c));
		if $is_file {
			%file = (id => $id, size => Int($c));
			$id++;
		}
		@map.push(%file) if %file{'size'} > 0; # Avoid zero-size files/gaps
		$is_file = !$is_file;
	}	
	@map;
}

sub calc_checksum(@map --> Int) {
	my $checksum = 0;
	for 0..@map.elems-1 -> $i {
		next if @map[$i] eq '.';
		$checksum += $i * @map[$i];
	}
	$checksum;
}
