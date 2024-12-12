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

my @regions = solve_part_one($map);
solve_part_two($map, @regions);

exit( 0 );

sub solve_part_one(Grid $map --> Array) {
	my %hist = $map.histogram();

	my $sum = 0;
	my @all_regions = ();
	for %hist.kv -> $letter, $count {
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
				$perimeter += $p;
				%counted{$coord.Str} = 1;
			}
			$sum += $perimeter * %region.elems;
			@all_regions.push(%region);
		}
	}

	say "Part One: the total price is $sum";
	@all_regions;
}

sub solve_part_two(Grid $map, @regions) {
	my $sum = 0;
	for @regions -> %region {
		my $edge_count = count_edges($map, %region);
		$sum += $edge_count * %region.elems;
	}
	say "Part Two: the total price is $sum";
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

sub count_edges(Grid $map, %region --> Int) {
	my @segments = ();
	my $value;
	
	for %region.kv -> $key, $coord {
		$value = $map.get($coord);
		
		my $n_val = $map.get($coord.offset('N'));
		if $n_val ne $value {
			my $seg = Segment.new(from => $coord, 
								  to => $coord.offset('E'));
			@segments.push($seg);
		}
		my $e_val = $map.get($coord.offset('E'));
		if $e_val ne $value {
			my $seg = Segment.new(from => $coord.offset('E'), 
								  to => $coord.offset('SE'));
			@segments.push($seg);
		}
		my $s_val = $map.get($coord.offset('S'));
		if $s_val ne $value {
			my $seg = Segment.new(from => $coord.offset('SE'), 
								  to => $coord.offset('S'));
			@segments.push($seg);
		}
		my $w_val = $map.get($coord.offset('W'));
		if $w_val ne $value {
			my $seg = Segment.new(from => $coord.offset('S'), 
								  to => $coord);
			@segments.push($seg);
		}
	}

	my @h_segs = @segments.grep(-> $s {$s.is_horizontal});
	my %y_binned = ();
	for @h_segs -> $hs {
		if !(%y_binned{$hs.from.y}:exists) { %y_binned{$hs.from.y} = Array.new() }
		%y_binned{$hs.from.y}.push($hs);
	}
	
	my @v_segs = @segments.grep(-> $s {$s.is_vertical});
	my %x_binned = ();
	for @v_segs -> $vs {
		if !(%x_binned{$vs.from.x}:exists) { %x_binned{$vs.from.x} = Array.new() }
		%x_binned{$vs.from.x}.push($vs);
	}

	my $edge_count = 0;
	
	for %y_binned.kv -> $y, @segments {
		my $count = count_contiguous_edges(@segments);
		$edge_count += $count;
	}
	
	for %x_binned.kv -> $x, @segments {
		my $count = count_contiguous_edges(@segments);
		$edge_count += $count;
	}

	say "edges from region with $value: $edge_count";
	
	$edge_count;
}

sub count_contiguous_edges(@segments --> Int) {
	my $count = 0;

	my @west = @segments.grep(-> $s { $s.direction eq 'W' });
	my @east = @segments.grep(-> $s { $s.direction eq 'E' });
	my @north = @segments.grep(-> $s { $s.direction eq 'N' });
	my @south = @segments.grep(-> $s { $s.direction eq 'S' });

	@west = @west.sort: { $^b.from.x <=> $^a.from.x };
	@east = @east.sort: { $^a.from.x <=> $^b.from.x };
	@north = @north.sort: { $^b.from.y <=> $^a.from.y };
	@south = @south.sort: { $^a.from.y <=> $^b.from.y };

	say "West: " ~ @west.join(' ');
	say "East: " ~ @east.join(' ');
	say "North: " ~ @north.join(' ');
	say "South: " ~ @south.join(' ');
	
	$count++ if @west.elems > 0;
	for (1..@west.elems-1) -> $i {
		if abs(@west[$i].from.x - @west[$i-1].from.x) > 1 {$count += 1;}		
	}
	$count++ if @east.elems > 0;
	for (1..@east.elems-1) -> $i {
		if abs(@east[$i].from.x - @east[$i-1].from.x) > 1 {$count += 1;}		
	}
	$count++ if @north.elems > 0;
	for (1..@north.elems-1) -> $i {
		if abs(@north[$i].from.y - @north[$i-1].from.y) > 1 {$count += 1;}		
	}
	$count++ if @south.elems > 0;
	for (1..@south.elems-1) -> $i {
		if abs(@south[$i].from.y - @south[$i-1].from.y) > 1 {$count += 1;}		
	}
	$count;
}
