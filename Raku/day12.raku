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

#$map.print();

my @regions = solve_part_one($map);
solve_part_two($map, @regions);

exit( 0 );

sub solve_part_one(Grid $map --> Array) {
	my %hist = $map.histogram();

	my $sum = 0;
	my @all_regions = ();
	for %hist.kv -> $letter, $count {
		my %counted = ();

		# Regions with the same letter may be non-contiguous
		my @coords_with_letter = $map.coords($letter);
		while @coords_with_letter.elems > 0 {
			my $c = @coords_with_letter.pop();
			# If this coordinate has already been assigned a region, next
			next if %counted{$c.Str}:exists;
			# Assign this coord to start the region
			my %region = ($c.Str => $c);
			# Recursively finds all contiguous coords with the same letter
			find_region($map, $c, %region);
			
			my $perimeter = 0;
			for %region.kv -> $key, $coord {
				# Get all neighbors with the same letter
				my @neighbors = $map.neighbors($coord).grep(-> $n {$map.get($n) eq $letter});
				# The number of sides added to perimeter is 4 - number of same neighbors
				my $p = 4 - @neighbors.elems;
				$perimeter += $p;
				# Note that we've assigned this coord to a region
				%counted{$coord.Str} = 1;
			}
			# Cost is Perimeter * Area
			$sum += $perimeter * %region.elems;
			# Saving the regions for Part Two
			@all_regions.push(%region);
		}
	}

	say "Part One: the total price is $sum";
	@all_regions;
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

sub solve_part_two(Grid $map, @regions) {
	my $sum = 0;
	for @regions -> %region {
		# All segments that outline the region
		my @edge_segments = find_edge_segments($map, %region);
		# Order them into edges and count them
		my $edge_count = count_edges(@edge_segments);
		# Cost is Edge Count * Area
		$sum += $edge_count * %region.elems;
	}
	say "Part Two: the total price is $sum";
}


sub find_edge_segments(Grid $map, %region --> Array) {
	my @segments = ();

	# Similar to perimeter, except this time we're recording
	# the edge. Directionality is clockwise.
	for %region.kv -> $key, $coord {
		my $value = $map.get($coord);
		
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
	@segments;
}

sub count_edges(@segments --> Int) {
	# Find all discrete y values for horizontal edges
	my @h_segs = @segments.grep(-> $s {$s.is_horizontal});
	my %y_binned = ();
	for @h_segs -> $hs {
		if !(%y_binned{$hs.from.y}:exists) { %y_binned{$hs.from.y} = Array.new() }
		%y_binned{$hs.from.y}.push($hs);
	}

	# Find all discrete x values for vertical edges
	my @v_segs = @segments.grep(-> $s {$s.is_vertical});
	my %x_binned = ();
	for @v_segs -> $vs {
		if !(%x_binned{$vs.from.x}:exists) { %x_binned{$vs.from.x} = Array.new() }
		%x_binned{$vs.from.x}.push($vs);
	}

	# Organize the segments into connected edges pointing the same way
	# and count them 
	my $edge_count = 0;
	
	for %y_binned.kv -> $y, @segments {
		my $count = count_contiguous_h_edges(@segments);
		$edge_count += $count;
	}
	
	for %x_binned.kv -> $x, @segments {
		my $count = count_contiguous_v_edges(@segments);
		$edge_count += $count;
	}
	
	$edge_count;
}

# I'm ashamed that the H and V code is so duplicated...

sub count_contiguous_h_edges(@segments --> Int) {
	my $count = 0;

	# Group into segments going W and E
	my @west = @segments.grep(-> $s { $s.direction eq 'W' });
	my @east = @segments.grep(-> $s { $s.direction eq 'E' });

	# Sort (hopefully) nose-to-tail
	@west = @west.sort: { $^b.from.x <=> $^a.from.x };
	@east = @east.sort: { $^a.from.x <=> $^b.from.x };

	$count++ if @west.elems > 0;
	for (1..@west.elems-1) -> $i {
		# Connected if abs(dx) is 1
		if abs(@west[$i].from.x - @west[$i-1].from.x) > 1 {$count += 1;}		
	}
	$count++ if @east.elems > 0;
	for (1..@east.elems-1) -> $i {
		# Connected if abs(dx) is 1
		if abs(@east[$i].from.x - @east[$i-1].from.x) > 1 {$count += 1;}		
	}
	$count;
}

sub count_contiguous_v_edges(@segments --> Int) {
	my $count = 0;

	# Group into segments going N and S
	my @north = @segments.grep(-> $s { $s.direction eq 'N' });
	my @south = @segments.grep(-> $s { $s.direction eq 'S' });

	# Sort (hopefully) nose-to-tail
	@north = @north.sort: { $^b.from.y <=> $^a.from.y };
	@south = @south.sort: { $^a.from.y <=> $^b.from.y };

	$count++ if @north.elems > 0;
	for (1..@north.elems-1) -> $i {
		# Connected if abs(dy) is 1
		if abs(@north[$i].from.y - @north[$i-1].from.y) > 1 {$count += 1;}		
	}
	$count++ if @south.elems > 0;
	for (1..@south.elems-1) -> $i {
		# Connected if abs(dy) is 1
		if abs(@south[$i].from.y - @south[$i-1].from.y) > 1 {$count += 1;}		
	}
	$count;
}
