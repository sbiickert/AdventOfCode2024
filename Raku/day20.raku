#!/usr/bin/env raku

use lib $*PROGRAM.dirname ~ '/lib';
use AOC::Util;
use AOC::Geometry;
use AOC::Grid;

my $INPUT_PATH = '../Input';
#my $INPUT_FILE = 'day20_test.txt';
my $INPUT_FILE = 'day20_challenge.txt';
my @input = read_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2024, Day 20: Race Condition";

my $track = Grid.new(default => '.', rule => AdjacencyRule::ROOK);
$track.load(@input);
my $start = $track.coords('S')[0];
my $end = $track.coords('E')[0];

#$track.print;
my @course = walk_track($track);

solve_part_one($track, @course);
solve_part_two($track, @course);

exit( 0 );

sub solve_part_one(Grid $track, @course) {
	say "track length: " ~ @course.elems;
	my $limit = @course.elems < 100 ?? 1 !! 100;
	my %cheats = ();

	for (0..@course.end-1) -> $i {
		for ($i+1..@course.end) -> $j {
			my $md = @course[$i].manhattanDistanceTo(@course[$j]);
			my $saved = $j - $i - $md;
			if (1 < $md <= 2) && $saved >= $limit {
				my $key = @course[$i] ~ ',' ~ @course[$j];
				%cheats{$key} = $saved;
			}
		}
		if $i %% 500 { say $i }
	}

	summarize_savings(%cheats);

	say "Part One: there are " ~ %cheats.elems ~ " cheats"; # 6810 too high
}

sub solve_part_two(Grid $track, @course) {
	my $limit = @course.elems < 100 ?? 50 !! 100;
	my %cheats = ();

	for (0..@course.end-1) -> $i {
		for ($i+1..@course.end) -> $j {
			my $md = @course[$i].manhattanDistanceTo(@course[$j]);
			my $saved = $j - $i - $md;
			if (1 < $md <= 20) && $saved >= $limit {
				my $key = @course[$i] ~ ',' ~ @course[$j];
				%cheats{$key} = $saved;
#				my @shortcuts = shortcuts_between(@course[$i],@course[$j], $track);
#				for @shortcuts -> $key {
#					%cheats{$key} = $saved;
#				}
			}
		}
		if $i %% 500 { say $i }
	}

	summarize_savings(%cheats);

	say "Part Two: there are " ~ %cheats.elems ~ " cheats";
}

sub summarize_savings(%cheats) {
	my %savings = ();
	for %cheats.kv -> $key,$saved {
		%savings{$saved} += 1;
	}

	for %savings.keys.sort({$^a <=> $^b}) -> $saved {
		say %savings{$saved} ~ " cheats saved $saved";
	} 
}

sub walk_track(Grid $track --> Array) {
	my @course = ();
	my $step = 0;
	my $pos = $start;

	$track.set($end, '.');
	while ($pos eqv $end) == False {
		$track.set($pos, $step);
		@course.push($pos);
		$pos = $track.neighbors($pos).grep(->$c {$track.get($c) eq '.'})[0];
		$step++;
	}
	@course.push($pos);
	$track.set($pos, $step);
	@course;
}

sub shortcut_between(Coord $c1, Coord $c2, Grid $track --> Str) {
	my $dx = $c2.x - $c1.x;
	my $dy = $c2.y - $c1.y;
	my $cheat_coord = Coord.from_ints($c1.x + Int($dx/2), $c1.y + Int($dy/2));
	die if $track.get($cheat_coord) ne '#';
	"$c2,$cheat_coord";
}

####3###
###323##
##32#23#
#32#S#23
##32#23#
###323##
####3###
my %paths = ();
sub init_paths() {
	%paths = (
	"0,-2" =>  [[mk_c(0,-1), mk_c(0,-2)]],
	"1,-1" =>  [[mk_c(0,-1), mk_c(1,-1)],[mk_c(1,0),mk_c(1,-1)]],
	"2,0" =>   [[mk_c(1,0), mk_c(2,0)]],
	"1,1" =>   [[mk_c(0,1), mk_c(1,1)],[mk_c(1,0),mk_c(1,1)]],
	"0,2" =>   [[mk_c(0,1), mk_c(0,2)]],
	"-1,1" =>  [[mk_c(0,1), mk_c(-1,1)],[mk_c(-1,0),mk_c(-1,1)]],
	"-2,0" =>  [[mk_c(-1,0), mk_c(-2,0)]],
	"-1,-1" => [[mk_c(0,-1), mk_c(-1,-1)],[mk_c(-1,0),mk_c(-1,-1)]]);
	%paths{"0,-3"}  = %paths{"0,-2"};
	%paths{"1,-2"}  = %paths{"1,-1"};
	%paths{"2,-1"}  = %paths{"1,-1"};
	%paths{"3,0"}   = %paths{"2,0"};
	%paths{"2,1"}   = %paths{"1,1"};
	%paths{"1,2"}   = %paths{"1,1"};
	%paths{"0,3"}   = %paths{"0,2"};
	%paths{"-1,2"}  = %paths{"-1,1"};
	%paths{"-2,1"}  = %paths{"-1,1"};
	%paths{"-3,0"}  = %paths{"-2,0"};
	%paths{"-2,-1"} = %paths{"-1,-1"};
	%paths{"-1,-2"} = %paths{"-1,-1"};
}
sub mk_c(Int $x, Int $y) { Coord.new(x => $x, y => $y) }

sub shortcuts_between(Coord $c1, Coord $c2, Grid $track --> Array) {
	if %paths.elems == 0 { init_paths() }
	my $md = $c1.manhattanDistanceTo($c2);
	my $dx = $c2.x - $c1.x;
	my $dy = $c2.y - $c1.y;
	my @paths = %paths{"$dx,$dy"};
	if (@paths[0][0] ~~ Coord) == False { @paths = @paths[0].map(-> $p {$p}) }
	my @shortcuts = ();
	for @paths -> @path {
		my $p0 = $c1.add(@path[0]);
		my $val0 = $track.get($p0);
		next if $val0 ne '#';
		my $p1 = $c1.add(@path[1]);
		my $val1 = $track.get($p1);
		next if $val1 eq '#';
		@shortcuts.push("$p0,$p1");
	}
	@shortcuts;
}
