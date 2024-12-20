#!/usr/bin/env raku

use lib $*PROGRAM.dirname ~ '/lib';
use AOC::Util;
use AOC::Geometry;
use AOC::Grid;

my $INPUT_PATH = '../Input';
my $INPUT_FILE = 'day20_test.txt';
#my $INPUT_FILE = 'day20_challenge.txt';
my @input = read_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2024, Day 20: Race Condition";

my $track = Grid.new(default => '.', rule => AdjacencyRule::ROOK);
$track.load(@input);
my $start = $track.coords('S')[0];
my $end = $track.coords('E')[0];

$track.print;


solve_part_one($track);
#solve_part_two(@input);

exit( 0 );

sub solve_part_one(Grid $track) {
	my @course = walk_track($track);
	my $limit = @course.elems < 100 ?? 1 !! 100;
	my %cheats = ();

	for (0..@course.end-1) -> $i {
		my $best_saved = 0;
		my $best_key = '';
		for ($i+1..@course.end) -> $j {
			my $md = @course[$i].manhattanDistanceTo(@course[$j]);
			my $saved = $j - $i - $md;
			if (1 < $md <= 3) && $saved > 0 {
				my @shortcuts = shortcuts_between(@course[$i],@course[$j], $track);
				for @shortcuts -> $key {
					if $saved > $best_saved {
						$best_saved = $saved;
						$best_key = $key;
					}
				}
			}
			if $best_saved > 0 {
				say "save $best_saved $best_key";
				%cheats{$best_key} = $best_saved;
			}
		}
	}

	say "Part One: there are " ~ %cheats.elems ~ " cheats";
}

sub solve_part_two(@input) {
	
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
		next if $md == 3 && $val1 ne '#';
		@shortcuts.push("$p0,$p1");
	}
	@shortcuts;
}
