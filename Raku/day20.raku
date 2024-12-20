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

solve_parts($track, @course);

exit( 0 );

sub solve_parts(Grid $track, @course) {
	#say "track length: " ~ @course.elems;
	my $limit_p1 = @course.elems < 100 ?? 1 !! 100;
	my $limit_p2 = @course.elems < 100 ?? 50 !! 100;
	my %cheats_p1 = ();
	my %cheats_p2 = ();

	for (0..@course.end-1) -> $i {
		for ($i+2..@course.end) -> $j {
			my $md = @course[$i].manhattanDistanceTo(@course[$j]);
			my $saved = $j - $i - $md;
			if (1 < $md <= 2) && $saved >= $limit_p1 {
				my $key = @course[$i] ~ ',' ~ @course[$j];
				%cheats_p1{$key} = $saved;
			}
			if (1 < $md <= 20) && $saved >= $limit_p2 {
				my $key = @course[$i] ~ ',' ~ @course[$j];
				%cheats_p2{$key} = $saved;
			}
		}
		#if $i %% 500 { say $i }
	}

	#summarize_savings(%cheats_p1);
	say "Part One: there are " ~ %cheats_p1.elems ~ " cheats";

	#summarize_savings(%cheats_p2);
	say "Part Two: there are " ~ %cheats_p2.elems ~ " cheats";
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

# Was a sanity check for part one
#sub shortcut_between(Coord $c1, Coord $c2, Grid $track --> Str) {
#	my $dx = $c2.x - $c1.x;
#	my $dy = $c2.y - $c1.y;
#	my $cheat_coord = Coord.from_ints($c1.x + Int($dx/2), $c1.y + Int($dy/2));
#	die if $track.get($cheat_coord) ne '#';
#	"$c2,$cheat_coord";
#}
