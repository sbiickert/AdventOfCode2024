#!/usr/bin/env raku

use lib $*PROGRAM.dirname ~ '/lib';
use AOC::Util;
#use AOC::Geometry;
#use AOC::Grid;

my $INPUT_PATH = '../Input';
#my $INPUT_FILE = 'day23_test.txt';
my $INPUT_FILE = 'day23_challenge.txt';
my @input = read_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2024, Day 23: LAN Party";

my %lan = parse_connections(@input);

solve_part_one();
solve_part_two();

exit( 0 );

sub solve_part_one() {
	my %groups = ();
	for %lan.kv -> $computer, @connected_to {
		for @connected_to.combinations(2) -> @combo {
			my @group = ($computer, @combo.Slip).sort();
			if are_interconnected(@group) {
				my $joined = @group.join(',');
				%groups{@group.join(',')} = 1;
			}
		}
	}
	my @t_groups = ();
	for %groups.keys -> $group {
		@t_groups.push($group) if $group ~~ / t\w /;
	}

	say "Part One: the number of groups with a t* computer is " ~ @t_groups.elems;
}

sub solve_part_two() {
	my $password = '';
	my $largest_group_size = 3;
	for %lan.kv -> $computer, @connected_to {
		for @connected_to.combinations($largest_group_size) -> @combo {
			my @group = ($computer, @combo.Slip).sort();
			if are_interconnected(@group) {
				$largest_group_size++;
				$password = @group.join(',');
				last;
			}
		}
	}

	say "Part Two: the LAN password is $password";
}

my %cache = ();
sub are_interconnected(@computers --> Bool) {
	my $joined = @computers.join(',');
	return %cache{$joined} if %cache{$joined}:exists;
	
	for (0..@computers.end-1) -> $i {
		for ($i+1..@computers.end) -> $j {
			if !(@computers[$j] (elem) %lan{@computers[$i]}) {
				%cache{$joined} = False;
				return False
			}
		}
	}
	%cache{$joined} = True;
	True;
}

sub parse_connections(@input --> Hash) {
	my %connections = ();
	for @input -> $line {
		my $computer1 = $line.substr(0, 2);
		my $computer2 = $line.substr(3, 2);
		my $c = %connections{$computer1} ~~ Array ?? %connections{$computer1}.elems !! 0;
		%connections{$computer1}[$c] = $computer2;
		$c = %connections{$computer2} ~~ Array ?? %connections{$computer2}.elems !! 0;
		%connections{$computer2}[$c] = $computer1;
	}
	%connections;
}
