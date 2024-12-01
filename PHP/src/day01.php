<?php

namespace sjb\aoc2024;

require 'lib/Util.php';
// require 'lib/Geometry.php';
// require 'lib/Grid.php';

echo "Advent of Code 2024, Day 01: Historian Hysteria\n";

$INPUT_DIR = '../Input/';
// $INPUT_FILE = 'day01_test.txt';
$INPUT_FILE = 'day01_challenge.txt';
$INPUT_INDEX = 0;

$input = read_grouped_input($INPUT_DIR . $INPUT_FILE, $INPUT_INDEX);

$data = parse_data($input);

$result1 = solvePartOne($data);
echo "Part One: $result1\n";

$result2 = solvePartTwo($data);
echo "Part Two: $result2\n";


function solvePartOne(array $data):int {
	$distances = array_map(fn ($i): int => abs($data[0][$i] - $data[1][$i]), range(0, count($data[0])-1));
	$total = array_sum($distances);
	return $total;
}

function solvePartTwo(array $data):int  {
	$freq = array();
	foreach ($data[1] as $num) {
		if (!array_key_exists($num, $freq)) {
			$freq[$num] = 0;
		}
		$freq[$num] += 1;
	}
	$sim_score = 0;
	foreach ($data[0] as $num) {
		if (array_key_exists($num, $freq)) {
			$sim_score += $num * $freq[$num];
		}
	}
	return $sim_score;
}

function parse_data(array $input): array {
	$data = array();
	foreach ($input as $line) {
		$nums = preg_split('/\s+/', $line);
		array_push($data, $nums);
	}
	$data = pivot_matrix($data);
	
	sort($data[0], SORT_NUMERIC);
	sort($data[1], SORT_NUMERIC);
	
	return $data;
}