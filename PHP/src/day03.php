<?php

namespace sjb\aoc2024;

require 'lib/Util.php';
// require 'lib/Geometry.php';
// require 'lib/Grid.php';

echo "Advent of Code 2024, Day 3: Mull It Over\n";

$INPUT_DIR = '../Input/';
//$INPUT_FILE = 'day03_test.txt';
$INPUT_FILE = 'day03_challenge.txt';
$INPUT_INDEX = 0;

$input = read_grouped_input($INPUT_DIR . $INPUT_FILE, $INPUT_INDEX);

$joined = implode('', $input);

$result1 = solvePartOne($joined);
echo "Part One: $result1\n";

$result2 = solvePartTwo($joined);
echo "Part Two: $result2\n";


function solvePartOne(string $input):int {
	return sum_for_line($input);
}

function solvePartTwo(string $input):int {
	$dos = explode('do()', $input);
	$callback = fn(string $s): string => explode("don't()", $s)[0];
	$enabled_sections = array_map($callback, $dos);
	$line = implode('', $enabled_sections);
	$sum = sum_for_line($line);
	return $sum;
}

function sum_for_line(string $line):int {
	preg_match_all("/mul\((\d+),(\d+)\)/", $line, $out, PREG_SET_ORDER);
	$callback = fn(array $a): int => intval($a[1]) * intval($a[2]);
	$products = array_map($callback, $out);
	$sum = array_sum($products);
	return $sum;
}
