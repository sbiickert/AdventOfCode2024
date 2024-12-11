//
//  day11.swift
//  AoC2024
//

import Foundation

class Day11: AoCSolution {
	override init() {
		super.init()
		day = 11
		self.name = "Plutonian Pebble"
		self.emptyLinesIndicateMultipleInputs = true
	}
	
	override func solve(_ input: AoCInput) -> AoCResult {
		super.solve(input)
		
		let stones: [Int] = input.textLines.first!
			.split(separator: " ")
			.map(String.init)
			.map({Int($0)!})
		
		let result1 = solvePart(stones, blinks: 25)
		let result2 = solvePart(stones, blinks: 75)

		return AoCResult(part1: result1, part2: result2)
	}
	
	private func solvePart(_ stones: [Int], blinks: Int) -> String {
		//print(stones.map({String($0)}).joined(separator: " "))
		var sum = 0
		for stone in stones {
			sum += countStones(stone, blinks: blinks)
		}
		return "The number of stones after \(blinks) iterations is \(sum)."
	}

	// Courtesy of https://www.reddit.com/r/adventofcode/comments/1hbm0al/comment/m1kpxgp/
	private var _countCache = Dictionary<String, Int>()
	private func countStones(_ stone: Int, blinks: Int) -> Int {
		let key = "\(stone) \(blinks)"
		if let cached = _countCache[key] { return cached }
		
		if blinks <= 0 {
			_countCache[key] = 1
			return 1
		}
		
		let numberString = "\(stone)"
		if stone == 0 {
			let result = countStones(1, blinks: blinks - 1)
			_countCache[key] = result
			return result
		}
		else if numberString.count.isMultiple(of: 2) {
			let left = Int(numberString.prefix(numberString.count/2))!
			let right = Int(numberString.suffix(numberString.count/2))!
			let result = countStones(left, blinks: blinks - 1) +
						 countStones(right, blinks: blinks - 1)
			_countCache[key] = result
			return result
		}
		let result = countStones(stone * 2024, blinks: blinks - 1)
		_countCache[key] = result
		return result
	}
}
