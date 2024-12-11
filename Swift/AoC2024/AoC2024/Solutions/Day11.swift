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
		
		let numbers: [Int] = input.textLines.first!.split(separator: " ").map(String.init).map({Int($0)!})
		let stones = numbers.map(Stone.init)
		
		let result1 = solvePart(stones, iterations: 25)
		let result2 = "nope" //solvePart(stones, iterations: 75)

		return AoCResult(part1: result1, part2: result2)
	}
	
	private func solvePart(_ stones: [Stone], iterations :Int) -> String {
		var input = stones
		print(input.map({$0.description}).joined(separator: " "))
		for i in 0..<iterations {
			print("\(i)")
			var result = [Stone]()
			for stone in input {
				result.append(contentsOf: stone.blink())
			}
			input = result
			//print(input.map(\.description).joined(separator: " "))
		}
		
		return "The number of stones after \(iterations) iterations is \(input.count)."
	}
}

struct Stone {
	let number: Int
	
	func blink() -> [Stone] {
		let numberString = "\(number)"
		if number == 0 {
			return [Stone(number: 1)]
		}
		else if numberString.count.isMultiple(of: 2) {
			return [
				Stone(number: Int(numberString.prefix(numberString.count/2))!),
				Stone(number: Int(numberString.suffix(numberString.count/2))!)
			]
		}
		return [Stone(number: number * 2024)]
	}
	
	var description: String {
		"(\(number))"
	}
	var debugDescription: String {
		description
	}
}
