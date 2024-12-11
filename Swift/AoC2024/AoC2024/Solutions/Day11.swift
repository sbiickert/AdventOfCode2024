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
		
		let numbers: [Int] = input.textLines.first!
			.split(separator: " ")
			.map(String.init)
			.map({Int($0)!})
		let stones = numbers.map(Stone.init)
		
		let result1 = solvePart(stones, iterations: 25)
		let result2 = solvePart(stones, iterations: 75)

		return AoCResult(part1: result1, part2: result2)
	}
	
	private func solvePart(_ stones: [Stone], iterations :Int) -> String {
		var input = stones
		print(input.map({$0.description}).joined(separator: " "))
		let strideLength = 5
		for i in stride(from: 1, through: iterations, by: strideLength) {
			print("\(i)")
			var result = [Stone]()
			for stone in input {
				result.append(contentsOf: stone.multiBlink(strideLength))
			}
			input = result
			//print(input.map(\.description).joined(separator: " "))
		}
		
		return "The number of stones after \(iterations) iterations is \(input.count)."
	}
}

struct Stone {
	let number: Int
	
	static var cache = Dictionary<Int, Dictionary<Int, [Stone]>>()
	
	static func getCache(number:Int, iterations:Int) -> [Stone]? {
		if cache[number] == nil {
			cache[number] = Dictionary<Int, [Stone]>()
			return nil
		}
		if cache[number]![iterations] == nil {
			return nil
		}
		return cache[number]![iterations]
	}
	
	static func setCache(number:Int, iterations:Int, stones:[Stone]) {
		if cache[number] == nil {
			cache[number] = Dictionary<Int, [Stone]>()
		}
		cache[number]![iterations] = stones
	}
	
	func multiBlink(_ iterations: Int) -> [Stone] {
		guard iterations > 0 else { return [] }
		if let cached = Stone.getCache(number: number, iterations: iterations) {
			return cached
		}
		var blinked = [self]
		for i in 1...iterations {
			if let cached = Stone.getCache(number: number, iterations: i) {
				blinked = cached
			}
			else {
				var result = [Stone]()
				for stone in blinked {
					result.append(contentsOf: stone.blink())
				}
				blinked = result
				Stone.setCache(number: number, iterations: i, stones: blinked)
			}
		}
		return blinked
	}
	
	func blink() -> [Stone] {
		if let cached = Stone.getCache(number: number, iterations: 1) {
			return cached
		}
		let numberString = "\(number)"
		var result: [Stone]
		if number == 0 {
			result = [Stone(number: 1)]
		}
		else if numberString.count.isMultiple(of: 2) {
			result = [
				Stone(number: Int(numberString.prefix(numberString.count/2))!),
				Stone(number: Int(numberString.suffix(numberString.count/2))!)
			]
		}
		else {
			result = [Stone(number: number * 2024)]
		}
		Stone.setCache(number: number, iterations: 1, stones: result)
		return result
	}
	
	var description: String {
		"(\(number))"
	}
	var debugDescription: String {
		description
	}
}
