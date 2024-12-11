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
		
		Stone.initCache(maxIterations: 35)
		let numbers: [Int] = input.textLines.first!
			.split(separator: " ")
			.map(String.init)
			.map({Int($0)!})
		let stones = numbers.map(Stone.init)
		
		let result1 = solvePart1(stones)
		let result2 = solvePart2(stones)

		return AoCResult(part1: result1, part2: result2)
	}
	
	private func solvePart1(_ stones: [Stone]) -> String {
		print(stones.map({$0.description}).joined(separator: " "))
		let result = doBlinking(stones, iterations: 25, strideLength: 5)
		return "The number of stones after 25 iterations is \(result.count)."
	}
	
	private func solvePart2(_ stones: [Stone]) -> String {
		print(stones.map({$0.description}).joined(separator: " "))
		let firstForty = doBlinking(stones, iterations: 40, strideLength: 10)
		
		var count = 0
		var i = 0
		print("Doing the next 35 iterations for \(firstForty.count) stones")
		for stone in firstForty {
			let result = stone.multiBlink(35)
			//let result = doBlinking([stone], iterations: 35, strideLength: 35)
			count += result.count
			i += 1
			if i % 1000 == 0 {
				print("\(i) \(count)")
			}
		}// 65601038650482 too low
		return "The number of stones after 75 iterations is \(count)."
	}
	
	
	private func doBlinking(_ stones: [Stone], iterations: Int, strideLength: Int) -> [Stone] {
		var blinked = stones
		for _ in stride(from: 1, through: iterations, by: strideLength) {
			//print("\(i)")
			var result = [Stone]()
			for stone in blinked {
				result.append(contentsOf: stone.multiBlink(strideLength))
			}
			blinked = result
			//print(input.map(\.description).joined(separator: " "))
		}
		return blinked
	}
}

struct Stone {
	let number: Int
	
	static var cache = [Dictionary<Int, [Stone]>]()
	
	static func initCache(maxIterations: Int) {
		cache.removeAll()
		for _ in 0...maxIterations {
			cache.append(Dictionary<Int, [Stone]>())
		}
	}
	static func getCache(number:Int, iterations:Int) -> [Stone]? {
//		if cache[iterations][number] == nil {
//			return nil
//		}
		return cache[iterations][number]
	}
	
	static func setCache(number:Int, iterations:Int, stones:[Stone]) {
		if cache[iterations][number] == nil {
			cache[iterations][number] = stones
		}
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
