//
//  Day01.swift
//  AoC 2024
//

import Foundation

class Day01: AoCSolution {
	override init() {
		super.init()
		day = 1
		self.name = "Historian Hysteria"
		self.emptyLinesIndicateMultipleInputs = true
	}
	
	override func solve(_ input: AoCInput) -> AoCResult {
		super.solve(input)
		let data = LocData(input.textLines)
		
		let r1 = solvePart1(data)
		let r2 = solvePart2(data)
		
		return AoCResult(part1: r1, part2: r2)
	}
	
	func solvePart1(_ data: LocData) -> String {
		let distances = (0..<data.list1.count).map {abs(data.list1[$0] - data.list2[$0])}
		let total = distances.reduce(0, +)
		return "\(total)"
	}
	
	
	func solvePart2(_ data: LocData) -> String {
		let frequencyMap = data.frequencyMap
		var similarity: Int = 0
		for num in data.list1 {
			similarity += num * (frequencyMap[num] ?? 0)
		}
		return "\(similarity)"
	}
}
	

struct LocData {
	let list1: [Int]
	let list2: [Int]
	init (_ input: [String]) {
		var matrix = [[Int]]()
		for line in input {
			let numbers = line.split(separator: /\s+/)
			matrix.append(numbers.map { Int($0)! })
		}
		let pivot = AoCUtil.pivotMatrix(matrix)
		self.list1 = pivot[0].map { $0 as! Int }.sorted()
		self.list2 = pivot[1].map { $0 as! Int }.sorted()
	}
	
	var frequencyMap: [Int: Int] {
		return list2.reduce(into: [:]) { $0[$1, default: 0] += 1 }
	}
}
