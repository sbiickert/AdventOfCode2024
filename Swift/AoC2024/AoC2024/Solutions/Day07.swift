//
//  Day07.swift
//  AoC2024
//
//  Created by Simon Biickert on 2024-12-07.
//

import Foundation

class Day07: AoCSolution {
	override init() {
		super.init()
		day = 7
		self.name = "Bridge Repair"
		self.emptyLinesIndicateMultipleInputs = true
	}
	
	override func solve(_ input: AoCInput) -> AoCResult {
		super.solve(input)
		
		let equations = input.textLines.map {Equation($0) }
		
		let r1 = solvePart1(equations)
		let r2 = solvePart2(equations)
		
		return AoCResult(part1: r1, part2: r2)
	}
	
	func solvePart1(_ equations: [Equation]) -> String {
		var sum = 0
		
		var workItems = [DispatchWorkItem]()
		for equation in equations {
			let dwi = DispatchWorkItem {
				if equation.isEquationPossible() { sum += equation.answer }
			}
			DispatchQueue.global().async(execute: dwi)
			workItems.append(dwi)
		}
		for dwi in workItems { dwi.wait() }

		return "The sum of possible answers is \(sum)."
	}
	
	
	func solvePart2(_ equations: [Equation]) -> String {
		var sum = 0

		var workItems = [DispatchWorkItem]()
		for equation in equations {
			let dwi = DispatchWorkItem {
				if equation.isEquationPossible(allowConcatenation: true) { sum += equation.answer }
			}
			DispatchQueue.global().async(execute: dwi)
			workItems.append(dwi)
		}
		for dwi in workItems { dwi.wait() }

		return "The sum of possible answers is \(sum)."
	}
}

struct Equation {
	let answer: Int
	let numbers: [Int]
	
	init(_ source: String) {
		let parts = Array(source.split(separator: /\:?\s/))
		answer = Int(parts.first!)!
		numbers = parts[1..<parts.count].map({ Int(String($0))! }).reversed()
	}
	
	init(answer: Int, numbers: [Int]) {
		self.answer = answer
		self.numbers = numbers
	}
	
	var description: String {
		return "\(self.answer) = \(self.numbers)"
	}
	
	func isEquationPossible(allowConcatenation ac: Bool = false) -> Bool {
		if numbers.count == 1 {
			return answer == numbers[0]
		}
		if ac {
			let conEquation = self.reducedWithConcatenation
			if conEquation.isEquationPossible(allowConcatenation: true) { return true }
		}
		let mulEquation = self.reducedWithMultiplication
		if mulEquation.isEquationPossible(allowConcatenation: ac) { return true }
		let addEquation = self.reducedWithAddition
		if addEquation.isEquationPossible(allowConcatenation: ac) { return true }
		return false
	}

	var reducedWithMultiplication: Equation {
		var mutableNumbers = numbers
		let n0 = mutableNumbers.popLast()!
		let n1 = mutableNumbers.popLast()!
		let product = n0 * n1
		mutableNumbers.append(product)
		return Equation(answer: self.answer, numbers: mutableNumbers)
	}
	
	var reducedWithAddition: Equation {
		var mutableNumbers = numbers
		let n0 = mutableNumbers.popLast()!
		let n1 = mutableNumbers.popLast()!
		let sum = n0 + n1
		mutableNumbers.append(sum)
		return Equation(answer: self.answer, numbers: mutableNumbers)
	}
	
	var reducedWithConcatenation: Equation {
		var mutableNumbers = numbers
		let n0 = mutableNumbers.popLast()!
		let n1 = mutableNumbers.popLast()!
		let concat = Int("\(n0)\(n1)")!
		mutableNumbers.append(concat)
		return Equation(answer: self.answer, numbers: mutableNumbers)
	}
}
