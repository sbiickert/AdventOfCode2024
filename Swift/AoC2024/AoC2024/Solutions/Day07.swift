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
		var possible = [Equation]()
		var workItems = [DispatchWorkItem]()
		for equation in equations {
			let dwi = DispatchWorkItem {
				if self.isEquationPossible1(equation) { possible.append(equation) }
			}
			DispatchQueue.global().async(execute: dwi)
			workItems.append(dwi)
		}
		for dwi in workItems { dwi.wait() }

		var sum = 0
		for equation in possible {
			sum += equation.answer
		}
		return "The sum of possible answers is \(sum)."
	}
	
	
	func solvePart2(_ equations: [Equation]) -> String {
		var possible = [Equation]()
		var workItems = [DispatchWorkItem]()
		for equation in equations {
			let dwi = DispatchWorkItem {
				if self.isEquationPossible2(equation) { possible.append(equation) }
			}
			DispatchQueue.global().async(execute: dwi)
			workItems.append(dwi)
		}
		for dwi in workItems { dwi.wait() }

		var sum = 0
		for equation in possible {
			sum += equation.answer
		}
		return "The sum of possible answers is \(sum)."
	}
	
	func isEquationPossible1(_ equation: Equation) -> Bool {
		if equation.numbers.count == 1 {
			return equation.answer == equation.numbers[0]
		}
		let mulEquation = equation.reducedWithMultiplication
		if isEquationPossible1(mulEquation) { return true }
		let addEquation = equation.reducedWithAddition
		if isEquationPossible1(addEquation) { return true }
		return false
	}
	
	func isEquationPossible2(_ equation: Equation) -> Bool {
		if equation.numbers.count == 1 {
			return equation.answer == equation.numbers[0]
		}
		let conEquation = equation.reducedWithConcatenation
		if isEquationPossible2(conEquation) { return true }
		let mulEquation = equation.reducedWithMultiplication
		if isEquationPossible2(mulEquation) { return true }
		let addEquation = equation.reducedWithAddition
		if isEquationPossible2(addEquation) { return true }
		return false
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
	
	var reducedWithMultiplication: Equation {
		var mutableNumbers = numbers
		let n0 = mutableNumbers.popLast()!
		let n1 = mutableNumbers.popLast()!
		let product = n0 * n1
		mutableNumbers.append(product)
		let eq = Equation(answer: self.answer, numbers: mutableNumbers)
//		print(eq.description)
		return eq
	}
	
	var reducedWithAddition: Equation {
		var mutableNumbers = numbers
		let n0 = mutableNumbers.popLast()!
		let n1 = mutableNumbers.popLast()!
		let sum = n0 + n1
		mutableNumbers.append(sum)
		let eq = Equation(answer: self.answer, numbers: mutableNumbers)
//		print(eq.description)
		return eq
	}
	
	var reducedWithConcatenation: Equation {
		var mutableNumbers = numbers
		let n0 = mutableNumbers.popLast()!
		let n1 = mutableNumbers.popLast()!
		let concat = Int("\(n0)\(n1)")!
		mutableNumbers.append(concat)
		let eq = Equation(answer: self.answer, numbers: mutableNumbers)
//		print(eq.description)
		return eq
	}
}
