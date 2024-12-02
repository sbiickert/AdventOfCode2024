//
//  Day02.swift
//  AoC2024
//
//  Created by Simon Biickert on 2024-12-02.
//

import Foundation

class Day02: AoCSolution {
	override init() {
		super.init()
		day = 2
		self.name = "Red-Nosed Reports"
		self.emptyLinesIndicateMultipleInputs = true
	}
	
	override func solve(_ input: AoCInput) -> AoCResult {
		super.solve(input)
		
		let reports = input.textLines.map { Report(source: $0) }
		
		let safeCount1 = solvePartOne(reports)
		return AoCResult(part1: "\(safeCount1)", part2: "sync")
	}
	
	private func solvePartOne(_ reports: [Report]) -> Int {
		let safeReports = reports.filter { $0.isSafe }
		return safeReports.count
	}
}

enum ReportTrajectory {
	case ascending
	case descending
	case undefined
}

struct Report {
	let values: [Int]
	
	init(source: String) {
		let split = source.split(separator: " ")
		values = split.map { Int(String($0))! }
	}
	
	var isSafe: Bool {
		return Report.isSafe(values: values)
	}
	
	static func isSafe(values: [Int]) -> Bool {
		var trajectory: ReportTrajectory = .undefined
		
		for i in 1..<values.count {
			let diff = values[i] - values[i-1]
			if abs(diff) < 1 || abs(diff) > 3 {
				return false
			}
			
			if trajectory == .undefined {
				if diff > 0 { trajectory = .ascending }
				else		{ trajectory = .descending }
			}
			if trajectory == .descending && diff > 0 { return false }
			if trajectory == .ascending && diff < 0  { return false }
		}
		return true;
	}
	
	var isUnsafe: Bool {
		return !isSafe
	}
}
