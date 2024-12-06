//
//  Day06.swift
//  AoC2024
//
//  Created by Simon Biickert on 2024-12-06.
//

import Foundation

class Day06: AoCSolution {
	override init() {
		super.init()
		day = 6
		self.name = "Guard Gallivant"
		self.emptyLinesIndicateMultipleInputs = true
	}
	
	override func solve(_ input: AoCInput) -> AoCResult {
		super.solve(input)
		
		let r1 = solvePart1(input.textLines)
		let r2 = solvePart2(input.textLines)
		
		return AoCResult(part1: r1, part2: r2)
	}
	
	func solvePart1(_ input: [String]) -> String {
		let map = AoCGrid2D()
		map.load(data: input)
		_ = walk(map: map)
		let visitedCount = map.getCoords(withValue: "X").count
		return "Number of places the guard walks is \(visitedCount)."
	}
	
	
	func solvePart2(_ input: [String]) -> String {
		let map = AoCGrid2D()
		map.load(data: input)
		let start = map.getCoords(withValue: "^").first!
		_ = walk(map: map, markPath: true)
		let visited = map.getCoords(withValue: "X")
		map.load(data: input)
		
		var blockPoints = [AoCCoord2D]()
		var workItems = [DispatchWorkItem]()
		for coord in visited {
			if coord == start { continue }
			let dwi = DispatchWorkItem { [self] in
				let localMap = AoCGrid2D()
				localMap.load(data: input)
				localMap.setValue("#", at: coord)
				let end_pos = walk(map: localMap, markPath: false)
				if map.extent!.contains(end_pos.location) {
					blockPoints.append(coord)
				}
			}
			DispatchQueue.global().async(execute: dwi)
			workItems.append(dwi)
		}
		for dwi in workItems { dwi.wait() }
		
		return "The number of potential obstacles is \(blockPoints.count)."
	}
	
	func walk(map: AoCGrid2D, markPath:Bool = true) -> AoCPos2D {
		let start = map.getCoords(withValue: "^").first!
		var pos = AoCPos2D(location: start, direction: .north)
		let ext = map.extent!
		var positions: Set<AoCPos2D> = []
		
		while ext.contains(pos.location) {
			if markPath { map.setValue("X", at: pos.location)}
			let nextPos = pos.movedForward()
			if map.stringValue(at: nextPos.location) == "#" {
				pos = pos.turned(.right)
			}
			else {
				pos = nextPos
			}
			if positions.contains(pos) {
				return pos //loop
			}
			positions.insert(pos)
		}
		return pos
	}
}
