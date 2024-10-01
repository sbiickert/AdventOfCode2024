//
//  AoCInputTests.swift
//  AoC2024Tests
//
//  Created by Simon Biickert on 2024-10-01.
//

import Testing

struct AoCInputTests {

    @Test func test_fileName() async throws {
		#expect(AoCInput.fileName(day: 0, isTest: true) == "day00_test.txt")
		#expect(AoCInput.fileName(day: 5, isTest: false) == "day05_challenge.txt")
    }

	@Test func test_inputPath() async throws {
		var p = AoCInput.inputPath(for: AoCInput.fileName(day: 0, isTest: true))
		#expect(p.relativeString == "file:///Users/sjb/Developer/Advent%20of%20Code/2024/AdventOfCode2024/Input/day00_test.txt")
		p = AoCInput.inputPath(for: AoCInput.fileName(day: 5, isTest: false))
		#expect(p.relativeString == "file:///Users/sjb/Developer/Advent%20of%20Code/2024/AdventOfCode2024/Input/day05_challenge.txt")
	}
	
	@Test func test_inputs() async throws {
		let s = Day00()
		var inputs = AoCInput.inputsFor(solution: s)
		#expect(inputs.count == 4)
		#expect(inputs.first?.textLines.count == 3)
		#expect(inputs.first?.textLines.first == "G0, L0")
		#expect(inputs[2].textLines[1] == "G1, L1")
	}
	
	@Test func test_readInputFile() async throws {
		
	}
	
	@Test func test_readGroupedInputFile() async throws {
		
	}
}
