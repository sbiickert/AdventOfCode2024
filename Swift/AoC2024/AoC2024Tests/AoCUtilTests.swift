//
//  AoC2024Tests.swift
//  AoC2024Tests
//
//  Created by Simon Biickert on 2024-09-30.
//

import Testing
import AoC2024

struct AoCUtilTests {

    @Test func test_rangeToArray() async throws {
        // Write your test here and use APIs like `#expect(...)` to check expected conditions.
		let arr = AoCUtil.rangeToArray(r: 0..<10)
		#expect(arr.count == 10)
		#expect(arr.first == 0)
		#expect(arr.last == 9)
    }

	@Test func test_xorRanges() async throws {
		#expect(AoCUtil.xorRanges(r1: 0..<10, r2: 10..<20) == [0..<10, 10..<20])
		#expect(AoCUtil.xorRanges(r1: 0..<15, r2: 10..<20) == [0..<10, 15..<20])
		#expect(AoCUtil.xorRanges(r1: 0..<20, r2: 10..<15) == [0..<10, 15..<20])
		#expect(AoCUtil.xorRanges(r1: 10..<15, r2: 0..<20) == [0..<10, 15..<20])
	}
	
	@Test func test_intersectRanges() async throws {
		#expect(AoCUtil.intersectRanges(r1: 0..<15, r2: 10..<20) == 10..<15)
		#expect(AoCUtil.intersectRanges(r1: 0..<20, r2: 10..<15) == 10..<15)
		#expect(AoCUtil.intersectRanges(r1: 0..<10, r2: 10..<20) == nil)
	}
	
	@Test func test_cRangeToArray() async throws {
		let arr = AoCUtil.cRangeToArray(r: 0...10)
		#expect(arr.count == 11)
		#expect(arr.first == 0)
		#expect(arr.last == 10)
	}
	
	@Test func test_numberToIntArray() async throws {
		#expect(AoCUtil.numberToIntArray("123456789") == [1, 2, 3, 4, 5, 6, 7, 8, 9])
	}
	
	@Test func test_trueMod() async throws {
		#expect(AoCUtil.trueMod(num: 10, mod: 3) == 1)
		#expect(AoCUtil.trueMod(num: 11, mod: 3) == 2)
		#expect(AoCUtil.trueMod(num: 12, mod: 3) == 0)
		
		#expect(AoCUtil.trueMod(num: -1, mod: 3) == 2)
		#expect(-1 % 3 == -1)
	}
	
	@Test func test_gcd() async throws {
		#expect(AoCUtil.gcd(2, 4) == 2)
		#expect(AoCUtil.gcd(15, 20) == 5)
		#expect(AoCUtil.gcd(13, 20) == 1)
	}
	
	@Test func test_lcm() async throws {
		#expect(AoCUtil.lcm(2, 4) == 4)
		#expect(AoCUtil.lcm(15, 20) == 60)
		#expect(AoCUtil.lcm(13, 20) == 260)
		#expect(AoCUtil.lcm(values: [2,3,4]) == 12)
		#expect(AoCUtil.lcm(values: [3,4,13]) == 156)
	}
	
	@Test func test_reduceFraction() async throws {
		let fractions:[[Int]] = [[2, 4], [2, 6], [2, 8], [3,13]]
		var reduced: [Fraction] = []
		for f in fractions {
			reduced.append(Fraction(numerator: f[0], denominator: f[1]).reduce())
		}
		#expect(reduced == [Fraction(1, 2), Fraction(1, 3), Fraction(1, 4), Fraction(3, 13)])
	}
}
