//
//	AoCUtil.swift
//	AoC 2023
//
//	Created by Simon Biickert on 2023-04-26.
//

import Foundation

class AoCUtil {
	public static let ALPHABET = "abcdefghijklmnopqrstuvwxyz"
	
	static func rangeToArray(r: Range<Int>) -> [Int] {
		var result = [Int]()
		for i in r {
			result.append(i)
		}
		return result
	}
	
	static func xorRanges(r1: Range<Int>, r2: Range<Int>) -> [Range<Int>] {
		guard r1.overlaps(r2) else { return [r1, r2] }
		let common = r1.clamped(to: r2)
		var results = [Range<Int>]()
		for r in [r1, r2] {
			if r.lowerBound < common.lowerBound {
				let exclusiveRange = r.lowerBound..<common.lowerBound
				results.append(exclusiveRange)
			}
			if common.upperBound < r.upperBound {
				let exclusiveRange = common.upperBound..<r.upperBound
				results.append(exclusiveRange)
            }
        }
        return results
    }
    
    static func intersectRanges(r1: Range<Int>, r2: Range<Int>) -> Range<Int>? {
        guard r1.overlaps(r2) else { return nil }
        return r1.clamped(to: r2)
    }
    
    static func cRangeToArray(r: ClosedRange<Int>) -> [Int] {
        var result = [Int]()
        for i in r {
            result.append(i)
        }
        return result
    }
    
    static func numberToIntArray(_ n: String) -> [Int] {
        let arr = Array(n)
        return arr.map { Int(String($0))! }
    }
    
	// Have included Swift Algorithms as a dependency. Do not need this.
//    static func minMaxOf(array: [Int]) -> (Int, Int)? {
//        guard !array.isEmpty else { return nil }
//        
//        var min = Int.max
//        var max = Int.min
//        for value in array {
//            if value < min { min = value }
//            if value > max { max = value }
//        }
//        
//        return (min, max)
//    }
    
    static func trueMod(num: Int, mod: Int) -> Int {
        return (mod + (num % mod)) % mod;
    }
    
    static func gcd(_ x:Int, _ y:Int) -> Int {
        var a = 0
        var b = max(x,y)
        var r = min(x,y)
        while r != 0 {
            a = b
            b = r
            r = a % b
        }
        return b
    }
    
    static func lcm(_ x:Int, _ y:Int) -> Int {
        return x / gcd(x, y) * y
    }
    
    static func lcm(values:[Int]) -> Int {
        guard !values.isEmpty else { return 0 }
        var running = values.first!
        for n in 1..<values.count {
            running = lcm(running,values[n])
        }
        return running
    }
	
	static func pivotMatrix(_ matrix: [[Any]]) -> [[Any]] {
		guard !matrix.isEmpty else { return [] }
		let nrow = matrix.count
		let ncol = matrix[0].count
		
		var pivot: [[Any]] = []
		for _ in 0..<ncol {
			pivot.append([Any](repeating: 0, count: nrow))
		}
		
		for row in 0..<nrow {
			for col in 0..<ncol {
				pivot[col][row] = matrix[row][col]
			}
		}
		
		return pivot
	}
}


struct Fraction: Equatable {
	let numerator:Int
	let denominator:Int

	init(numerator: Int, denominator: Int) {
		self.numerator = numerator
		self.denominator = denominator
	}
	
	init(_ numerator: Int, _ denominator:Int) {
		self.numerator = numerator
		self.denominator = denominator
	}
	
	func reduce() -> Fraction {
		let gcd = AoCUtil.gcd(numerator, denominator)
		return Fraction(numerator: numerator/gcd, denominator: denominator/gcd)
	}
}

