//
//  AoCSpatialTests.swift
//  AoC2024Tests
//
//  Created by Simon Biickert on 2024-10-02.
//

import Testing
import AoC2024

struct AoCSpatialTests {
	
	@Test func test_direction() async throws {
		var up = AoCDir.fromAlias("^")
		#expect(up == AoCDir.north)
		up = AoCDir.fromAlias("up")
		#expect(up == AoCDir.north)
		up = AoCDir.fromAlias("u")
		#expect(up == AoCDir.north)
		
		let left = AoCDir.fromAlias("left")
		#expect(left == AoCDir.west)
	}
	
	@Test func test_turning() async throws {
		let up = AoCDir.north
		let left = AoCTurn.left.apply(to: up)
		#expect(left == AoCDir.west)
		var dir = up
		for _ in 0...5 {
			dir = AoCTurn.right.apply(to: dir)
		}
		#expect(dir == AoCDir.south)
		
		dir = up
		for _ in 0...6 {
			dir = AoCTurn.left.apply(to: dir, size: .fortyFive)
		}
		#expect(dir == .ne)
		
		let noTurn = AoCTurn.fromAlias("nothing")
		dir = noTurn.apply(to: up)
		#expect(dir == up)
	}
	
	@Test func test_c2d_creation() async throws {
		let c1 = AoCCoord2D(x: 1, y: 3)
		#expect(c1.x == 1)
		#expect(c1.y == 3)
		let origin = AoCCoord2D.origin
		#expect(origin.x == 0)
		#expect(origin.y == 0)
	}
	
	@Test func test_c2d_math() async throws {
		let c1 = AoCCoord2D(x: 1, y: 3)
		let c2 = AoCCoord2D(x: 10, y: 30)
		
		let sum = c1 + c2
		#expect(sum.x == 11)
		#expect(sum.y == 33)
		#expect(sum - c1 == c2)
		
		let diff = c2 - c1
		#expect(diff.x == 9)
		#expect(diff.y == 27)
	}
	
	@Test func test_c2d_distance() async throws {
		let c1 = AoCCoord2D(x: 1, y: 3)
		let c2 = AoCCoord2D(x: 10, y: 30)
		
		let d = c1.distance(to: c2)
		#expect(d > 28 && d < 29)
		
		let md = c1.manhattanDistance(to: c2)
		#expect(md == 36)
	}
	
	@Test func test_c2d_adjacent() async throws {
		let c1 = AoCCoord2D(x: 1, y: 3)
		let c2 = AoCCoord2D(x: 10, y: 30)
		let horiz = AoCCoord2D(x: 2, y: 3)
		let vert = AoCCoord2D(x: 1, y: 2)
		let diag = AoCCoord2D(x: 2, y: 2)
		
		#expect(c1.isAdjacent(to: c2) == false)
		#expect(c1.isAdjacent(to: c2, rule: .bishop) == false)
		#expect(c1.isAdjacent(to: c2, rule: .queen) == false)
		
		#expect(c1.isAdjacent(to: horiz) == true)
		#expect(c1.isAdjacent(to: horiz, rule: .bishop) == false)
		#expect(c1.isAdjacent(to: horiz, rule: .queen) == true)
		
		#expect(c1.isAdjacent(to: vert) == true)
		#expect(c1.isAdjacent(to: vert, rule: .bishop) == false)
		#expect(c1.isAdjacent(to: vert, rule: .queen) == true)
		
		#expect(c1.isAdjacent(to: diag) == false)
		#expect(c1.isAdjacent(to: diag, rule: .bishop) == true)
		#expect(c1.isAdjacent(to: diag, rule: .queen) == true)
		
		var offsets = AoCCoord2D.getAdjacentOffsets()
		#expect(offsets.count == 4)
		#expect(offsets[0] == AoCCoord2D(x: 0, y: -1))
		offsets = AoCCoord2D.getAdjacentOffsets(rule: .bishop)
		#expect(offsets.count == 4)
		offsets = AoCCoord2D.getAdjacentOffsets(rule: .queen)
		#expect(offsets.count == 8)
		
		var adj = c1.getAdjacentCoords()
		#expect(adj.count == 4)
		#expect(adj[0] == AoCCoord2D(x: 1, y: 2))
		adj = c1.getAdjacentCoords(rule: .bishop)
		#expect(adj.count == 4)
		#expect(adj[0] == AoCCoord2D(x: 0, y: 2))
		adj = c1.getAdjacentCoords(rule: .queen)
		#expect(adj.count == 8)
		#expect(adj[7] == AoCCoord2D(x: 2, y: 4))
	}
	
	@Test func test_c2d_direction() async throws {
		let c1 = AoCCoord2D.origin // 0,0
		
		let north = AoCDir(rawValue: "N")
		#expect(north == AoCDir.north)
		
		var n = c1 + AoCDir.north.offset
		#expect(n.x == 0 && n.y == -1)
		n = c1.offset(direction: .north) // Equivalent operation
		#expect(n.x == 0 && n.y == -1)
		let e = c1 + AoCDir.east.offset
		#expect(e.x == 1 && e.y == 0)
		let s = c1 + AoCDir.south.offset
		#expect(s.x == 0 && s.y == 1)
		let w = c1 + AoCDir.west.offset
		#expect(w.x == -1 && w.y == 0)
	}
	
	@Test func testPos2D() async throws {
		let p1 = AoCPos2D(location: AoCCoord2D.origin, direction: .north)
		var moved = p1.movedForward()
		#expect(moved.location == p1.location.offset(direction: .north))
		moved = p1.movedForward(distance: 10)
		#expect(moved.location == AoCCoord2D(x: 0, y: -10))
		
		let turned = p1.turned(.right)
		moved = turned.movedForward()
		#expect(moved.location == AoCCoord2D(x: 1, y: 0))
	}
	
	@Test func testExtent2D_create() async throws {
		let e1 = AoCExtent2D(min: AoCCoord2D.origin, max: AoCCoord2D(x: 10, y: 5))
		#expect(e1.min.x == 0 && e1.min.y == 0 && e1.max.x == 10 && e1.max.y == 5)
		#expect(e1.width == 11 && e1.height == 6)
		#expect(e1.area == 66)
		
		let e2 = AoCExtent2D(min: AoCCoord2D.origin, max: AoCCoord2D(x: -10, y: -5))
		#expect(e2.max.x == 0 && e2.max.y == 0 && e2.min.x == -10 && e2.min.y == -5)
		#expect(e2.width == 11 && e2.height == 6)
		#expect(e2.area == 66)
		
		let c1 = AoCCoord2D(x: 1, y: 1)
		let c2 = AoCCoord2D(x: 10, y: 10)
		let c3 = AoCCoord2D(x: 2, y: -5)
		var temp = AoCExtent2D.build(from: [c1, c2, c3])
		#expect(temp != nil)
		if let e3 = temp {
			#expect(e3.min.x == 1 && e3.min.y == -5 && e3.max.x == 10 && e3.max.y == 10)
			#expect(e3.width == 10 && e3.height == 16)
			#expect(e3.area == 160)
		}
		temp = AoCExtent2D.build(from: [AoCCoord2D]())
		#expect(temp == nil)
		
		let e4 = AoCExtent2D.build(1, 2, 3, 4)
		#expect(e4.min.x == 1 && e4.min.y == 2 && e4.max.x == 3 && e4.max.y == 4)
	}
	
	@Test func testExtent2D_modify() async throws {
		let e1 = AoCExtent2D(min: AoCCoord2D.origin, max: AoCCoord2D(x: 10, y: 5))
		let expanded = e1.expanded(toFit: AoCCoord2D(x: -5, y: 6))
		#expect(expanded.min.x == -5 && expanded.min.y == 0 &&
				expanded.max.x == 10 && expanded.max.y == 6)
		
		let inset = e1.inset(amount: 2)
		#expect(inset != nil)
		#expect(inset!.min.x == 2 && inset!.min.y == 2 &&
				inset!.max.x == 8 && inset!.max.y == 3)
		#expect(e1.inset(amount: 3) == nil)
	}
	
	@Test func testExtent2D_coords() async throws {
		let e1 = AoCExtent2D(min: AoCCoord2D.origin, max: AoCCoord2D(x: 5, y: 6))
		let coords = e1.allCoords
		#expect(coords.count == e1.area)
	}
	
	@Test func testExtent2D_relations() async throws {
		let e1 = AoCExtent2D(min: AoCCoord2D.origin, max: AoCCoord2D(x: 10, y: 5))
		
		#expect(e1.contains(AoCCoord2D(x: 1, y: 1)) == true)
		#expect(e1.contains(AoCCoord2D(x: 10, y: 1)) == true)
		#expect(e1.contains(AoCCoord2D(x: 11, y: 1)) == false)
		#expect(e1.contains(AoCCoord2D(x: -1, y: -1)) == false)
		
		let e = AoCExtent2D.build(1,1,10,10)
		var i = e.intersect(other: AoCExtent2D.build(5,5,12,12))
		#expect(i == AoCExtent2D.build(5,5,10,10))
		i = e.intersect(other: AoCExtent2D.build(5,5,7,7))
		#expect(i == AoCExtent2D.build(5,5,7,7))
		i = e.intersect(other: AoCExtent2D.build(1,1,12,2))
		#expect(i == AoCExtent2D.build(1,1,10,2))
		i = e.intersect(other: AoCExtent2D.build(11,11,12,12))
		#expect(i == nil)
		i = e.intersect(other: AoCExtent2D.build(1,10,10,20))
		#expect(i == AoCExtent2D.build(1,10,10,10))
		
		var products = e.union(other: AoCExtent2D.build(5,5,12,12))
		var expected = [AoCExtent2D.build(5,5,10,10),
						AoCExtent2D.build(1,1,4,4),
						AoCExtent2D.build(1,5,4,10),
						AoCExtent2D.build(5,1,10,4),
						AoCExtent2D.build(11,11,12,12),
						AoCExtent2D.build(11,5,12,10),
						AoCExtent2D.build(5,11,10,12)]
		#expect(verifyUnionResults(actual: products, expected: expected))
		products = e.union(other: AoCExtent2D.build(5,5,7,7))
		expected = [AoCExtent2D.build(5,5,7,7),
					AoCExtent2D.build(1,1,4,4),
					AoCExtent2D.build(1,8,4,10),
					AoCExtent2D.build(1,5,4,7),
					AoCExtent2D.build(8,1,10,4),
					AoCExtent2D.build(8,8,10,10),
					AoCExtent2D.build(8,5,10,7),
					AoCExtent2D.build(5,1,7,4),
					AoCExtent2D.build(5,8,7,10)]
		#expect(verifyUnionResults(actual: products, expected: expected))
		products = e.union(other: AoCExtent2D.build(1,1,12,2))
		expected = [AoCExtent2D.build(1,1,10,2),
					AoCExtent2D.build(1,3,10,10),
					AoCExtent2D.build(11,1,12,2)]
		#expect(verifyUnionResults(actual: products, expected: expected))
		products = e.union(other: AoCExtent2D.build(11,11,12,12))
		expected = [AoCExtent2D.build(11,11,12,12)];
		expected = [AoCExtent2D.build(1,1,10,10),
					AoCExtent2D.build(11,11,12,12)]
		#expect(verifyUnionResults(actual: products, expected: expected))
		products = e.union(other: AoCExtent2D.build(1,10,10,20))
		expected = [AoCExtent2D.build(1,10,10,10),
					AoCExtent2D.build(1,1,10,9),
					AoCExtent2D.build(1,11,10,20)]
		#expect(verifyUnionResults(actual: products, expected: expected))
	}
	
	private func verifyUnionResults(actual: [AoCExtent2D], expected: [AoCExtent2D]) -> Bool {
		if actual.count != expected.count { return false }
		for i in 0..<actual.count {
			if actual[i] != expected[i] {
				print("actual[\(i)]: \(actual[i]), expected[\(i)]: \(expected[i])")
				return false
			}
		}
		return true
	}
}
