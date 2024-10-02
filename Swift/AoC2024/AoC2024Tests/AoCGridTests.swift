//
//  AoCGridTests.swift
//  AoC2024Tests
//
//  Created by Simon Biickert on 2024-10-02.
//

import Testing

struct AoCGridTests {

	@Test func testGrid2D_create() async throws {
		let g1 = AoCGrid2D()
		#expect(g1.defaultValue == ".")
		#expect(g1.rule == .rook)
		#expect(g1.extent == nil)

		let g2 = AoCGrid2D(defaultValue: "x", rule: .queen)
		#expect(g2.defaultValue == "x")
		#expect(g2.rule == .queen)
		#expect(g2.extent == nil)
	}
	
	@Test func testGrid2D_values() async throws {
		let g1 = AoCGrid2D()
		g1.setValue("@", at: AoCCoord2D.origin)
		#expect(g1.extent != nil)
		#expect(g1.extent!.width == 1 && g1.extent!.height == 1)
		let at = g1.stringValue(at: AoCCoord2D.origin)
		#expect(at == "@")
		
		g1.setValue("!", at: AoCCoord2D(x: 5, y: 4))
		#expect(g1.extent != nil)
		#expect(g1.extent!.width == 6 && g1.extent!.height == 5)
		let excl = g1.stringValue(at: AoCCoord2D(x: 5, y: 4))
		#expect(excl == "!")
		
		let def = g1.stringValue(at: AoCCoord2D(x: 10, y: 10))
		#expect(def == g1.defaultValue)
		
		let sample = SampleRenderable(letter: "Pig")
		g1.setValue(sample, at: AoCCoord2D(x: -1, y: -1))
		let sampleStr = g1.stringValue(at: AoCCoord2D(x: -1, y: -1))
		#expect(sampleStr == "P")
		if let sampleVal = g1.value(at: AoCCoord2D(x: -1, y: -1)) as? SampleRenderable {
			#expect(sampleVal == sample)
		}
		
		let coords = g1.coords
		#expect(coords.count == 3) // Doesn't include defaults
		
		let counts = g1.histogram
		#expect(counts.count == 4)
	}
	
	@Test func testGrid2D_coords() async throws {
		let g1 = AoCGrid2D()
		g1.setValue("@", at: AoCCoord2D.origin)
		g1.setValue("!", at: AoCCoord2D(x: 5, y: 4))

		let offsets = g1.neighbourOffsets
		#expect(offsets.count == 4)
		
		let coords = g1.neighbourCoords(at: AoCCoord2D.origin)
		#expect(coords.count == 4)
		#expect(coords[0].isAdjacent(to: AoCCoord2D.origin, rule: g1.rule))
		
		g1.setValue("A", at: AoCCoord2D(x: 1, y: 0))
		g1.setValue("B", at: AoCCoord2D(x: 0, y: 1))
		let coordsWithA = g1.neighbourCoords(at: AoCCoord2D.origin, withValue: "A")
		#expect(coordsWithA.count == 1)
		#expect(coordsWithA[0] == AoCCoord2D(x: 1, y: 0))
	}
	
	@Test func testGrid2D_draw() async throws {
		let g1 = AoCGrid2D()
		g1.setValue("@", at: AoCCoord2D.origin)
		g1.setValue("!", at: AoCCoord2D(x: 5, y: 4))
		g1.setValue(SampleRenderable(letter: "Pig"), at: AoCCoord2D(x: 5, y: 5))

		let coords = g1.neighbourCoords(at: AoCCoord2D.origin)
		for coord in coords {
			g1.setValue("N", at: coord)
		}

		g1.draw()
		
		var markers = Dictionary<AoCCoord2D, String>()
		markers[AoCCoord2D(x: 3, y: 4)] = "*"
		markers[AoCCoord2D(x: 0, y: 8)] = "&" // Will not draw, outside extent
		
		g1.draw(markers: markers)
	}

}

struct SampleRenderable: AoCGridRenderable, Equatable {
	let letter: String
	
	var glyph: String {
		if letter.isEmpty == false {
			return "\(letter[0])"
		}
		return "X"
	}
}
