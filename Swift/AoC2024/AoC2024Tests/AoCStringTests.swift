//
//  AoCStringTests.swift
//  AoC2024Tests
//
//  Created by Simon Biickert on 2024-10-01.
//

import Testing

struct AoCStringTests {

    @Test func test_subscript() async throws {
        let str = "Hello, World!"
		#expect(str[3] == "l")
    }

	@Test func test_indexesOf() async throws {
		let str = "Hello, World!"
		#expect(str.indexesOf(string: "o") == [4, 8])
		#expect(str.indexesOf(string: "ll") == [2])
	}
}
