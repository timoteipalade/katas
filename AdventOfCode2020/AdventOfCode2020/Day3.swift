//
//  Day3.swift
//  AdventOfCode2020
//
//  Created by Palade Timotei on 06.01.21.
//

import Foundation

func day31(input: [String], xStep: Int, yStep: Int) -> Int {
    let width = input.first!.count
    let height = input.count
    
    assert(width >= xStep)
    assert(height >= yStep)
    
    var x = xStep
    var y = yStep
    
    var count = 0
    
    while y < height {
        let row = input[y]
        if row[x] == "#" {
            count += 1
        }
        y += yStep
        x = (x + xStep) % width
    }
    
    return count
}

func run31_Example() {
    let example = """
    ..##.......
    #...#...#..
    .#....#..#.
    ..#.#...#.#
    .#...##..#.
    ..#.##.....
    .#.#.#....#
    .#........#
    #.##...#...
    #...##....#
    .#..#...#.#
    """
    
    let input = example.split(separator: "\n").compactMap({ String($0) })
    
    print(day31(input: input, xStep: 3, yStep: 1))
}

func input31() -> [String] {
    let path = "/Users/paladetimotei/Documents/Developer/Becoming Fantastic/AdventOfCode2020/AdventOfCode2020/input/input3_1.txt"
    return inputList(path: path)
}

func run31() {
    let input = input31()
    print(day31(input: input, xStep: 3, yStep: 1))
}

func day32(input: [String]) -> Int {
    let steps = [(1,1), (3,1), (5,1), (7,1), (1,2)]
    
    var product = 1
    
    for step in steps {
        product *= day31(input: input, xStep: step.0, yStep: step.1)
    }
    
    return product
}

func run32_Example() {
    let example = """
    ..##.......
    #...#...#..
    .#....#..#.
    ..#.#...#.#
    .#...##..#.
    ..#.##.....
    .#.#.#....#
    .#........#
    #.##...#...
    #...##....#
    .#..#...#.#
    """
    
    let input = example.split(separator: "\n").compactMap({ String($0) })
    
    print(day32(input: input))
}

func run32() {
    let input = input31()
    print(day32(input: input))
}
