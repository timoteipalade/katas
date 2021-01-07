//
//  Day2.swift
//  AdventOfCode2020
//
//  Created by Palade Timotei on 03.01.21.
//

import Foundation

struct PasswordContainer {
    let password: String
    let policy: PasswordPolicy
    
    static func from(_ string: String) -> PasswordContainer {
        var temp = string.split(separator: ":")
        assert(temp.count == 2)
        let passPolicyString = temp.first!
        let password = String(temp[1])
        
        temp = passPolicyString.split(separator: " ")
        assert(temp.count == 2)
        let character = Character(String(temp[1]))
        let minMax = temp.first!
        
        temp = minMax.split(separator: "-")
        let min = Int(temp.first!)!
        let max = Int(temp[1])!
        
        let policy = PasswordPolicy(min: min, max: max, character: character)
        return PasswordContainer(password: password, policy: policy)
    }
}

struct PasswordPolicy {
    let min: Int
    let max: Int
    let character: Character
}

func day21(input: [String]) -> Int {
    var count = 0
    for string in input {
        let container = PasswordContainer.from(string)
        let characterCount = container.password.count(character: container.policy.character)
        if characterCount >= container.policy.min && characterCount <= container.policy.max {
            count = count + 1
        }
    }
    
    return count
}

func day22(input: [String]) -> Int {
    var count = 0
    for string in input {
        let container = PasswordContainer.from(string)
        let password = container.password
        let min = container.policy.min
        let max = container.policy.max
        let character = container.policy.character
        
        let first = password[min] == character
        let last = password[max] == character
        
        if (first || last) && !(first && last) {
            count = count + 1
        }
    }
    
    return count
}

func run21Example() {
    let example = """
        1-3 a: abcde
        1-3 b: cdefg
        2-9 c: ccccccccc
    """.split(separator: "\n").compactMap({ String($0) })
    
    let result = day21(input: example)
    
    print("Example Result is: \(result)")
}

func input21() -> [String] {
    let path = "/Users/paladetimotei/Documents/Developer/katas/AdventOfCode2020/AdventOfCode2020/input/input2_1.txt"
    return inputList(path: path)
}

func run21() {
    let input = input21()
    let result = day21(input: input)
    print("Result is: \(result)")
}

func run22Example() {
    let example = """
        1-3 a: abcde
        1-3 b: cdefg
        2-9 c: ccccccccc
    """.split(separator: "\n").compactMap({ String($0) })
    
    let result = day22(input: example)
    
    print("Example Result is: \(result)")
}

func run22() {
    let input = input21()
    let result = day22(input: input)
    print("Result is: \(result)")
}

extension String {
    func count(character: Character) -> Int {
        var count = 0
        for c in self {
            if c == character {
                count = count + 1
            }
        }
        return count
    }
}

extension StringProtocol {
    subscript(offset: Int) -> Character { self[index(startIndex, offsetBy: offset)] }
    subscript(range: Range<Int>) -> SubSequence {
        let startIndex = index(self.startIndex, offsetBy: range.lowerBound)
        return self[startIndex..<index(startIndex, offsetBy: range.count)]
    }
    subscript(range: ClosedRange<Int>) -> SubSequence {
        let startIndex = index(self.startIndex, offsetBy: range.lowerBound)
        return self[startIndex..<index(startIndex, offsetBy: range.count)]
    }
    subscript(range: PartialRangeFrom<Int>) -> SubSequence { self[index(startIndex, offsetBy: range.lowerBound)...] }
    subscript(range: PartialRangeThrough<Int>) -> SubSequence { self[...index(startIndex, offsetBy: range.upperBound)] }
    subscript(range: PartialRangeUpTo<Int>) -> SubSequence { self[..<index(startIndex, offsetBy: range.upperBound)] }
}
