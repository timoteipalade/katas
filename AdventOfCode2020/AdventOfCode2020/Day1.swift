//
//  Day1_1.swift
//  AdventOfCode2020
//
//  Created by Palade Timotei on 02.01.21.
//

import Foundation

func inputList(path: String) -> [String] {
    let stringInput = try! String(contentsOfFile: path)
    return stringInput.split(separator: "\n").compactMap({ String($0) })
}

func intListInput(path: String) -> [Int] {
    return inputList(path: path).compactMap({ Int($0) })
}

func input11() -> [Int] {
    let path = "/Users/paladetimotei/Documents/Developer/katas/AdventOfCode2020/AdventOfCode2020/input/input1_1.txt"
    return intListInput(path: path)
}

func sum2020_bruteForce(input: [Int]) -> Int {
    for i in 0..<input.count {
        for j in 0..<input.count {
            guard i != j else { continue }
            if input[i] + input[j] == 2020 {
                return input[i] * input[j]
            }
        }
    }
    
    return -1
}

func sum2020(input: [Int]) -> Int {
    
    var negativeSet = Set<Int>()
    
    for number in input {
        let negative = 2020 - number
        
        if negativeSet.contains(number) {
            return number * negative
        }
        
        negativeSet.insert(negative)
    }
    
    return -1
}

func sum2020_3_bruteForce(input: [Int]) -> Int {
    for i in 0..<input.count {
        for j in 0..<input.count {
            for k in 0..<input.count {
                guard i != j, i != k, j != k else { continue }
                if input[i] + input[j] + input[k] == 2020 {
                    return input[i] * input[j] * input[k]
                }
            }
        }
    }
    return -1
}

func sum2020_3(input: [Int]) -> Int {
    // assumes that all numbers in the input are positive.
    // tip: when you have a bounded set, you can use an array instead of a set.
    // example: say you have an input of numbers between 0 and 2020,
    // then you can use an array of 2021 elements, and at each index you can store if that number equal to the index exists.
    
    var stored = Array.init(repeating: false, count: 2021)
    for number in input {
        stored[number] = true
    }
    
    var sorted = [Int]()
    
    for (i, exists) in stored.enumerated() {
        if exists {
            sorted.append(i)
        }
    }
    
    for i in 0..<sorted.count-1 {
        for j in i+1..<sorted.count {
            if i + j > 2020 {
                break
            }
            
            let k = 2020 - (i + j)
            if stored[k] {
                return i * j * k
            }
        }
    }
    
    return -1
}

func run11() {
    let input = input11()
    let bruteResult = sum2020_bruteForce(input: input)
    print("Brute Force Result: \(bruteResult)")
    
    let result = sum2020(input: input)
    print("Result: \(result)")
}

func run12() {
    let input = input11()
    print("Result: \(sum2020_3_bruteForce(input: input))")
}
