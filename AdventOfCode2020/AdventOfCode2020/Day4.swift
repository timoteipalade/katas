//
//  Day4.swift
//  AdventOfCode2020
//
//  Created by Palade Timotei on 07.01.21.
//

import Foundation
/*
 byr (Birth Year)
 iyr (Issue Year)
 eyr (Expiration Year)
 hgt (Height)
 hcl (Hair Color)
 ecl (Eye Color)
 pid (Passport ID)
 cid (Country ID)
 */

func day41(input: String) -> Int {
    
    var ends = [Int]()
    for i in 1..<input.count-1 {
        if input[i] == "\n" && input[i+1] == "\n" {
            ends.append(i-1)
        }
    }
    ends.append(input.count - 1)
    
    var count = 0
    var start = 0
    for end in ends {
        let passport = input[start...end]
        if isValid(String(passport)) {
            count += 1
        }
        start = end + 3
    }
    
    return count
}


func isValid(_ passport: String) -> Bool {
    // passport keys can be separated by new lines or spaces
    let entries = passport.split(separator: "\n").flatMap({ $0.split(separator: " ")})
    var requiredKeys = Set<PassportKey>()
    for entry in entries {
        let comp = entry.split(separator: ":")
        guard let key = PassportKey(rawValue: String(comp.first!)) else { continue }
        
        if key.isRequired()  {
            requiredKeys.insert(key)
        }
        
        let value = String(comp[1])
        if !key.isValid(value: value) {
            return false
        }
    }
    
    if requiredKeys.count == PassportKeys.requiredCount() {
        return true
    }
    
    return false
}

func run41_Example() {
    let example = """
    ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
    byr:1937 iyr:2017 cid:147 hgt:183cm

    iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
    hcl:#cfa07d byr:1929

    hcl:#ae17e1 iyr:2013
    eyr:2024
    ecl:brn pid:760753108 byr:1931
    hgt:179cm

    hcl:#cfa07d eyr:2025 pid:166559648
    iyr:2011 ecl:brn hgt:59in
    """
    
    print(day41(input: example))
}

func input41() -> String {
    let path = "/Users/paladetimotei/Documents/Developer/Becoming Fantastic/AdventOfCode2020/AdventOfCode2020/input/input4_1.txt"
    return try! String(contentsOfFile: path)
}

func run41() {
    let input = input41()
    print(day41(input: input))
}

func run42_allBad_Example() {
    let example = """
    eyr:1972 cid:100
    hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

    iyr:2019
    hcl:#602927 eyr:1967 hgt:170cm
    ecl:grn pid:012533040 byr:1946

    hcl:dab227 iyr:2012
    ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

    hgt:59cm ecl:zzz
    eyr:2038 hcl:74454a iyr:2023
    pid:3556412378 byr:2007
    """
    
    print(day41(input: example))
}

func run42_allGood_Example() {
    let example = """
    pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
    hcl:#623a2f

    eyr:2029 ecl:blu cid:129 byr:1989
    iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

    hcl:#888785
    hgt:164cm byr:2001 iyr:2015 cid:88
    pid:545766238 ecl:hzl
    eyr:2022

    iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719
    """
    
    print(day41(input: example))
}

func run42() {
    let input = input41()
    print(day41(input: input))
}

struct Height {
    enum Scale: String, CaseIterable {
        case `in`
        case cm
    }
    let value: String
    let scale: Scale
    
    static func from(_ value: String) -> Height? {
        var components = value.splitAt(word: "in")
        if components.count == 2 {
            let value = components.first!
            return Height(value: value, scale: .in)
        }
        
        components = value.splitAt(word: "cm")
        if components.count == 2 {
            let value = components.first!
            return Height(value: value, scale: .cm)
        }
        
        return nil
    }
}

struct PassportKeys {
    static func requiredCount() -> Int {
        return PassportKey.allCases.map({ $0.isRequired() }).filter({ $0 == true }).count
    }
}

enum PassportKey: String, CaseIterable {
    case byr
    case iyr
    case eyr
    case hgt
    case hcl
    case ecl
    case pid
    case cid
    
    func isRequired() -> Bool {
        switch self {
        case .byr: return true
        case .ecl: return true
        case .eyr: return true
        case .hcl: return true
        case .hgt: return true
        case .iyr: return true
        case .pid: return true
        case .cid: return false
        }
    }
    /*
     byr (Birth Year) - four digits; at least 1920 and at most 2002.
     iyr (Issue Year) - four digits; at least 2010 and at most 2020.
     eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
     hgt (Height) - a number followed by either cm or in:
     If cm, the number must be at least 150 and at most 193.
     If in, the number must be at least 59 and at most 76.
     hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
     ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
     pid (Passport ID) - a nine-digit number, including leading zeroes.
     cid (Country ID) - ignored, missing or not.
     */
    
    func isValid(value: String) -> Bool {
        switch self {
        case .byr:
            return checkAsInt(value, digitCount: 4, min: 1920, max: 2002)
        case .iyr:
            return checkAsInt(value, digitCount: 4, min: 2010, max: 2020)
        case .eyr:
            return checkAsInt(value, digitCount: 4, min: 2020, max: 2030)
        case .hgt:
            guard let h = Height.from(value) else { return false }
            switch h.scale {
            case .in:
                return checkAsInt(h.value, min: 59, max: 76)
            case .cm:
                return checkAsInt(h.value, min: 150, max: 193)
            }
        case .hcl:
            return isHexValue(value)
        case .ecl:
            let possibilities = Set(["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])
            return possibilities.contains(value)
        case .pid:
            guard value.count == 9 else { return false }
            return value.allSatisfy { $0 >= "0" && $0 <= "9" }
        case .cid: return true
        }
    }
    
    private func checkAsInt(_ value: String, digitCount: Int, min: Int, max: Int) -> Bool {
        guard value.count == digitCount else { return false }
        return checkAsInt(value, min: min, max: max)
    }
    
    private func checkAsInt(_ value: String, min: Int, max: Int) -> Bool {
        guard let intValue = Int(value) else { return false }
        return intValue >= min && intValue <= max
    }
    
    func isHexValue(_ value: String) -> Bool {
        guard value.count == 7 else { return false }
        let comp = value.split(separator: "#")
        guard comp.count == 1 else { return false }
        return comp.first!.allSatisfy { ($0 >= "0" && $0 <= "9") || ($0 >= "a" && $0 <= "f") }
    }
}

extension String {
    func splitAt(word: String) -> [String] {
        return self.components(separatedBy: word)
    }
}
