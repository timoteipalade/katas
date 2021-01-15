module Main where
    
import Lib
import Parsing

-- Advent of Code 2020 Problem 4

solvePart1 :: String -> Int
solvePart1 input = case parse passports input of
                        [] -> 0
                        [(p, _)] -> (length . filter (== True) . map hasRequiredFields) p

solvePart2 :: String -> Int
solvePart2 input = case parse passports input of
                        [] -> 0
                        [(p, _)] -> (length . filter (==True) . map isValid) p

main :: IO ()
main = do input <- readFile "./input/input4.txt"
          print (solvePart1 input)
          print (solvePart2 input)
