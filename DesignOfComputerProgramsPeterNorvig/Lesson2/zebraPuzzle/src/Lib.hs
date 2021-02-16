module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Data Types
data Nationality = Norwegian | Japanese | English | Spanish | Ukranian
data Animal = Dog | Snail | Fox | Horse | Zebra
data Drink = Tea | Coffee | Milk | Juice | Water
data Cigarette = Gold | Chester | Kools | Lucky | Parliments
data Color = Yellow | Green | Ivory | Red | Blue

-- Solution Type 
type Solution = ([Nationality], [Animal], [Drink], [Cigarette], [Color])

-- Constraint Type

