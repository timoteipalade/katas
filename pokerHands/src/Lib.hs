module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Hand = [Card]
data Card = Card Rank Suite
type Rank = Int
data Suite = Clubs | Diamonds | Hearts | Spades

winningHands :: [Hand] -> [Hand]
winningHands inp = []