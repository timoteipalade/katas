module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Hand = [Card]
data Card = Card {rank :: Rank, suite :: Suite}
type Rank = Int
data Suite = Clubs | Diamonds | Hearts | Spades deriving Eq

winningHands :: [Hand] -> [Hand]
winningHands inp = []

-- Thre are 9 categories of hands in standard poker:
-- 1. Highest Card
-- 2. One Pair
-- 3. Two Pair 
-- 4. Three of a kind
-- 5. Straight 
-- 6. Flush
-- 7. Full House
-- 8. Four of a kind
-- 9. Straight flush

type Category = Int -- between 1 and 9

category :: Hand -> Category
category inp = 1

-- returns the highest rank, if cards are consecutive
consecutive :: Hand -> Maybe Rank
consecutive [] = Nothing 
consecutive hand = Just 0

-- returns the ranks ordered descendingly, if all cards have the same suite
sameSuite :: Hand -> Maybe [Rank]
sameSuite [] = Nothing
sameSuite hand = Just []

-- returns the rank, if you have count cards of the same rank
sameRank :: Rank -> Hand -> Maybe Rank
sameRank _ [] = Nothing
sameRank count hand = Just 0

-- returns the ranks of each pair, if you have at least one pair
pairs :: Int -> Hand -> Maybe [Rank]
pairs _ [] = Nothing
pairs count hand = Just []

-- Categories

straightFlush :: Hand -> Maybe Category
straightFlush [] = Nothing 
straightFlush hand = do sameSuite hand
                        consecutive hand
                        return 9

fourOfAKind :: Hand -> Maybe Category
fourOfAKind [] = Nothing
fourOfAKind hand = do sameRank 4 hand
                      return 8

fullHouse :: Hand -> Maybe Category
fullHouse [] = Nothing
fullHouse hand = do sameRank 2 hand
                    sameRank 3 hand
                    return 7

flush :: Hand -> Maybe Category
flush [] = Nothing
flush hand = do sameSuite hand
                return 6

straight :: Hand -> Maybe Category
straight [] = Nothing
straight hand = do consecutive hand 
                   return 5

threeOfAKind :: Hand -> Maybe Category
threeOfAKind [] = Nothing 
threeOfAKind hand = do sameRank 3 hand
                       return 4

twoPairs :: Hand -> Maybe Category 
twoPairs [] = Nothing 
twoPairs hand = do pairs 2 hand
                   return 3

onePair :: Hand -> Maybe Category
onePair [] = Nothing
onePair hand = do pairs 1 hand
                  return 2

highCard :: Hand -> Maybe Category
highCard [] = Nothing 
highCard hand = Just 1

                                
--straightFlush [] = Nothing