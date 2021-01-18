module Lib
    ( someFunc
    ) where

import Control.Applicative

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Hand = [Card] -- 5 cards only
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

-- first Rank represents the category, the rest stand for other ranks that depend on the category
-- to compare Scores you need to compare the ranks from left to right
-- if one rank is bigger than the other, then one rank is bigger than the other 
-- if all ranks are the same the Score is the same
data Score = Score CategoryId Rank Rank Rank Rank Rank
-- TODO: Implement comparison

type CategoryId = Int -- number between 1 and 9

newtype Category a = Category (Hand -> Maybe a)

eval :: Category a -> Hand -> Maybe a
eval (Category a) hand = a hand

instance Functor Category where
   -- fmap :: (a -> b) -> Parser a -> Parser b
   fmap g p = Category (\hand -> case eval p hand of
                                    Nothing -> Nothing 
                                    Just a -> Just (g a))

instance Applicative Category where
   -- pure :: a -> Parser a
   pure v = Category (\hand -> Just v)
   -- <*> :: Parser (a -> b) -> Parser a -> Parser b
   pg <*> px = Category (\hand -> case eval pg hand of
                                    Nothing -> Nothing 
                                    Just g -> eval (fmap g px) hand)

instance Monad Category where
   -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
   p >>= f = Category (\hand -> case eval p hand of
                                    Nothing -> Nothing 
                                    Just v -> eval (f v) hand)

instance Alternative Category where
    empty = Category (\hand -> Nothing)

    p <|> q = Category (\hand -> case eval p hand of
                                    Nothing -> eval q hand
                                    Just a -> Just a)

-- Note: To be able to implement the alternative you need to implement Functor and Applicative 
-- Functor, Applicative, Monad, and Alternative need a metatype (a type that applies to another type.)

-- Notes on implementation so far. I seem to be a little confused. Category FullRank does not make sense to me. 

--fullRank :: Hand -> Maybe Score
pokerHand :: Category Score
pokerHand = do straightFlush <|> fourOfAKind <|> fullHouse <|> flush <|> straight <|> threeOfAKind <|> twoPairs <|> onePair <|> highCard

-- returns the highest rank, if cards are consecutive
consecutive :: Hand -> Maybe Rank
consecutive [] = Nothing 
consecutive hand = Just 0

-- returns the ranks ordered descendingly, if all cards have the same suite
sameSuite :: Hand -> Maybe [Rank]
sameSuite [] = Nothing
sameSuite hand = Just []

-- returns the rank of the matching cars, and an array of the ranks of the other non matching cards ordered descendingly, if you have count cards of the same rank
sameRank :: Rank -> Hand -> Maybe (Rank, [Rank])
sameRank _ [] = Nothing
sameRank count hand = Just (0, [])

-- returns the ranks of each pair ordered descendingly and the ranks of the remaining cards ordered descendingly, if you have at least one pair
pairs :: Int -> Hand -> Maybe ([Rank], [Rank])
pairs _ [] = Nothing
pairs count hand = Just ([], [])

-- returns the highest rank in a hand
maxRank :: Hand -> Maybe Rank
maxRank [] = Nothing 
maxRank hand = Just 0

count :: Int -> Hand -> Maybe Int
count i hand = if length hand == i then return i else Nothing

-- Categories

straightFlush :: Category Score
straightFlush = Category (\hand -> do count 5 hand
                                      sameSuite hand
                                      highestRank <- consecutive hand
                                      return (Score 9 highestRank 0 0 0 0))

fourOfAKind :: Category Score
fourOfAKind = Category (\hand -> do count 5 hand
                                    (rank, kicker:_) <- sameRank 4 hand
                                    return (Score 8 rank kicker 0 0 0))

fullHouse :: Category Score
fullHouse = Category (\hand -> do count 5 hand
                                  (rank3, _) <- sameRank 3 hand
                                  (rank2, _) <- sameRank 2 hand
                                  return (Score 7 rank3 rank2 0 0 0))

flush :: Category Score
flush = Category (\hand -> do count 5 hand
                              (r5: r4: r3: r2: r1: _) <- sameSuite hand
                              return (Score 6 r5 r4 r3 r2 r1))

straight :: Category Score
straight = Category (\hand -> do count 5 hand
                                 highestRank <- consecutive hand
                                 return (Score 5 highestRank 0 0 0 0))

threeOfAKind :: Category Score
threeOfAKind = Category (\hand -> do count 5 hand
                                     (rank, r2: r1: _) <- sameRank 3 hand
                                     return (Score 4 rank r2 r1 0 0))

twoPairs :: Category Score
twoPairs = Category (\hand -> do count 5 hand
                                 (rank1: rank2: _, kicker: _) <- pairs 2 hand
                                 return (Score 3 rank1 rank2 kicker 0 0)) 

onePair :: Category Score
onePair = Category (\hand -> do count 5 hand
                                (rank: _, r3: r2: r1: _) <- pairs 1 hand
                                return (Score 2 rank r3 r2 r1 0))

highCard :: Category Score
highCard = Category (\hand -> do count 5 hand
                                 highestRank <- maxRank hand
                                 return (Score 1 highestRank 0 0 0 0))