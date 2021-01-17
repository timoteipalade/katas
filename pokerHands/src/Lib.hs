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
-- to compare FullRanks you need to compare the ranks from left to right
-- if one rank is bigger than the other, then one rank is bigger than the other 
-- if all ranks are the same the FullRank is the same
data FullRank = FullRank CategoryId Rank Rank Rank Rank Rank
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



--fullRank :: Hand -> Maybe FullRank
pokerHand :: Category FullRank
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

-- Categories

straightFlush :: Category FullRank
straightFlush = Category (\hand -> do sameSuite hand
                                      highestRank <- consecutive hand
                                      return (FullRank 9 highestRank 0 0 0 0))

fourOfAKind :: Category FullRank
fourOfAKind = Category (\hand -> do (rank, kicker:_) <- sameRank 4 hand
                                    return (FullRank 8 rank kicker 0 0 0))

fullHouse :: Category FullRank
fullHouse = Category (\hand -> do (rank3, _) <- sameRank 3 hand
                                  (rank2, _) <- sameRank 2 hand
                                  return (FullRank 7 rank3 rank2 0 0 0))

flush :: Category FullRank
flush = Category (\hand -> do (r5: r4: r3: r2: r1: _) <- sameSuite hand
                              return (FullRank 6 r5 r4 r3 r2 r1))

straight :: Category FullRank
straight = Category (\hand -> do highestRank <- consecutive hand
                                 return (FullRank 5 highestRank 0 0 0 0))

threeOfAKind :: Category FullRank
threeOfAKind = Category (\hand -> do (rank, r2: r1: _) <- sameRank 3 hand
                                     return (FullRank 4 rank r2 r1 0 0))

twoPairs :: Category FullRank
twoPairs = Category (\hand -> do (rank1: rank2: _, kicker: _) <- pairs 2 hand
                                 return (FullRank 3 rank1 rank2 kicker 0 0)) 

onePair :: Category FullRank
onePair = Category (\hand -> do (rank: _, r3: r2: r1: _) <- pairs 1 hand
                                return (FullRank 2 rank r3 r2 r1 0))

highCard :: Category FullRank
highCard = Category (\hand -> do highestRank <- maxRank hand
                                 return (FullRank 1 highestRank 0 0 0 0))