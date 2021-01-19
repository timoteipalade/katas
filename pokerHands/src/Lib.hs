module Lib(Hand, Card(..), Rank, Suite(..), Category(..), Score(..), CategoryId, pokerHandScore, eval) where
import Control.Applicative

type Hand = [Card]

data Card = Card {rank :: Rank, suite :: Suite}

type Rank = Int -- between 1 and and 14 (How can I specify this restriction?)

data Suite = Clubs | Diamonds | Hearts | Spades deriving Eq

-- Note: To be able to implement the alternative you need to implement Functor and Applicative 
-- Functor, Applicative, Monad, and Alternative need a metatype (a type that applies to another type.)
newtype Category a = Category (Hand -> Maybe a)

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

-- Score 

data Score = Score CategoryId Rank Rank Rank Rank Rank deriving Show

instance Eq Score where
   -- == :: Score -> Score -> Bool
   (Score lcid lr1 lr2 lr3 lr4 lr5) == (Score rcid rr1 rr2 rr3 rr4 rr5) = recursiveCompare [lcid, lr1, lr2, lr3, lr4, lr5] [rcid, rr1, rr2, rr3, rr4, rr5] == EQ

instance Ord Score where
   -- compare :: Score -> Score -> Ordering
   compare (Score lcid lr1 lr2 lr3 lr4 lr5) (Score rcid rr1 rr2 rr3 rr4 rr5) = recursiveCompare [lcid, lr1, lr2, lr3, lr4, lr5] [rcid, rr1, rr2, rr3, rr4, rr5]

-- helper
recursiveCompare :: [Int] -> [Int] -> Ordering
recursiveCompare (h1:[]) (h2:[])
   | h1 > h2 = GT
   | h1 < h2 = LT
   | otherwise = EQ 
recursiveCompare (h1: t1) (h2: t2) 
   | h1 > h2 = GT
   | h1 < h2 = LT
   | otherwise = recursiveCompare t1 t2


pokerHandScore :: Category Score
pokerHandScore = do straightFlush <|> fourOfAKind <|> fullHouse <|> flush <|> straight <|> threeOfAKind <|> twoPairs <|> onePair <|> highCard

-- Category Scores

-- Thre are 9 categories of hands in standard poker listed here from weakest to strongest:
-- 1. Highest Card
-- 2. One Pair
-- 3. Two Pair 
-- 4. Three of a kind
-- 5. Straight 
-- 6. Flush
-- 7. Full House
-- 8. Four of a kind
-- 9. Straight flush

type CategoryId = Int -- number between 1 and 9

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

-- Utils

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

-- returns the count if the length of the hand matches it
count :: Int -> Hand -> Maybe Int
count i hand = if length hand == i then return i else Nothing

-- evaluates a category
eval :: Category a -> Hand -> Maybe a
eval (Category a) hand = a hand
