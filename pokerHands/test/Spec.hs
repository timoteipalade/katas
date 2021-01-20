import Lib

test :: Eq a => Show a => String -> a -> a -> IO ()
test name result expected = do
  putStrLn ((if result == expected then "✅" else "🔴") ++ " > " ++ name)
  --print result

straightFlush :: Hand 
straightFlush = [Card 11 Spades, Card 10 Spades, Card 9 Spades, Card 8 Spades, Card 7 Spades]

fourOfAKind :: Hand
fourOfAKind = [Card 12 Spades, Card 12 Hearts, Card 12 Diamonds, Card 12 Clubs, Card 10 Spades]

fullHouse :: Hand
fullHouse = [Card 11 Spades, Card 11 Hearts, Card 11 Diamonds, Card 10 Clubs, Card 10 Spades]

twoPairs :: Hand
twoPairs = [Card 11 Spades, Card 11 Diamonds, Card 12 Hearts, Card 12 Clubs, Card 2 Hearts]

onePair :: Hand 
onePair = [Card 11 Spades, Card 11 Diamonds, Card 10 Hearts, Card 7 Spades, Card 3 Hearts]

straightFlushScore = eval pokerHandScore straightFlush
fourOfAKindScore = eval pokerHandScore fourOfAKind
fullHouseScore = eval pokerHandScore fullHouse

main :: IO ()
main = do 
    -- test simple score evaluation
    test "evaluate straigh flush score" (eval pokerHandScore straightFlush) (Just (Score 9 11 0 0 0 0))

    -- test comparison between categories
    test "compare straight flush to four of a kind" (straightFlushScore > fourOfAKindScore) True
    test "compare four of a kind to full house" (fourOfAKindScore > fullHouseScore) True

    -- test comparison within categories
    test "compare straight flush to the same straight flush" (straightFlushScore == straightFlushScore) True
    test "compare four of a kind to the same four of a kind" (fourOfAKindScore == fourOfAKindScore) True
    test "compare full house to the same full house" (fullHouseScore == fullHouseScore) True

    -- test same suite
    test "all cards have same suite in a straightFlush" (sameSuite straightFlush) (Just [11, 10, 9, 8, 7])
    test "all cards DON'T have the same suite in a fourOfAKind" (sameSuite fourOfAKind) Nothing
    test "all cards DON'T have the same suite in a fourOfAKind" (sameSuite fullHouse) Nothing

    -- test max rank
    test "max rank of straightFlush" (maxRank straightFlush) (Just 11)
    test "max rank of fourOfAKind" (maxRank fourOfAKind) (Just 12)

    -- test boundedIterate
    test "boundedIterate: create list of 1,2,3,4,5" (boundedIterate (+ 1) 1 5) ([1,2,3,4,5])

    -- test consecutive
    test "straight flush is consecutive" (consecutive straightFlush) (Just 11)
    test "four of a kind is not consecutive" (consecutive fourOfAKind) Nothing

    -- test sameRank
    test "4 cards have sameRank in four of a kind" (sameRank 4 fourOfAKind) (Just ([12], [10]))
    test "4 cards DON'T have the sameRank in a full house" (sameRank 4 fullHouse) Nothing

    -- test pairs 
    test "twoPairs has 2 pairs" (pairs 2 twoPairs) (Just ([12,11], [2]))
    test "full house DOESN'T have 2 pairs" (pairs 2 fullHouse) Nothing

    test "onePair has 1 pair" (pairs 1 onePair) (Just ([11], [10, 7, 3]))
    test "twoPairs doesn't have just 1 pair" (pairs 1 twoPairs) Nothing 
    test "fullHouse has 1 pair" (pairs 1 fullHouse) (Just ([10], [11]))