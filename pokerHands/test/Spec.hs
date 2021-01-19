import Lib

test :: Eq a => Show a => String -> a -> a -> IO ()
test name result expected = do
  putStrLn (show (result == expected) ++ " > " ++ name)

straightFlush :: Hand 
straightFlush = [Card 11 Spades, Card 10 Spades, Card 9 Spades, Card 8 Spades, Card 7 Spades]

fourOfAKind :: Hand
fourOfAKind = [Card 12 Spades, Card 12 Hearts, Card 12 Diamonds, Card 12 Clubs, Card 10 Spades]

fullHouse :: Hand
fullHouse = [Card 11 Spades, Card 11 Hearts, Card 11 Diamonds, Card 10 Clubs, Card 10 Spades]

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