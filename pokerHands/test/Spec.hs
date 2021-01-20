import Lib

-- Test Util

test :: Eq a => Show a => String -> a -> a -> IO ()
test name result expected = do
  putStrLn ((if result == expected then "✅" else "🔴") ++ " > " ++ name)
  --print result

-- Hands

straightFlush :: Hand 
straightFlush = [Card 11 Spades, Card 10 Spades, Card 9 Spades, Card 8 Spades, Card 7 Spades]

fourOfAKind :: Hand
fourOfAKind = [Card 12 Spades, Card 12 Hearts, Card 12 Diamonds, Card 12 Clubs, Card 10 Spades]

fullHouse :: Hand
fullHouse = [Card 11 Spades, Card 11 Hearts, Card 11 Diamonds, Card 10 Clubs, Card 10 Spades]

flush :: Hand
flush = [Card 11 Spades, Card 9 Spades, Card 8 Spades, Card 4 Spades, Card 3 Spades]

straight :: Hand
straight = [Card 10 Spades, Card 9 Clubs, Card 8 Diamonds, Card 7 Hearts, Card 6 Clubs]

threeOfAKind :: Hand
threeOfAKind = [Card 12 Diamonds, Card 12 Clubs, Card 12 Hearts, Card 9 Clubs, Card 8 Hearts]

twoPairs :: Hand
twoPairs = [Card 11 Spades, Card 11 Diamonds, Card 12 Hearts, Card 12 Clubs, Card 2 Hearts]

onePair :: Hand 
onePair = [Card 11 Spades, Card 11 Diamonds, Card 10 Hearts, Card 7 Spades, Card 3 Hearts]

highCard :: Hand
highCard = [Card 13 Diamonds, Card 12 Diamonds, Card 7 Clubs, Card 4 Clubs, Card 3 Hearts]

-- Scores
straightFlushScore :: Maybe Score
straightFlushScore = eval score straightFlush

fourOfAKindScore :: Maybe Score
fourOfAKindScore = eval score fourOfAKind

fullHouseScore :: Maybe Score
fullHouseScore = eval score fullHouse

flushScore :: Maybe Score
flushScore = eval score flush

straightScore :: Maybe Score
straightScore = eval score straight

threeOfAKindScore :: Maybe Score
threeOfAKindScore = eval score threeOfAKind

twoPairsScore :: Maybe Score
twoPairsScore = eval score twoPairs

onePairScore :: Maybe Score
onePairScore = eval score onePair

highCardScore :: Maybe Score
highCardScore = eval score highCard

-- Lists of Hands

allHands :: [Hand]
allHands = [twoPairs, highCard, straightFlush, onePair, fullHouse, threeOfAKind, flush, fourOfAKind, straight]

-- Tests

main :: IO ()
main = do 
    -- Test the score for every category individually
    -- Test score comparison between categories 
    -- Test score comparison withing categories
    -- For the last 2 points above: If the scores are calculated correctly
    -- and the score comparison works correctly then the last 2 points will work correctly.
    -- So all I need to do is to check the score for every category
    -- And thest recursiveCompare.

    -- test simple score evaluation
    test "evaluate straigh flush score"   straightFlushScore    (Just (Score 9 11 0 0 0 0))
    test "evaluate four of a kind score"  fourOfAKindScore      (Just (Score 8 12 10 0 0 0))
    test "evaluate full house score"      fullHouseScore        (Just (Score 7 11 10 0 0 0))
    test "evaluate flush score"           flushScore            (Just (Score 6 11 9 8 4 3))
    test "evaluate straight score"        straightScore         (Just (Score 5 10 0 0 0 0))
    test "evaluate three of a kind score" threeOfAKindScore     (Just (Score 4 12 9 8 0 0))
    test "evaluate two pairs score"       twoPairsScore         (Just (Score 3 12 11 2 0 0))
    test "evaluate one pair score"        onePairScore          (Just (Score 2 11 10 7 3 0))
    test "evaluate highCard score"        highCardScore         (Just (Score 1 13 0 0 0 0))
    putStrLn ""

    -- test recursiveCompare
    test "recursive compare with empty values"                              (recursiveCompare [] []) EQ
    test "recursive compare with arrays of length 1 with the same element"  (recursiveCompare [1] [1]) EQ
    test "recursive compare with identical arrays"                          (recursiveCompare [1, 2, 3, 4] [1, 2, 3, 4]) EQ
    test "recursive compare where the left array is bigger than the right"  (recursiveCompare [1, 2, 3, 1] [1, 2, 2, 1]) GT
    test "recursive compare where the left array is smaller than the right" (recursiveCompare [1, 2, 3, 4] [1, 2, 5, 4]) LT
    putStrLn ""

    -- test comparison between categories
    test "compare straight flush to four of a kind" (straightFlushScore > fourOfAKindScore) True
    test "compare four of a kind to full house" (fourOfAKindScore > fullHouseScore) True
    putStrLn ""

    -- test comparison within categories
    test "compare straight flush to the same straight flush" (straightFlushScore == straightFlushScore) True
    test "compare four of a kind to the same four of a kind" (fourOfAKindScore == fourOfAKindScore) True
    test "compare full house to the same full house" (fullHouseScore == fullHouseScore) True
    putStrLn ""

    -- test same suite
    test "all cards have same suite in a straightFlush" (sameSuite straightFlush) (Just [11, 10, 9, 8, 7])
    test "all cards DON'T have the same suite in a fourOfAKind" (sameSuite fourOfAKind) Nothing
    test "all cards DON'T have the same suite in a fourOfAKind" (sameSuite fullHouse) Nothing
    putStrLn ""

    -- test max rank
    test "max rank of straightFlush" (maxRank straightFlush) (Just 11)
    test "max rank of fourOfAKind" (maxRank fourOfAKind) (Just 12)
    putStrLn ""

    -- test boundedIterate
    test "boundedIterate: create list of 1,2,3,4,5" (boundedIterate (+ 1) 1 5) ([1,2,3,4,5])
    putStrLn ""

    -- test consecutive
    test "straight flush is consecutive" (consecutive straightFlush) (Just 11)
    test "four of a kind is not consecutive" (consecutive fourOfAKind) Nothing
    putStrLn ""

    -- test sameRank
    test "four of a kind has 4 cards of the same same rank" (sameRank 4 fourOfAKind) (Just ([12], [10]))
    test "full house doesn't have 4 cards of the same rank" (sameRank 4 fullHouse) Nothing
    putStrLn ""

    -- test pairs 
    test "twoPairs has 2 pairs" (pairs 2 twoPairs) (Just ([12,11], [2]))
    test "full house DOESN'T have 2 pairs" (pairs 2 fullHouse) Nothing
    putStrLn ""

    test "onePair has 1 pair" (pairs 1 onePair) (Just ([11], [10, 7, 3]))
    test "twoPairs doesn't have just 1 pair" (pairs 1 twoPairs) Nothing 
    test "fullHouse has 1 pair" (pairs 1 fullHouse) (Just ([10], [11]))
    putStrLn ""

    test "best hand" (best allHands) straightFlush
    putStrLn ""