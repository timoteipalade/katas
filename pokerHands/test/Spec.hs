import Lib

test :: Eq a => Show a => String -> a -> a -> IO ()
test name result expected = do
  putStrLn (show (result == expected) ++ " > " ++ name)

straightFlush :: Hand 
straightFlush = [Card 11 Spades, Card 10 Spades, Card 9 Spades, Card 8 Spades, Card 7 Spades]

main :: IO ()
main = do 
    test "evaluate straigh flush score" (eval pokerHandScore straightFlush) (Just (Score 9 11 0 0 0 0))
