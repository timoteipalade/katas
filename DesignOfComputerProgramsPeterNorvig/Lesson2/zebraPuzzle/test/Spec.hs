import Lib
import Criterion.Main

test :: Eq a => Show a => String -> a -> a -> IO ()
test name result expected = do
  putStrLn ((if result == expected then "✅" else "🔴") ++ " > " ++ name)
  --print result

benchmark = 
    defaultMain [
        bgroup "Zebra Puzzle" [bench "Zebra Puzzle benchmark" $ whnfIO (putStrLn $ show run)]
    ]

lengths = [length zeroLevelWithNationalities, length firstLevel, length firstLevelWithAnimals, length secondLevel, length secondLevelWithDrinks, length thirdLevel, length thirdLevelWithCigarettes, length fourthLevel, length fourthLevelWithColors, length fifthLevel]

lengthsEvolution = foldl (\b a -> b ++ " -> " ++ show a) "0" lengths

solution :: [Solution]
solution = [([Norwegian,Ukranian,English,Spanish,Japanese],[Fox,Horse,Snail,Dog,Zebra],[Water,Tea,Milk,Juice,Coffee],[Kools,Chester,Gold,Lucky,Parliments],[Yellow,Blue,Red,Ivory,Green])]

main :: IO ()
main = 
    do 
        test "Solution is correct" run solution
        putStrLn "Solution space evolution: "
        putStrLn lengthsEvolution
        -- bechmark
        
