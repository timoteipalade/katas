import Lib
import Criterion.Main

benchmark = 
    defaultMain [
        bgroup "Zebra Puzzle" [bench "Zebra Puzzle benchmark" $ whnfIO (putStrLn $ show run)]
    ]

main :: IO ()
main = 
    do 
        print run
        print (length firstLevel)
        print (length secondLevel)
        print (length thirdLevel)
        print (length fourthLevel)
        print (length fifthLevel)
        -- benchmark
        
