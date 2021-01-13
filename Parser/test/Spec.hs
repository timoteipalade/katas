import Data.Functor
import Control.Monad
import Control.Applicative hiding (many)
import Data.Maybe

import Lib
import Parsing

test1 :: Eq a => Show a => String -> a -> a -> IO ()
test1 name result expected = do
  putStrLn (show (result == expected) ++ " > " ++ name)
  --print result

main :: IO ()
main = do

  -- passport
  test1 "parse simple passport with no space" (parse passport "eyr:123") [([("eyr", "123")], "")]
  test1 "parse simple passport with space" (parse passport "eyr:123 ") [([("eyr", "123")], "")]
  test1 "parse passport with 2 terms" (parse passport "eyr:123 byr:123") [([("eyr", "123"), ("byr", "123")], "")]
  putStrLn ""