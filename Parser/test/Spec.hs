import Data.Functor
import Control.Monad
import Control.Applicative hiding (many)
import Data.Maybe

import Lib

test :: Eq a => String -> Parser a -> String -> Maybe a -> IO ()
test name p input expected = do
  let got = parse p input
  putStrLn (show (got == expected) ++ " > " ++ name)

test1 :: Eq a => Show a => String -> Maybe a -> Maybe a -> IO ()
test1 name result expected = do
  putStrLn (show (result == expected) ++ " > " ++ name)
  print result

main :: IO ()
main = do
  -- pfail
  test "pfail with empty string" (failure :: Parser Int) "" Nothing
  test "pfail with non-empty string" (failure :: Parser Int) "foo" Nothing
  putStrLn ""

  -- split
  test1 "split with empty string" (split "" 'a' []) Nothing
  test1 "split with non-empty string" (split "Hello World" ' ' []) (Just("Hello", "World"))
  putStrLn ""

  -- matchPrefix
  test1 "matchPrefix with empty string and empty prefix" (matchPrefix "" "") (Just "")
  test1 "matchPrefix with empty string and non-empty prefix" (matchPrefix "" "Hello") Nothing
  test1 "matchPrefix with non-empty string and empty prefix" (matchPrefix "Hello" "") (Just "Hello")
  test1 "matchPrefix with the same strings" (matchPrefix "Hello" "Hello") (Just "")
  test1 "matchPrefix where the string is missing the last character" (matchPrefix "Hell" "Hello") Nothing
  test1 "matchPrefix where the string is longer than the prefix" (matchPrefix "Hello You" "Hello") (Just " You")
  test1 "matchPrefix where the prefix is 1 character" (matchPrefix "_You" "_") (Just "You")
  test1 "matchPrefix where the prefix is 2 characters" (matchPrefix "_You" "_Y") (Just "ou")
  putStrLn ""

-- split'
  test1 "split' with empty string and empty separator" (split' "" "" []) Nothing
  test1 "split' with empty string and non-empty separator" (split' "" "Hello" []) Nothing
  test1 "split' with non-empty string and empty separator" (split' "Hello" "" []) (Just ("", "Hello"))
  test1 "split' with non-empty string and non-empty separator" (split' "Hello\n\nWorld!" "\n\n" []) (Just ("Hello", "World!"))
  test1 "split' with non-empty string and 1 character separator" (split' "Hello World!" "_" []) (Just ("Hello", "World!"))
  putStrLn ""

  -- separatedBy
  test1 "separatedBy with empty string and empty separator" (separatedBy "" "" []) (Just [])
  test1 "separatedBy: not able to separate with character separator" (separatedBy "Hello" "_" []) (Just ["Hello"])
  test1 "separatedBy: separate in 2 with character separator" (separatedBy "Hello World" "_" []) (Just ["Hello", "World"])
  test1 "separatedBy: separate in 3 with character separator" (separatedBy "Hello Worldy Character" "_" []) (Just ["Hello", "Worldy", "Character"])
  test1 "separatedBy: not able to separate with string separator" (separatedBy "Hello" "\n\n" []) (Just ["Hello"])
  test1 "separatedBy: separate in 2 with string separator" (separatedBy "Hello\n\nWorld" "\n\n" []) (Just ["Hello", "World"])
  test1 "separatedBy: separate in 3 with string separator" (separatedBy "Hello\n\nWorldy\n\nCharacter" "\n\n" []) (Just ["Hello", "Worldy", "Character"])
  putStrLn ""
