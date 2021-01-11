{-# LANGUAGE Safe #-}
{-# LANGUAGE LambdaCase #-}

module Lib (Parser(..), failure, success, oneChar, parse, split, matchPrefix, split', separatedBy) where

newtype Parser a = Parser { runParser :: String  -> Maybe (a, String)}

failure :: Parser a
failure = Parser (const Nothing)

success :: a -> Parser a
success a = Parser $ \s -> Just (a, s)

oneChar :: Parser Char
oneChar = Parser $ \case
  [] -> Nothing
  (c : xs) -> Just (c, xs)

parse :: Parser a -> String -> Maybe a
parse p s =
  case runParser p s of
    Nothing -> Nothing
    Just (a, _) -> Just a

split :: String -> Char -> [Char] -> Maybe ([Char], [Char])
split [] char acc = Nothing 
split (head: tail) char acc = 
  if head == char 
    then Just(acc, tail)
  else
    split tail char (acc ++ [head])

-- If match is found, returns the string after the match
matchPrefix :: String -> String -> Maybe String
matchPrefix [] [] = Just []
matchPrefix (x1: xs1) [] = Just (x1: xs1)
matchPrefix [] (x2: xs2) = Nothing
matchPrefix (x1: xs1) (x2: xs2) = 
  if x1 == x2 then matchPrefix xs1 xs2
  else Nothing

split' :: String -> String -> String -> Maybe (String, String)
split' [] _ _ = Nothing
split' s [] _ = Just([], s)
split' (x1: xs1) (x2: xs2) acc =
  case matchPrefix (x1: xs1) (x2: xs2) of
    Nothing -> split' xs1 (x2:xs2) (acc ++ [x1])
    Just rest -> Just (acc, rest)

type Separator = String

separatedBy :: String -> Separator -> [String] -> [String]
separatedBy [] _ acc = acc
separatedBy string sep acc = 
  case split' string sep [] of
    Nothing -> separatedBy [] sep (acc ++ [string])
    Just (first, rest) -> separatedBy rest sep (acc ++ [first])
