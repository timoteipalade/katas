module Lib (passport, passports, isValid, hasRequiredFields, areFieldsValid, result) where

import Parsing
import qualified Data.Set as Set

type Passport = [(String, String)]

-- Passport Parsing

-- passport starts with an optional new line followed by terms separated by space or new line
passport :: Parser Passport
passport = do string "\n" <|> string ""
              some (termFollowedBy ' ' <|> termFollowedBy '\n' <|> term)

passports :: Parser [Passport]
passports = some passport

--  byr (Birth Year)
--  iyr (Issue Year)
--  eyr (Expiration Year)
--  hgt (Height)
--  hcl (Hair Color)
--  ecl (Eye Color)
--  pid (Passport ID)
--  cid (Country ID)

key :: Parser String
key = do string "byr" 
      <|> string "iyr" 
      <|> string "eyr" 
      <|> string "hgt" 
      <|> string "hcl" 
      <|> string "ecl" 
      <|> string "pid" 
      <|> string "cid"

value :: Parser String
value = some (alphanum <|> char '#')

term :: Parser (String, String)
term = do k <- key
          char ':'
          v <- value
          return (k,v)

termFollowedBy :: Char -> Parser (String, String)
termFollowedBy sep = do t <- term 
                        char sep
                        return t

-- Passport Validation

isValid :: Passport -> Bool
isValid pass = hasRequiredFields pass && areFieldsValid pass

hasRequiredFields :: Passport -> Bool
hasRequiredFields pass = (foldr foldFunc 0 pass) == Set.size requiredFields

areFieldsValid :: Passport -> Bool
areFieldsValid pass = and (map isFieldValid pass)

requiredFields :: Set.Set [Char]
requiredFields = Set.fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

foldFunc :: (String, String) -> Int -> Int 
foldFunc (first, _) z = if Set.member first requiredFields
                        then z + 1
                        else z

-- Field Validation

-- byr (Birth Year) - four digits; at least 1920 and at most 2002.
-- iyr (Issue Year) - four digits; at least 2010 and at most 2020.
-- eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
-- hgt (Height) - a number followed by either cm or in:
--    If cm, the number must be at least 150 and at most 193.
--    If in, the number must be at least 59 and at most 76.
-- hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
-- ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
-- pid (Passport ID) - a nine-digit number, including leading zeroes.
-- cid (Country ID) - ignored, missing or not.

isFieldValid :: (String, String) -> Bool 
isFieldValid ("byr", value) = successful (result (parse byr value))
isFieldValid ("iyr", value) = successful (result (parse iyr value))
isFieldValid ("eyr", value) = successful (result (parse eyr value))
isFieldValid ("hgt", value) = successful (result (parse hgt value))
isFieldValid ("hcl", value) = successful (result (parse hcl value))
isFieldValid ("ecl", value) = successful (result (parse ecl value))
isFieldValid ("pid", value) = successful (result (parse pid value))
isFieldValid ("cid", value) = True
isFieldValid _ = False

count :: Int -> Parser String
count len = P (\inp -> if length inp == len 
                        then [("", inp)]
                        else [])

byr :: Parser Int
byr = do count 4
         x <- int
         if x >= 1920 && x <= 2002 then return x else empty

iyr :: Parser Int
iyr = do count 4
         x <- int
         if x >= 2010 && x <= 2020 then return x else empty 

eyr :: Parser Int
eyr = do count 4
         x <- int
         if x >= 2020 && x <= 2030 then return x else empty 

data Height = Cm Int | In Int

hgt :: Parser Height 
hgt = do x <- nat
         y <- string "cm" <|> string "in"
         if y == "cm" 
               then if x >= 150 && x <= 193 then return (Cm x) else empty
               else if x >= 59 && x <= 76 then return (In x) else empty 

hcl :: Parser String
hcl = do char '#' 
         count 6
         some (digit <|> char 'a' <|> char 'b' <|> char 'c' <|> char 'd' <|> char 'e' <|> char 'f')

ecl :: Parser String
ecl = string "amb" 
      <|> string "blu" 
      <|> string "brn" 
      <|> string "gry" 
      <|> string "grn" 
      <|> string "hzl" 
      <|> string "oth"

pid :: Parser String
pid = do count 9
         some digit

-- Utils

result :: [(a,String)] -> Maybe a
result [] = Nothing 
result [(r, str)] = Just r
result _ = Nothing

successful :: Maybe a -> Bool
successful Nothing = False
successful (Just _) = True