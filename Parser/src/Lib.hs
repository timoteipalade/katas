module Lib (key, value, term, termFollowedBy, passport, passports, isValid, unpack) where

import Parsing
import qualified Data.Set as Set

--  byr (Birth Year)
--  iyr (Issue Year)
--  eyr (Expiration Year)
--  hgt (Height)
--  hcl (Hair Color)
--  ecl (Eye Color)
--  pid (Passport ID)
--  cid (Country ID)

key = do string "byr" 
      <|> string "iyr" 
      <|> string "eyr" 
      <|> string "hgt" 
      <|> string "hcl" 
      <|> string "ecl" 
      <|> string "pid" 
      <|> string "cid"

value = some (alphanum <|> char '#')

term = do k <- key
          char ':'
          v <- value
          return (k,v)

termFollowedBy sep = do t <- term 
                        char sep
                        return t


type Passport = [(String, String )]

-- passport starts with an optional new line followed by terms separated by space or new line
passport :: Parser Passport
passport = do string "\n" <|> string ""
              some (termFollowedBy ' ' <|> termFollowedBy '\n' <|> term)

passports = some passport

requiredFields = Set.fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

foldFunc :: (String, String) -> Int -> Int 
foldFunc (first, _) z = if Set.member first requiredFields
                        then z + 1
                        else z

isValid :: Passport -> Bool
isValid pass = (foldr foldFunc 0 pass) == Set.size requiredFields

-- this is unsafe
unpack :: [(a,String)] -> a
unpack [(result, str)] = result