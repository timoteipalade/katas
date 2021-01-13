module Lib (key, value, term, termFollowedBy, passport) where

import Parsing

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

value = some alphanum 

term :: Parser (String, String)
term = do k <- key
          char ':'
          v <- value
          return (k,v)

termFollowedBy sep = do t <- term 
                        char sep
                        return t

-- passport is one or more terms, separated by spaces or new lines
-- passport inp = some term separatedBy space
passport = some (termFollowedBy ' ' <|> termFollowedBy '\n' <|> term)
