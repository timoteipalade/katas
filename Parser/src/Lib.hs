module Lib (key, value, term, termFollowedBy, passport, passports) where

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

value = some (alphanum <|> char '#')

term = do k <- key
          char ':'
          v <- value
          return (k,v)

termFollowedBy sep = do t <- term 
                        char sep
                        return t

-- passport starts with an optional new line followed by terms separated by space or new line
passport = do string "\n" <|> string ""
              some (termFollowedBy ' ' <|> termFollowedBy '\n' <|> term)

passports = some passport