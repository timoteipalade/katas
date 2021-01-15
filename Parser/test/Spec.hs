import Lib
import Parsing

test1 :: Eq a => Show a => String -> a -> a -> IO ()
test1 name result expected = do
  putStrLn (show (result == expected) ++ " > " ++ name)
  -- print result

example = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\n\
          \byr:1937 iyr:2017 cid:147 hgt:183cm\n\n\
          \iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\n\
          \hcl:#cfa07d byr:1929"

validPass1 = "hcl:#ae17e1 iyr:2013\n\
              \eyr:2024\n\
              \ecl:brn pid:760753108 byr:1931\n\
              \hgt:179cm"

invalidPass1 = "hcl:#ae17e1 iyr:2013\n\
              \eyr:2050\n\
              \ecl:brn pid:760753108 byr:1931\n\
              \hgt:179cm"

main :: IO ()
main = do

  -- passport
  test1 "parse simple passport with no space" (parse passport "eyr:123") [([("eyr", "123")], "")]
  test1 "parse simple passport with space" (parse passport "eyr:123 ") [([("eyr", "123")], "")]
  test1 "parse passport with 2 terms separated by space" (parse passport "eyr:123 byr:123") [([("eyr", "123"), ("byr", "123")], "")]
  test1 "parse passport with 2 terms separated by new line" (parse passport "eyr:123\nbyr:123") [([("eyr", "123"), ("byr", "123")], "")]

  test1 "parse passports" (parse passports "eyr:123 byr:123\n\neyr:123 byr:123") [([[("eyr", "123"), ("byr", "123")], [("eyr", "123"), ("byr", "123")]], "")]
  test1 "parse example passports" (parse passports example) [([[("ecl","gry"),("pid","860033327"),("eyr","2020"),("hcl","#fffffd"),("byr","1937"),("iyr","2017"),("cid","147"),("hgt","183cm")],[("iyr","2013"),("ecl","amb"),("cid","350"),("eyr","2023"),("pid","028048884"),("hcl","#cfa07d"),("byr","1929")]],"")]

  test1 "parse and validate passport with all required fields" (fmap hasRequiredFields (result (parse passport validPass1))) (Just True)
  test1 "parse and validate passport with valid fields" (fmap areFieldsValid (result (parse passport validPass1))) (Just True)
  test1 "parse and validate passport with invalid fields" (fmap areFieldsValid (result (parse passport invalidPass1))) (Just False)
  putStrLn ""