module Day4
  ( part1
  , part2
  ) where

import           Data.List.Split (splitWhen)
import           Data.Map        (fromList, (!))
import           Data.Maybe      (Maybe (Just), isJust)
import           Data.Void
import           Helpers.Parsers (alnum, custom)
import           Text.Megaparsec (Parsec, oneOf, some, (<|>))
import           Text.Read       (readMaybe)

parser =
  fromList
    [ ("byr", byr)
    , ("iyr", iyr)
    , ("eyr", eyr)
    , ("hgt", hgt)
    , ("hcl", hcl)
    , ("ecl", ecl)
    , ("pid", pid)
    , ("cid", cid)
    ]

customised :: Parsec Void String (Maybe String)
customised =
  fmap Just . some . oneOf $ '#' : ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']

isValid :: [(String, String)] -> Bool
isValid = all validPair

validPair :: (String, String) -> Bool
validPair (i, v) = (parser ! i) v

byr :: String -> Bool
byr s = isJust rms && rs >= 1920 && rs <= 2002
  where
    rms = readMaybe s
    (Just rs) = rms

iyr :: String -> Bool
iyr s = isJust rms && rs >= 2010 && rs <= 2020
  where
    rms = readMaybe s
    (Just rs) = rms

eyr :: String -> Bool
eyr s = isJust rms && rs >= 2020 && rs <= 2030
  where
    rms = readMaybe s
    (Just rs) = rms

hgt :: String -> Bool
hgt s
  | length s == 5 = isJust rms3 && rs3 >= 150 && rs3 <= 193 && drop 3 s == "cm"
  | length s == 4 = isJust rms2 && rs2 >= 59 && rs2 <= 76 && drop 2 s == "in"
  | otherwise = False
  where
    rms3 = readMaybe . take 3 $ s
    (Just rs3) = rms3
    rms2 = readMaybe . take 2 $ s
    (Just rs2) = rms2

hcl :: String -> Bool
hcl (x:xs) =
  length xs == 6 && x == '#' && all (`elem` (['0' .. '9'] ++ ['a' .. 'f'])) xs

ecl :: String -> Bool
ecl s = s `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

pid :: String -> Bool
pid s = length s == 9 && isJust (readMaybe s :: Maybe Int)

cid :: String -> Bool
cid _ = True

hasAllFields :: [String] -> Bool
hasAllFields s =
  not . any (`notElem` s) $ ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

toPairs :: [String] -> [(String, String)]
toPairs []       = []
toPairs (a:b:xs) = (a, b) : toPairs xs

part1 :: Bool -> String -> String
part1 _ =
  show .
  length . filter hasAllFields . map concat . splitWhen null . custom customised

part2 :: Bool -> String -> String
part2 _ =
  show .
  length .
  filter isValid .
  map toPairs .
  filter hasAllFields . map concat . splitWhen null . custom customised
