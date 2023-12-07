module Day2 where
import           Data.List.Split    (splitOn)

import           Data.Char          (ord)

score1 :: String -> Int
score1 (a:_:b:_)
  | ord a == ord b - 23 = ord b - 84
  | a == 'A' && b == 'Z' = 3
  | a == 'C' && b == 'X' = 7
  | ord a < ord b - 23 = ord b - 81
  | otherwise = ord b - 87

score2 :: String -> Int
score2 (a:_:b:_)
  | b == 'Y' = ord a - 61
  | b == 'X' = mod (ord a - 66) 3 + 1
  | b == 'Z' = mod (ord a - 67) 3 + 7

part1 :: Bool -> String -> String
part1 _ = show . sum . map score1 . lines

part2 :: Bool -> String -> String
part2 _ = show . sum . map score2 . lines
