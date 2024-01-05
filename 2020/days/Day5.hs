module Day5
  ( part1
  , part2
  ) where

import           Data.List       (sort)
import           Helpers.Parsers

fromBin :: Int -> Char -> Int
fromBin result digit = curVal + 2 * result
  where
    curVal
      | digit == 'F' || digit == 'L' = 0
      | digit == 'B' || digit == 'R' = 1

allSeats :: String -> [Int]
allSeats = map (foldl fromBin 0) . lines

findAvailable :: [Int] -> Int
findAvailable seats =
  (\[(a, b)] -> a + 1) . filter (\(a, b) -> b - a == 2) . zip (init seats) $
  tail seats

part1 :: Bool -> String -> String
part1 _ = show . maximum . allSeats

part2 :: Bool -> String -> String
part2 _ = show . findAvailable . sort . allSeats
