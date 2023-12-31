module Day10
  ( part1
  , part2
  ) where

import           Data.List (group, sort)
import           Data.Set  (Set, fromList, member)

countDifs :: [Int] -> Int
countDifs adapters =
  (\[a, b] -> (a + 1) * (b + 1)) .
  map length . group . sort . zipWith (flip (-)) (init sorted) $
  tail sorted
  where
    sorted = sort adapters

combinations :: Set Int -> Int -> Int
combinations list = (map comb [0 ..] !!)
  where
    comb 0 = 1
    comb 1
      | 1 `member` list = comb 0
    comb 2
      | 2 `member` list = comb 1 + comb 0
    comb n
      | n `member` list = sum . combined $ n
      | otherwise = 0
    combined n = map (combinations list . (n -)) [1 .. 3]

findCombinations :: [Int] -> Int
findCombinations list = combinations (fromList sorted) device
  where
    sorted = sort list
    device = last sorted

part1 :: Bool -> String -> String
part1 _ = show . countDifs . map read . lines

part2 :: Bool -> String -> String
part2 _ = show . findCombinations . map read . lines
