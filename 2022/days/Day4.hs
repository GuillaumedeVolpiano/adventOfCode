module Day4
  ( part1
  , part2
  ) where

import           Data.List.Split    (splitOn)

day = 4

overlapFull :: ((Int, Int), (Int, Int)) -> Bool
overlapFull ((a, b), (c, d)) = (a <= c && b >= d) || (c <= a && d >= b)

overlapPart :: ((Int, Int), (Int, Int)) -> Bool
overlapPart ((a, b), (c, d)) = not (c > b || d < a)

pairs :: String -> [((Int, Int), (Int, Int))]
pairs =
  map
    ((\(x:y:_) -> (x, y)) .
     map ((\(x:y:_) -> (read x, read y)) . splitOn "-") . splitOn ",") .
  lines

part1 :: Bool -> String -> String
part1 _ = show . length . filter overlapFull . pairs

part2 :: Bool -> String -> String
part2 _ = show . length . filter overlapPart . pairs
