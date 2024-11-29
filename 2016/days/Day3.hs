module Day3
  ( part1
  , part2
  ) where

import           Data.List       (transpose)
import           Data.List.Split (chunksOf)
import           Helpers.Parsers (numbers)

isTriangle :: [Int] -> Bool
isTriangle [a, b, c] = a + b > c && a + c > b && b + c > a

part1 :: Bool -> String -> String
part1 _ = show . length . filter isTriangle . numbers

part2 :: Bool -> String -> String
part2 _ =
  show . length . filter isTriangle . chunksOf 3 . concat . transpose . numbers
