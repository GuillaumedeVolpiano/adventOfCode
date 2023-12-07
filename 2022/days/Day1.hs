module Day1
  ( part1, part2 ) where

import           Data.List.Split    (splitOn)

import           Data.List          (sortBy)

calories :: String -> [Int]
calories =
    map (sum . map read . filter (not . null) . splitOn "\n") . splitOn "\n\n"

part1 :: Bool -> String -> String
part1 _ = show . maximum . calories

part2 :: Bool -> String -> String
part2 _ = show . sum . take 3 . sortBy (flip compare) . calories
