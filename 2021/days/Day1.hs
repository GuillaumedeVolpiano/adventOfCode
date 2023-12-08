module Day1
  ( part1
  , part2
  ) where

import           Text.Regex.TDFA (getAllTextMatches, (=~))

score :: [Int] -> Int
score list = length . filter (uncurry (<)) . zip (init list) $ tail list

part1 :: Bool -> String -> String
part1 _ input = show . score $ list
  where
    list = map read . lines $ input

part2 :: Bool -> String -> String
part2 _ input = show . score . map sum . toThree $ list
  where
    list = map read . lines $ input
    toThree [x, y] = []
    toThree (x:xs) = (x : take 2 xs) : toThree xs
