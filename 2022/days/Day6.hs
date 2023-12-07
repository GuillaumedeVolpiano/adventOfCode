module Day6 (part1, part2) where
import           Data.List.Split    (splitOn)

import           Data.List          (nub)

day = 6

shorten :: Int -> String -> String
shorten size string@(x:xs)
  | length full == length nubbed = drop size string
  | otherwise = shorten size xs
  where
    full = take size string
    nubbed = nub full

part1 :: Bool -> String -> String
part1 _ input = show $ length sequence - (length . shorten 4 $ sequence)
  where
    sequence = init input

part2 :: Bool -> String -> String
part2 _ input = show $ length sequence - (length . shorten 14 $ sequence)
  where
    sequence = init input
