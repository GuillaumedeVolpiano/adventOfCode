module Day7
  ( part1
  , part2
  ) where

import           Data.List (permutations)
import           Intcode   (initialiseChain, runChain)

findLargestOutput :: [Int] -> String -> Int
findLargestOutput vals string =
  maximum . map (runChain 0 . initialiseChain string) . permutations $ vals

part1 :: Bool -> String -> String
part1 _ = show . findLargestOutput [0 .. 4]

part2 :: Bool -> String -> String
part2 _ = show . findLargestOutput [5 .. 9]
