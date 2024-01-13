module Day5
  ( part1
  , part2
  ) where

import           Intcode (initialise, outputIntcode, sendInput)

test :: Int -> String -> Int
test inp = head . outputIntcode . sendInput inp . initialise

part1 :: Bool -> String -> String
part1 _ = show . test 1

part2 :: Bool -> String -> String
part2 _ = show . test 5
