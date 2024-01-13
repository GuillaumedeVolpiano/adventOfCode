module Day9
  ( part1
  , part2
  ) where

import           Intcode (initialise, outputIntcode, sendInput)

part1 :: Bool -> String -> String
part1 _ = show . head . outputIntcode . sendInput 1 . initialise

part2 :: Bool -> String -> String
part2 _ = show . head . outputIntcode . sendInput 2 . initialise
