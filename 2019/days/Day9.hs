module Day9
  ( part1
  , part2
  ) where

import           Intcode (execIntcode, initialise, sendInput)

part1 :: Bool -> String -> String
part1 _ = show . head . execIntcode . sendInput 1 . initialise

part2 :: Bool -> String -> String
part2 _ = show . head . execIntcode . sendInput 2 . initialise
