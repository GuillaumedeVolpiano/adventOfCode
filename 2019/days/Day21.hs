module Day21
  ( part1
  , part2
  ) where

import           Intcode (initialise, outputASCIIcode, outputIntcode,
                          sendASCIIInput)

walkscript = "NOT B J\nNOT C T\nOR T J\nAND D J\nNOT A T\nOR T J\nWALK\n"

runscript = "NOT B J\nNOT C T\nOR T J\nAND D J\nAND H J\nNOT A T\nOR T J\nRUN\n"

part1 :: Bool -> String -> String
part1 _ = show . head . outputIntcode . sendASCIIInput walkscript . initialise

part2 :: Bool -> String -> String
part2 _ = show . head . outputIntcode . sendASCIIInput runscript . initialise
