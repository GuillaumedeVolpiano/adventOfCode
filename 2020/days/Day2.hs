module Day2
  ( part1
  , part2
  ) where

import           Helpers.Parsers (alpha, complexParser, nums)

validate :: [String] -> Bool
validate [a, b, [c], d] = length pruned <= read b && length pruned >= read a
  where
    pruned = filter (== c) d

otherValidate :: [String] -> Bool
otherValidate [a, b, [c], d] = (da || db) && not (da && db)
  where
    da = d !! (read a - 1) == c
    db = d !! (read b - 1) == c

part1 :: Bool -> String -> String
part1 _ =
  show .
  length .
  filter validate .
  map concat . complexParser ["-", " ", ": "] [nums, nums, alpha, alpha]

part2 :: Bool -> String -> String
part2 _ =
  show .
  length .
  filter otherValidate .
  map concat . complexParser ["-", " ", ": "] [nums, nums, alpha, alpha]
