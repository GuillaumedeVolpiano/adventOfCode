module Day6
  ( part1
  , part2
  ) where

import           Data.List.Split (splitWhen)
import           Data.Set        (Set, fromList, intersection, size, unions)

import           Helpers.Parsers

part1 :: Bool -> String -> String
part1 _ =
  show . sum . map (size . unions . map fromList) . splitWhen null . lines

part2 :: Bool -> String -> String
part2 _ =
  show .
  sum .
  map (size . foldl intersection (fromList ['a' .. 'z']) . map fromList) .
  splitWhen null . lines
