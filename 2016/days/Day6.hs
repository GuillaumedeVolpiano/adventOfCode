module Day6
  ( part1
  , part2
  ) where

import           Data.List (group, maximumBy, minimumBy, sort, transpose)
import           Data.Ord  (comparing)

part1 :: Bool -> String -> String
part1 _ =
  map (head . maximumBy (comparing length) . group . sort) . transpose . lines

part2 :: Bool -> String -> String
part2 _ =
  map (head . minimumBy (comparing length) . group . sort) . transpose . lines
