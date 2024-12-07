module Day6
  ( part1
  , part2
  ) where

import           Data.List (group, maximumBy, minimumBy, sort, transpose)
import           Data.Ord  (comparing)
import           Data.Text (Text, unpack)

part1 :: Bool -> Text -> String
part1 _ =
  map (head . maximumBy (comparing length) . group . sort) . transpose . lines . unpack

part2 :: Bool -> Text -> String
part2 _ =
  map (head . minimumBy (comparing length) . group . sort) . transpose . lines . unpack
