module Day2
  ( part1
  , part2
  ) where

import           Data.List  (group, sort, tails)
import           Data.Maybe (Maybe (Just, Nothing), catMaybes, isNothing)

findDifference :: [String] -> String
findDifference list =
  catMaybes .
  head .
  filter ((== 1) . length . filter isNothing) .
  concat .
  zipWith
    (map .
     zipWith
       (\a b ->
          if a == b
            then Just a
            else Nothing))
    (init list) $
  tails list

checksum :: [[Int]] -> Int
checksum iDs = twos * threes
  where
    threes = length . filter (3 `elem`) $ iDs
    twos = length . filter (2 `elem`) $ iDs

part1 :: Bool -> String -> String
part1 _ = show . checksum . map (map length . group . sort) . lines

part2 :: Bool -> String -> String
part2 _ = findDifference . lines
