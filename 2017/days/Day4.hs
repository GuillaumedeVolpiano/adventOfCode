module Day4
  ( part1
  , part2
  ) where

import           Data.List   (sort)
import           Data.Set    as S (fromList, size)

import           Debug.Trace

valid :: [String] -> Bool
valid l = length l == (size . fromList $ l)

part1 :: Bool -> String -> String
part1 _ = show . length . filter valid . map words . lines

part2 :: Bool -> String -> String
part2 _ = show . length . filter valid . map (map sort . words) . lines
