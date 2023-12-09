module Day9
  ( part1
  , part2
  ) where

import Parsers (integers)

toZero :: [Int] -> [[Int]]
toZero s
  | all (== 0) s = []
  | otherwise = s : (toZero . diffs $ s)

diffs :: [Int] -> [Int]
diffs (a:xs@(b:r))
  | null r = [b - a]
  | otherwise = (b - a) : diffs xs

extrapolate :: [[Int]] -> Int
extrapolate = sum . map head

extrapolateBackwards :: [[Int]] -> Int
extrapolateBackwards = foldr ((-) . head) 0

part1 :: Bool -> String -> String
part1 _ = show . sum . map (extrapolate . toZero) . integers

part2 :: Bool -> String -> String
part2 _ = show . sum . map (extrapolateBackwards . toZero) . integers
