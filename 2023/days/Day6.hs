module Day6
  ( part1
  , part2
  ) where

import           Debug.Trace
import           Parsers     (doubles)

toPair :: [[Double]] -> [(Double, Double)]
toPair (a:b:_) = zip a b

-- The problem boils down to x(t - x) > r, that is x² - tx + r < 0,
-- that is any integer between the roots of x^2 - tx + r
quadraticSolution :: (Double, Double) -> Int
quadraticSolution (t, r) = maxX - minX + 1
  where
    minX = ceiling ((t - sqrt (t ^ 2 - 4 * r)) / 2)
    maxX = floor ((t + sqrt (t ^ 2 - 4 * r)) / 2)

part1 :: Bool -> String -> String
part1 _ = show . product . map quadraticSolution . toPair . doubles

part2 :: Bool -> String -> String
part2 _ = show . quadraticSolution . head . toPair . doubles . filter (/= ' ')
