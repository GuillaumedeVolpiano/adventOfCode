{-# LANGUAGE TupleSections #-}

module Day24
  ( part1
  , part2
  ) where

import           Data.List       (tails)
import           Helpers.Parsers (complexParser, nums)
import           Linear.V3

import           Debug.Trace

type Pos = V3 Double

type Vel = V3 Double

type Line2D = (Double, Double)

type Hail = (Pos, Vel)

minV t
  | t = 7
  | otherwise = 200000000000000

maxV t
  | t = 27
  | otherwise = 400000000000000

-- [[x0, y0, _], [a, b, _]] . x = ka + x0, y = kb + y0. k = (y - y0)/b.
-- x =a(y-y0)/b + x = (a/b)y + x0 -(a * y0/b)
-- line2D :: [[Double]] -> Line2D
-- line2D [[x0, y0, _], [a, b, _]] = (a / b, x0 - a * y0 / b)
-- x = ay + b && x = cy + d. ay + b = cy + d. y = (d - b)/(a-c). x = ay + b
intersect2D :: Bool -> (Hail, Hail) -> Bool
intersect2D test ((V3 x0 y0 _, V3 a0 b0 _), (V3 x1 y1 _, V3 a1 b1 _))
  -- parallel lines
  | a0 * b1 - a1 == 0 = False
  | k >= 0 &&
      k' >= 0 &&
      x >= minV test && y >= minV test && x <= maxV test && y <= maxV test =
    trace (show x ++ " " ++ show y ++ " " ++ show k ++ " " ++ show k') True
  | otherwise = False
  where
    k' = (a0 * (y0 - y1) + (x1 - x0)) / (a0 * b1 - a1)
    k = (x1 + k' * a1 - x0) / a0
    x = x0 + k * a0
    y = y0 + k * b0
    -- the lines intercept if there exists k and k' such that x0 + ka0 == x1 +
    -- k'a1 && y0 + kb0 == y1 + k'b1. That is k == (x1 + k'a1 - x0)/a0. Then we
    -- have y0 + (x1 + k'a1 - x0)/a0 == y1 + k'b1. y0 + x1/a0 -x0/a0 - y1 ==
    -- k'(a0b1 - a1)/a0. k' = (a0 * y0 + x1 - x0 - a0 y1)/(a0b1 - a1)

count2DIntersections :: Bool -> [Hail] -> Int
count2DIntersections test l =
  length .
  concatMap (filter (intersect2D test) . (\(a, b) -> map (a, ) b)) .
  zip (init l) . tail . tails $
  l

makeHail :: [[Double]] -> Hail
makeHail [[a, b, c], [d, e, f]] = (V3 a b c, V3 d e f)

part1 :: Bool -> String -> String
part1 test =
  show .
  count2DIntersections test .
  map (makeHail . map (map read)) . complexParser ["@"] [nums, nums]

part2 :: Bool -> String -> String
part2 _ _ = "Part 2"
