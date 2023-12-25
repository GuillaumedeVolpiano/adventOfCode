{-# LANGUAGE TupleSections #-}

module Day24
  ( part1
  , part2
  ) where

import           Data.List                             (tails)
import           Data.Set                              (Set, fromList,
                                                        intersection)
import           Helpers.Parsers                       (complexParser, nums)
import           Linear.V3
import           Math.NumberTheory.ArithmeticFunctions (divisors)

type Pos a = V3 a

type Vel a = V3 a

type Hail a = (Pos a, Vel a)

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
intersect2D :: Bool -> (Hail Double, Hail Double) -> Bool
intersect2D test (h1@(V3 x0 y0 _, V3 a0 b0 _), h2@(V3 x1 y1 _, V3 a1 b1 _))
  -- parallel lines
  | a0 * b1 - a1 * b0 == 0 = False
  | k >= 0 &&
      k' >= 0 &&
      x >= minV test && y >= minV test && x <= maxV test && y <= maxV test =
    True
  | otherwise = False
  where
    k' = (a0 * (y0 - y1) + b0 * (x1 - x0)) / (a0 * b1 - a1 * b0)
    k = (x1 + k' * a1 - x0) / a0
    x = x0 + k * a0
    y = y0 + k * b0
    -- the lines intersect if there exists k and k' such that x0 + ka0 == x1 +
    -- k'a1 && y0 + kb0 == y1 + k'b1. That is k == (x1 + k'a1 - x0)/a0. Then we
    -- have y0 + (x1 + k'a1 - x0)b0/a0 == y1 + k'b1. y0 + x1b0/a0 -x0b0/a0 - y1 ==
    -- k'(a0b1 - a1b0)/a0. k' = (a0 * y0 + x1b0 - x0b0 - a0 y1)/(a0b1 - a1b0)

count2DIntersections :: Bool -> [Hail Double] -> Int
count2DIntersections test l =
  length .
  concatMap (filter (intersect2D test) . (\(a, b) -> map (a, ) b)) .
  zip (init l) . tail . tails $
  l

makeHail :: [[a]] -> Hail a
makeHail [[a, b, c], [d, e, f]] = (V3 a b c, V3 d e f)

potK :: Hail Int -> Hail Int -> Set Int
potK (V3 x y z, _) (V3 a b c, _)
  | x /= a && y /= b && z /= c = intersection dx . intersection dy $ dz
  | x == a = intersection dy dz
  | y == b = intersection dx dz
  | z == c = intersection dx dy
  where
    dx = divisors . abs $ x - a
    dy = divisors . abs $ y - b
    dz = divisors . abs $ z - c

findK :: Hail Int -> [Hail Int] -> Set Int
findK p [a]    = potK p a
findK p (a:as) = intersection (potK p a) . findK p $ as

allKs :: [Hail Int] -> [Set Int]
allKs l = zipWith findK (init l) . tail . tails $ l

similarVelocity :: [Hail Int] -> [(Hail Int, Hail Int)]
similarVelocity hails =
  concatMap (filter equalVel . (\(a, b) -> map (a, ) b)) .
  zip (init hails) . tail . tails $
  hails

equalVel :: (Eq a) => (Hail a, Hail a) -> Bool
equalVel ((_, V3 a b c), (_, V3 d e f)) = a == d || b == e || c == f

part1 :: Bool -> String -> String
part1 test =
  show .
  count2DIntersections test .
  map (makeHail . map (map read)) . complexParser ["@"] [nums, nums]

-- x mod k = x1 mod k
-- y mod k = y1 mod k
-- z mod k = z1 mod k
-- x + y + z mod k = x1 + y1 + z1
part2 :: Bool -> String -> String
part2 _ =
  show .
  similarVelocity .
  map (makeHail . map (map read)) . complexParser ["@"] [nums, nums]
