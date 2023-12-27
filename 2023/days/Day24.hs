{-# LANGUAGE TupleSections #-}

module Day24
  ( part1
  , part2
  ) where

import           Data.List       (tails)
import           Data.Sequence   (Seq ((:<|), (:|>)), singleton)
import           Helpers.Maths   (solve)
import           Helpers.Parsers (complexParser, nums)
import           Linear.Matrix   (M44 (..))
import           Linear.V2       (V2 (..))
import           Linear.V3       (V3 (..))
import           Linear.V4       (V4 (..))
import           Linear.Vector   ((*^))

type Pos a = V3 a

type Vel a = V3 a

type Hail a = (Pos a, Vel a)

type System = (M44 Integer, V4 Integer)

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

toXYEquations :: [Hail Integer] -> System
toXYEquations (a:b) =
  (\[(e1, r1), (e2, r2), (e3, r3), (e4, r4)] -> (V4 e1 e2 e3 e4, V4 r1 r2 r3 r4)) .
  map (toXYEquation a) $
  b

toXYEquation :: Hail Integer -> Hail Integer -> (V4 Integer, Integer)
toXYEquation (V3 x0 y0 _, V3 vx0 vy0 _) (V3 x1 y1 _, V3 vx1 vy1 _) =
  ( V4 (vy0 - vy1) (y1 - y0) (vx1 - vx0) (x0 - x1)
  , vy0 * x0 - vx0 * y0 + vx1 * y1 - vy1 * x1)

solve4System :: System -> V4 Integer
solve4System (V4 e1 e2 e3 e4, V4 w x y z) = fromSeq . solve $ seq
  where
    toSeq (V4 a b c d) = a :<| b :<| c :<| singleton d
    seq =
      (toSeq e1 :|> w) :<| (toSeq e2 :|> x) :<| (toSeq e3 :|> y) :<|
      singleton (toSeq e4 :|> z)
    fromSeq (a :<| b :<| c :<| d :<| _) = V4 a b c d

posVel :: [Hail Integer] -> Hail Integer
posVel hails = (V3 x y z, V3 dx dy dz)
  where
    (V4 x dx y dy) = solve4System . toXYEquations $ hails
    (V2 z dz) = findZdZ x dx . take 2 $ hails

findZdZ :: Integer -> Integer -> [Hail Integer] -> V2 Integer
findZdZ x dx [(V3 x0 _ z0, V3 vx0 _ vz0), (V3 x1 _ z1, V3 vx1 _ vz1)] = V2 z dz
  where
    t0 = div (x0 - x) (dx - vx0)
    t1 = div (x1 - x) (dx - vx1)
    r0 = z0 + vz0 * t0
    r1 = z1 + vz1 * t1
    dz = div (r1 - r0) (t1 - t0)
    z = r0 - dz * t0

sumCoords :: Hail Integer -> Integer
sumCoords (V3 x y z, _) = x + y + z

part1 :: Bool -> String -> String
part1 test =
  show .
  count2DIntersections test .
  map (makeHail . map (map read)) . complexParser ["@"] [nums, nums]

part2 :: Bool -> String -> String
part2 _ =
  show .
  sumCoords .
  posVel .
  take 5 . map (makeHail . map (map read)) . complexParser ["@"] [nums, nums]
