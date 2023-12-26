{-# LANGUAGE TupleSections #-}

module Day24
  ( part1
  , part2
  ) where

import           Data.List       (tails)
import           Helpers.Parsers (complexParser, nums)
import           Linear.Matrix   (M44 (..))
import           Linear.V2       (V2 (..))
import           Linear.V3       (V3 (..))
import           Linear.V4       (V4 (..))
import           Linear.Vector   ((*^))

import           Debug.Trace

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
solve4System = result . reduce3 . reduce2 . reduce1

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

reduce1 :: System -> System
reduce1 (V4 e1@(V4 a _ _ _) e2@(V4 b _ _ _) e3@(V4 c _ _ _) e4@(V4 d _ _ _), V4 r1 r2 r3 r4) =
  (V4 e1 ne2 ne3 ne4, V4 r1 nr2 nr3 nr4)
  where
    ne2 = a *^ e2 - b *^ e1
    nr2 = a * r2 - b * r1
    ne3 = a *^ e3 - c *^ e1
    nr3 = a * r3 - c * r1
    ne4 = a *^ e4 - d *^ e1
    nr4 = a * r4 - d * r1

reduce2 :: System -> System
reduce2 (V4 e1 e2@(V4 _ b _ _) e3@(V4 _ c _ _) e4@(V4 _ d _ _), V4 r1 r2 r3 r4) =
  (V4 e1 e2 ne3 ne4, V4 r1 r2 nr3 nr4)
  where
    ne3 = b *^ e3 - c *^ e2
    nr3 = b * r3 - c * r2
    ne4 = b *^ e4 - d *^ e2
    nr4 = b * r4 - d * r2

reduce3 :: System -> System
reduce3 (V4 e1 e2 e3@(V4 _ _ c _) e4@(V4 _ _ d _), V4 r1 r2 r3 r4) =
  (V4 e1 e2 e3 ne4, V4 r1 r2 r3 nr4)
  where
    ne4 = c *^ e4 - d *^ e3
    nr4 = c * r4 - d * r3

result :: System -> V4 Integer
result (V4 e1@(V4 a b c d) e2@(V4 0 e f g) e3@(V4 0 0 h i) e4@(V4 0 0 0 j), V4 r1 r2 r3 r4) =
  V4 w x y z
  where
    z = div r4 j
    y = div (r3 - i * z) h
    x = div (r2 - f * y - g * z) e
    w = div (r1 - b * x - c * y - d * z) a

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
