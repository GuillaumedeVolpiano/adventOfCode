module Day11
  ( part1
  , part2
  ) where

import           Data.List (maximumBy)
import           Data.Map  (Map, empty, insert, member, (!))

bestFuel :: Int -> (Int, Int)
bestFuel sn =
  maximumBy
    (\a b -> compare (power sn a) (power sn b))
    [(x, y) | x <- [1 .. 298], y <- [1 .. 298]]

bestAllFuel :: Int -> (Int, Int, Int)
bestAllFuel sn =
  (\(a, b, c) -> (a, b, c + 1)) .
  maximumBy (\a b -> compare (powerSize partSums a) (powerSize partSums b)) $
  [ (x, y, z)
  | x <- [1 .. 300]
  , y <- [1 .. 300]
  , z <- [0 .. 299]
  , x + z <= 300 && y + z <= 300
  ]
  where
    partSums = recurBuildPartialSum sn (300, 300) empty

recurBuildPartialSum ::
     Int -> (Int, Int) -> Map (Int, Int) Int -> Map (Int, Int) Int
recurBuildPartialSum sn pos@(x, y) partSum
  | member pos partSum = partSum
  | x == 1 && y == 1 = insert pos (fuel sn pos) partSum
  | x == 1 = insert pos (fuel sn pos + preSum) partSumUp
  | y == 1 = insert pos (fuel sn pos + preSum) partSumLeft
  | otherwise = insert pos (fuel sn pos + preSum) partSumBoth
  where
    preSum
      | x == 1 = partSumUp ! (x, y - 1)
      | y == 1 = partSumLeft ! (x - 1, y)
      | otherwise =
        partSumBoth ! (x, y - 1) + partSumBoth ! (x - 1, y) -
        partSumBoth ! (x - 1, y - 1)
    partSumUp = recurBuildPartialSum sn (x, y - 1) partSum
    partSumLeft = recurBuildPartialSum sn (x - 1, y) partSum
    partSumBoth =
      recurBuildPartialSum sn (x, y - 1) .
      recurBuildPartialSum sn (x - 1, y) .
      recurBuildPartialSum sn (x - 1, y - 1) $
      partSum

powerSize :: Map (Int, Int) Int -> (Int, Int, Int) -> Int
powerSize partSums (x, y, z) = iA - iB - iC + iD
  where
    iA
      | x == 1 || y == 1 = 0
      | otherwise = partSums ! (x - 1, y - 1)
    iB
      | y == 1 = 0
      | otherwise = partSums ! (x + z, y - 1)
    iC
      | x == 1 = 0
      | otherwise = partSums ! (x - 1, y + z)
    iD = partSums ! (x + z, y + z)

power :: Int -> (Int, Int) -> Int
power sn (x0, y0) =
  sum . map (fuel sn) $ [(x, y) | x <- [x0 .. x0 + 2], y <- [y0 .. y0 + 2]]

fuel :: Int -> (Int, Int) -> Int
fuel sn (x, y) = mod (div ((y * (x + 10) + sn) * (x + 10)) 100) 10 - 5

part1 :: Bool -> String -> String
part1 _ = show . bestFuel . read . filter (/= '\n')

part2 :: Bool -> String -> String
part2 _ = show . bestAllFuel . read . filter (/= '\n')
