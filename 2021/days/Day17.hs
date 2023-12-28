module Day17
  ( part1
  , part2
  ) where

import           Helpers.Parsers (integers)

xDist :: Int -> Int -> Int
xDist x k
  | k == 0 || x == 0 = 0
  | k >= x = div (x * (x + 1)) 2
  | otherwise = x + xDist (x - 1) (k - 1)

yDist :: Int -> Int -> Int
yDist y k
  | k == 0 = 0
  | otherwise = y + yDist (y - 1) (k - 1)

findPot :: [Int] -> [(Int, Int)]
findPot [minX, maxX, minY, maxY] =
  [ (x, y)
  | x <- [1 .. maxX]
  , y <- [minY .. yMax]
  , any
      (\k ->
         xDist x k >= minX &&
         xDist x k <= maxX && yDist y k >= minY && yDist y k <= maxY)
      [1 .. 2 * yMax + 2]
  ]
  where
    yMax = (-1) - minY

part1 :: Bool -> String -> String
part1 _ =
  show .
  (\x -> div (x * (x + 1)) 2) . ((-1) -) . last . init . concat . integers

part2 :: Bool -> String -> String
part2 _ = show . length . findPot . concat . integers
