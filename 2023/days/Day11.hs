{-# LANGUAGE TupleSections #-}

module Day11
  ( part1
  , part2
  , expandablesX
  , expandablesY
  , shortestPath
  ) where

import           Data.Array.Unboxed         (UArray, array, bounds, inRange,
                                             indices, (!))
import           Data.ByteString            (ByteString)
import           Data.List                  (tails)
import           Data.Word                  (Word8)
import           Data.Word8                 (_numbersign, _period)
import           Helpers.Parsers.ByteString (arrayFromByteString)
import           Helpers.Search             (bfsDist)
import           Linear.V2

type Image = UArray Pos Word8

type Pos = V2 Int

up = V2 0 (-1)

down = V2 0 1

left = V2 1 0

right = V2 (-1) 0

-- The shortest path between two galaxies is the Manhattan distance between them
-- plus the product of (expansion factor - 1) and the number of expandable lines.
-- We need to remove one from expansion factor as we already counted expandable
-- lines once in the Manhattan distance.
shortestPath :: Int -> [Int] -> [Int] -> (Pos, Pos) -> Int
shortestPath factor expX expY (V2 x1 y1, V2 x2 y2) =
  abs (x1 - x2) + abs (y1 - y2) + (factor - 1) * expansion
  where
    expansion = countExpandableX + countExpandableY
    countExpandableX =
      length . takeWhile (< max x1 x2) . dropWhile (< min x1 x2) $ expX
    countExpandableY =
      length . takeWhile (< max y1 y2) . dropWhile (< min y1 y2) $ expY

-- A line is expandable if it consists only of '.' characters
expandablesX :: Image -> [Int]
expandablesX image =
  filter (\x -> all (== _period) [image ! V2 x y | y <- [0 .. my]]) [0 .. mx]
  where
    (_, V2 mx my) = bounds image

expandablesY :: Image -> [Int]
expandablesY image =
  filter (\y -> all (== _period) [image ! V2 x y | x <- [0 .. mx]]) [0 .. my]
  where
    (_, V2 mx my) = bounds image

-- Build the image. Find the coordinate of the galaxies. Build a list of unique
-- pairs. Find the expandable lines. Calcualte the result.
part1 :: Bool -> ByteString -> String
part1 _ input = show . sum . map (shortestPath 2 expX expY) $ pairs
  where
    image = arrayFromByteString input
    galaxies = filter (\p -> (image ! p) == _numbersign) . indices $ image
    pairs = [(a, b) | (a:gs) <- tails galaxies, b <- gs]
    expX = expandablesX image
    expY = expandablesY image

-- The only difference with part 1 is the factor, which depends on whether we
-- are testing or not.
part2 :: Bool -> ByteString -> String
part2 test input = show . sum . map (shortestPath factor expX expY) $ pairs
  where
    image = arrayFromByteString input
    galaxies = filter (\p -> (image ! p) == _numbersign) . indices $ image
    pairs = [(a, b) | (a:gs) <- tails galaxies, b <- gs]
    factor
      | test = 10
      | otherwise = 10 ^ 6
    expX = expandablesX image
    expY = expandablesY image
