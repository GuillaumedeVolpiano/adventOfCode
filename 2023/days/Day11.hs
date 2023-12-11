{-# LANGUAGE TupleSections #-}

module Day11
  ( part1
  , part2
  ) where

import           Data.Array.Unboxed (UArray, array, bounds, inRange, indices,
                                     (!))
import           Data.List          (tails, transpose)
import           Helpers.Parsers    (arrayFromString)
import           Helpers.Search     (bfsDist)
import           Linear.V2

type Image = UArray Pos Char

type Pos = V2 Int

up = V2 0 (-1)

down = V2 0 1

left = V2 1 0

right = V2 (-1) 0

expandImage :: [String] -> [String]
expandImage [] = []
expandImage (s:xs)
  | all (== '.') s = s : s : expandImage xs
  | otherwise = s : expandImage xs

shortestPath :: Image -> Int -> [Int] -> [Int] -> (Pos, Pos) -> Int
shortestPath diagram factor expX expY (V2 x1 y1, V2 x2 y2) =
  abs (x1 - x2) + abs (y1 - y2) + (factor - 1) * expansion
  where
    expansion = countExpandableX + countExpandableY
    countExpandableX =
      length . takeWhile (< max x1 x2) . takeWhile (> min x1 x2) $ expX
    countExpandableY =
      length . takeWhile (< max y1 y2) . takeWhile (> min y1 y2) $ expX

expandablesX :: Image -> [Int]
expandablesX image =
  filter (\x -> all (== '.') [image ! V2 x y | y <- [0 .. my]]) [0 .. mx]
  where
    (_, V2 mx my) = bounds image

expandablesY :: Image -> [Int]
expandablesY image =
  filter (\y -> all (== '.') [image ! V2 x y | x <- [0 .. mx]]) [0 .. my]
  where
    (_, V2 mx my) = bounds image

part1 :: Bool -> String -> String
part1 _ input = show . sum . map (shortestPath image 2 expX expY) $ pairs
  where
    image = arrayFromString input
    galaxies = filter (\p -> (image ! p) == '#') . indices $ image
    pairs =
      concat . zipWith (\a b -> map (a, ) b) (init galaxies) . tail $
      tails galaxies
    expX = expandablesX image
    expY = expandablesY image

part2 :: Bool -> String -> String
part2 test input =
  show . sum . map (shortestPath image factor expX expY) $ pairs
  where
    image = arrayFromString input
    galaxies = filter (\p -> (image ! p) == '#') . indices $ image
    pairs =
      concat . zipWith (\a b -> map (a, ) b) (init galaxies) . tail $
      tails galaxies
    factor
      | test = 10
      | otherwise = 10 ^ 6
    expX = expandablesX image
    expY = expandablesY image
