module Day5
  ( part1
  , part2
  ) where

import           Data.List       (minimumBy)
import           Data.List.Split (chunksOf, splitWhen)
import           Parsers         (integers)

--parseInput :: [String] -> ([Int], Int -> Int)
--parseInput i = (map read . getNumbers . head $ i, getMaps i)
parseInput :: String -> ([Int], Int -> Int)
parseInput s = (head parsed, getMaps . drop 2 $ parsed)
  where
    parsed = integers s

getMaps :: [[Int]] -> (Int -> Int)
getMaps = process . map (gardenMap . map truple) . splitWhen null

truple :: [Int] -> (Int, Int, Int)
truple [a, b, c] = (a, b, c)

gardenMap :: [(Int, Int, Int)] -> Int -> Int
gardenMap [] x = x
gardenMap ((d, s, l):rs) x
  | x >= s && x - s < l = d + x - s
  | otherwise = gardenMap rs x

ranges :: [Int] -> [(Int, Int)]
ranges []      = []
ranges (a:b:c) = (a, a + b - 1) : ranges c

continuityRanges :: (Int -> Int) -> (Int, Int) -> [(Int, Int)]
continuityRanges f (a, b)
  | f b - f a == b - a = [(a, b)]
  | otherwise = continuityRanges f (a, c) ++ continuityRanges f (c + 1, b)
  where
    c = a + div (b - a) 2

process :: [Int -> Int] -> Int -> Int
process fl x = foldl (\x f -> f x) x fl

part1 :: Bool -> String -> String
part1 _ input = show . minimum . map mapping $ seeds
  where
    (seeds, mapping) = parseInput input

part2 :: Bool -> String -> String
part2 _ input =
  show .
  minimum .
  map (\(a, _) -> mapping a) . concatMap (continuityRanges mapping) . ranges $
  seeds
  where
    (seeds, mapping) = parseInput input
