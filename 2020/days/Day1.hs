{-# LANGUAGE TupleSections #-}

module Day1
  ( part1
  , part2
  ) where

import           Data.IntSet     as St (filter, fromList, member, toList)
import           Data.List       as L (filter, map, tails)
import           Helpers.Parsers (integers)

findSum :: [Int] -> Int
findSum z =
  head .
  L.map (uncurry (*)) .
  L.filter (\(a, b) -> a + b == 2020) .
  concat . zipWith (\a b -> map (a, ) b) (init z) . tail . tails $
  z

findThreeSum :: [Int] -> Int
findThreeSum z =
  head .
  L.map (\(a, b) -> a * b * (2020 - a - b)) .
  L.filter (\(a, b) -> (2020 - a - b) `member` set) $
  pairs
  where
    pairs = concat . zipWith (\a b -> map (a, ) b) (init z) . tail . tails $ z
    set = fromList z

part1 :: Bool -> String -> String
part1 _ = show . findSum . concat . integers

part2 :: Bool -> String -> String
part2 _ = show . findThreeSum . concat . integers
